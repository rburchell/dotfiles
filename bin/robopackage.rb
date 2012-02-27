#!/usr/bin/env ruby

# robopackager.rb
# repackages stuff so you don't have to™

# remember to set
# ROBOPACKAGE_GIT_DIR
# ROBOPACKAGE_HOME_PACKAGING_DIR

def runOrDie(cmd, action, doDie = true)
    if ENV["ROBOPACKAGE_DEBUG"] == "1"
        puts action + ": " + cmd
        return
    else
        puts action
    end

    if !system(cmd)
        puts "An error occured while:" + action + ", command was: "
        puts cmd

        if doDie
            Process.exit 1
        end
    end
end

def fixPath(path)
    if path.split('').last != "/"
        path += "/"
    end
    
    path
end

require 'rubygems'
require 'trollop'
require 'pp'

if !ENV["ROBOPACKAGE_GIT_DIR"] || !ENV["ROBOPACKAGE_HOME_PACKAGING_DIR"]
    puts "You must set ROBOPACKAGE_GIT_DIR and ROBOPACKAGE_HOME_PACKAGING_DIR"
    Process.exit 1
end

# fix paths
ENV["ROBOPACKAGE_GIT_DIR"] = fixPath(ENV["ROBOPACKAGE_GIT_DIR"])
ENV["ROBOPACKAGE_HOME_PACKAGING_DIR"] = fixPath(ENV["ROBOPACKAGE_HOME_PACKAGING_DIR"])

if !File.directory?(ENV["ROBOPACKAGE_GIT_DIR"])
    puts "Your ROBOPACKAGE_GIT_DIR doesn't exist"
    Process.exit 1
end

if (!File.directory?(ENV["ROBOPACKAGE_HOME_PACKAGING_DIR"]))
    puts "Your ROBOPACKAGE_HOME_PACKAGING_DIR doesn't exist"
    Process.exit 1
end

def main
    opts = Trollop::options do
        banner "Update the packaging to a given git tag"
        opt :package,
            "The package to update",
            :default => ""
        opt :tag,
            "The git tag to update to",
            :default => ""
        opt :pversion,
            "The version to release as the tag as",
            :default => ""
        opt :outdir,
            "Where the packaging lives",
            :default => ENV["ROBOPACKAGE_HOME_PACKAGING_DIR"]
        opt :onlypatches,
            "Only regenerate patches, don't touch anything else",
            :default => false
        opt :debug,
            "Don't do anything, just print out what would happen",
            :default => true
    end

    Trollop::die("No package specified") unless opts[:package].length > 0
    Trollop::die("No tag specified") unless opts[:tag].length > 0
    Trollop::die("No pversion specified") unless opts[:pversion].length > 0
    ENV["ROBOPACKAGE_DEBUG"] = "1" if opts[:debug]

    # setup
    packagePath = ENV["ROBOPACKAGE_HOME_PACKAGING_DIR"] + opts[:package]
    sourcePath = ENV["ROBOPACKAGE_GIT_DIR"] + opts[:package]

    if !File.directory?(packagePath)
        puts "Your packaging working dir for " + opts[:package] + " doesn't exist: " + packagePath
        Process.exit 1
    end

    if !File.directory?(sourcePath)
        puts "Your source dir for " + opts[:package] + " doesn't exist: " + sourcePath
        Process.exit 1
    end

    if not opts[:onlypatches]
        # search for (and remove) old tarball
        runOrDie("cd #{packagePath} && osc rm --force *.tar.gz", "removing tarballs", false)
    end

    runOrDie("cd #{packagePath} && osc rm --force *.patch", "removing patches", false)

    if not opts[:onlypatches]
        # update source archive
        runOrDie("cd #{sourcePath} && git fetch", "updating source")

        # create a new tarball
        runOrDie("cd #{sourcePath} && git checkout #{opts[:tag]}", "checking out tag")
        runOrDie("cd #{sourcePath} && (git archive --format=tar --prefix=#{opts[:package]}-#{opts[:pversion]}/ #{opts[:tag]} | gzip > #{File.join(packagePath, opts[:package] + "-" + opts[:pversion] + ".tar.gz")})", "creating new source archive")
    end

    # update and create patch series
    runOrDie("cd #{sourcePath} && git checkout rb-integration", "checking out integration branch") # TODO: don't hardcode this branch
    runOrDie("cd #{sourcePath} && git rebase #{opts[:tag]}", "rebasing patches on top of upstream tag");
    runOrDie("cd #{sourcePath} && git format-patch --no-numbered #{opts[:tag]}...", "converting patches to series")
    runOrDie("cd #{sourcePath} && mv *.patch #{packagePath}", "moving patches to package dir", false)
    runOrDie("cd #{packagePath} && for i in *.patch; do mv \"$i\" `echo \"$i\" | sed -e \"s/^[0-9]*-\\(.*\\)/#{opts[:package]}-#{opts[:pversion]}-\\1/g\"`; done", "renaming patches to remove numerical prefixes")

    # OBS magic
    # TODO: automate me more
    runOrDie("cd #{packagePath} && osc addremove", "adding and removing files to package")
    runOrDie("cd #{packagePath} && osc vc", "updating changelog")
    runOrDie("cd #{packagePath} && vim *.spec", "editing spec")

    if not opts[:onlypatches]
        runOrDie("cd #{sourcePath} && git branch  -D rb-integration-bak", "removing backup integration branch", false) # TODO: don't hardcode this branch
        runOrDie("cd #{sourcePath} && git checkout -b rb-integration-bak", "backing up integration branch") # TODO: don't hardcode this branch
        runOrDie("cd #{sourcePath} && git checkout -b rb-integration", "going back to integration branch") # TODO: don't hardcode this branch
    end
end

main if $0 == __FILE__

