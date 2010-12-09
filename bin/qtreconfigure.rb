#!/usr/bin/ruby

require 'pp'

def usage
    puts "build [branch] [debug|release]"
    Process.exit 1
end

def die(msg)
    puts "ERROR: #{msg}"
    Process.exit 2
end

def warn(msg)
    puts "WARNING: #{msg}"
end

usage if ARGV.length < 2

case ARGV[1]
    when "debug"
    when "release"
    else
        puts "I don't know this type of build"
        usage
end

thisFolderName = File.dirname(__FILE__)
branchFolderName = "#{ARGV[0]}-#{ARGV[1]}"

system("git new-workdir qt.git #{branchFolderName} #{ARGV[0]}") or
    warn("Couldn't create a new workdir")
#if not File.directory?(branchFolderName)
#    puts "Creating clone for branch #{branchFolderName}"
#    die("Couldn't create clone!") if 
#        !system("git clone --shared qt.git #{branchFolderName}")
#
#    system("cd #{branchFolderName} && git checkout #{ARGV[0]}") or
#        die("Couldn't checkout given branchname #{ARGV[0]}")
#end

puts "Configuring #{ARGV[0]} (#{ARGV[1]})"

# find what options to use
configureOptions = []

# generic options first
configureOptions << "-no-pch"
configureOptions << "-graphicssystem raster"
configureOptions << "-developer-build"
configureOptions << "-opensource"
configureOptions << "-system-sqlite"
configureOptions << "-prefix /usr"
configureOptions << "-confirm-license"
configureOptions << "-phonon"
configureOptions << "-phonon-backend"
configureOptions << "-fast"
configureOptions << "-nomake demos"
configureOptions << "-nomake examples"
#configureOptions << "-no-script"
#configureOptions << "-no-scripttools"
#configureOptions << "-no-declarative"

case ARGV[1]
    when "debug"
    when "release"
        configureOptions << "-release"

        # also hack the build system to not strip symbols
        # and include basic debug info, thanks kling
        puts "Applying buildsystem hack..."
        warn("Couldn't apply buildsystem hack") if
            !system("cd #{branchFolderName} && patch --forward -f -p1 < ../.releasedebuginfo.diff")

        system("cd #{branchFolderName} && git update-index --assume-unchanged mkspecs/common/linux.conf") or
            die("Couldn't mark mkspecs/common/linux.conf unchanged")
        system("cd #{branchFolderName} && git update-index --assume-unchanged mkspecs/common/gcc-base.conf") or
            die("Couldn't mark mkspecs/common/gcc-base.conf unchanged")

    else
        puts "please teach me this build type!"
        Process.exit 5
end

configureOptions = configureOptions.join " "

puts "Configuring..."
die("Couldn't configure!") if
    !system("cd #{branchFolderName} && ./configure #{configureOptions}")
