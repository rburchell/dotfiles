#!/usr/bin/ruby

require 'rubygems'
require 'trollop'
require 'pp'

def main
  opts = Trollop::options do
    banner "Extract a git tag into a tarball"
    opt :tag_prefix,
        "The git tag to archive is formed by concatenating this prefix with the version",
        :default => ""
    opt :outdir,
        "Where the output tarball should be saved",
        :default => ".."
    opt :name,
        "Package base name",
        :default => guess_package_name
    opt :package_name,
        "Full package name, can contain ruby interpolation directives with arbitrary code",
        :default => '#{name}-#{tag_version}'
    opt :dry_run,
        "Only print the archiving command, without actually running it",
        :default => false
    opt :tag_version,
        "The package tag_version number, if not specified, grabs the newest tag prefixed with tag-prefix",
        :default => false
  end
  opts[:tag_version] = opts[:tag_version] || guess_package_version(opts[:tag_prefix])
  pp opts
  cmd = Packager.new(opts).command
  puts cmd
  system(cmd) unless opts[:dry_run]
end

def guess_package_name
  File.basename(Dir.pwd)
end

def guess_package_version(tagprefix)
  a = `git tag -l "#{tagprefix}*"`.split("\n").sort { |a,b|
    # this won't work for 'tp1' vs 'beta' (e.g. Qt), should probably think how to fix that
    aa = a.split('.').map{|s|s.to_i}
    ab = b.split('.').map{|s|s.to_i}
    aa <=> ab
  }.reverse
  return a.first.sub(tagprefix, "")
end

class Packager
  def initialize(opts)
    @opts = opts
  end

  def method_missing(name)
    @opts[name]
  end

  def command
    full_name = instance_eval(%Q{"#{package_name}"})
    archive_command = "git archive --format=tar --prefix=#{full_name}/" +
                      " #{tag_prefix}#{tag_version}"
    zip_command = "gzip > #{File.join(outdir, full_name + ".tar.gz")}"
    [archive_command, zip_command].join(" | ")
  end
end

main if $0 == __FILE__

# vim:sw=2:ts=2:sts=2:ft=ruby:
