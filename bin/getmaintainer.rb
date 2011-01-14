#!/usr/bin/ruby

require "pp"

ARGV.each { |s|
    puts "Maintainers for #{s}:"
    system("git blame #{s} | cut -d'(' -f2 | cut -d' ' -f1 | sort | uniq -c | sort -nr | head -n20")
}
