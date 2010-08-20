#!/usr/bin/ruby
if !File.exists?("/j")
    puts "Shortcut dir doesn't exist, need to create it"
    `sudo mkdir "/j"`
    `sudo chmod o+rw "/j"`
end

if ARGV.length == 0
    puts `ls /j/`
else
    puts "Creating a jump for " << ARGV[0]
    puts system("ln -s `pwd` /j/" + ARGV[0])
end
