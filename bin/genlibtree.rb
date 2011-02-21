require 'pp'

$level = -2
$libstack = []

def recurseInto(libName)
  # TODO: this means circular dependencies don't make us explode, but it also
  # means we don't show a complete tree by ignoring repeated dependencies.
  # how can we fix this?
  $level += 1
  $libstack << libName
  0.upto($level).each {
	print "*"
  }

  print " "
  print libName
  print "\n"

  libs = `readelf -d #{libName} 2>&1 | grep NEEDED`.split("\n")
  libs.each { |lib|
    # yuck, I am sure this can be done better
    lib.sub(/.+\[(.*)\]/) { lib = $1 }

    # TODO: LD_LIBRARY_PATH, and other such fun things
    libPath = "/usr/lib/#{lib}"
    if $libstack.index(libPath) == nil then
        recurseInto(libPath)
    end
  }

  $level -= 1
end

print "Dependency tree for #{ARGV[0]}\n"
recurseInto(ARGV[0])
