#!/usr/bin/ruby
require 'pp'

$binName = nil
$writtenBinName = false
$outputType = "text"
$outFile = nil
$level = -2
$libstack = []

def recurseInto(libName)
  # TODO: this means circular dependencies don't make us explode, but it also
  # means we don't show a complete tree by ignoring repeated dependencies.
  # how can we fix this?
  $level += 1
  $libstack << libName
  if ($outputType == "text")
      0.upto($level).each {
        print "*"
      }

      print " "
      print libName
      print "\n"
  elsif $outputType == "dot" then
      $outFile.write('"' + libName + '"')
  end

  libs = `readelf -d #{libName} 2>&1 | grep NEEDED`.split("\n")
  libs.each { |lib|
    # yuck, I am sure this can be done better
    lib.sub(/.+\[(.*)\]/) { lib = $1 }

    # TODO: LD_LIBRARY_PATH, and other such fun things
    libPath = "/usr/lib/#{lib}"
    if $libstack.index(libPath) == nil then
        if $outputType == "dot" then
            if (!$writtenBinName) then
                $outFile.write($binName)
                $writtenBinName = true
            end
            $outFile.write(" -- ")
        end
        recurseInto(libPath)
    end
  }
  if $outputType == "dot" then
      $writtenBinName = false
      $outFile.write("\n")
  end

  $level -= 1
end

if ARGV[0][0].chr() == '-'
    if ARGV[0] == "-dot"
        $outputType = "dot"
    else
        print "unknown command argument " + ARGV[0]
        exit -2
    end

    ARGV.shift
end

print "Dependency tree for #{ARGV[0]}\n"

$binName = File.basename(ARGV[0])

if $outputType == "dot"
    $outFile = File.new(ARGV[1], "w")
    $outFile.write("graph #{$binName} {\n");
end

recurseInto(ARGV[0])

if $outputType == "dot"
    $outFile.write("}\n");
end
