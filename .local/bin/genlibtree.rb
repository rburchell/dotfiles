#!/usr/bin/ruby
require 'pp'

$outputType = "text"
$outFile = nil
$level = -2
$nodeHash = {}
$currentNode = nil
$oldNodes = []
$libSearchPaths = []

def recurseInto(libName)
  # TODO: this means circular dependencies don't make us explode, but it also
  # means we don't show a complete tree by ignoring repeated dependencies.
  # how can we fix this?
  $level += 1
  if ($outputType == "text")
      0.upto($level).each {
        print "*"
      }

      print " "
      print libName
      print "\n"
  elsif $outputType == "dot" then
      # do nothing, dot handled below
  end

  if ($currentNode == nil)
      $currentNode = libName
  end

  libs = `eu-readelf -d #{libName} 2>&1 | grep NEEDED`.split("\n")
  libs.each { |lib|
    # yuck, I am sure this can be done better
    lib.sub(/.+\[(.*)\]/) { lib = $1 }

    foundLib = false
    libPath = ""

    $libSearchPaths.each() { |path|
        libPath = "#{path}/#{lib}"
        if File.exists?(libPath)
            foundLib = true
            break
        end
    }

    if (!foundLib)
        print "cannot find lib " + lib + "\n"
        print "searched paths:\n"
        pp $libSearchPaths
        exit 3
    end

    print "current node is " + $currentNode + " examining " + libPath + "\n"

    if ($nodeHash.has_key?($currentNode + libPath))
        next
    end

    if ($nodeHash.has_key?(libPath + $currentNode))
        next
    end

    $nodeHash[$currentNode + libPath] = true
    $nodeHash[libPath + $currentNode] = true

    if $outputType == "dot" then
         $outFile.write('"' + $currentNode + '" -- "' + libPath + '"' + "\n")
    end

    $oldNodes << $currentNode
    $currentNode = libPath
    recurseInto(libPath)
    $currentNode = $oldNodes.pop()
  }

  $level -= 1
end


if ENV["LD_LIBRARY_PATH"]
    paths = ENV["LD_LIBRARY_PATH"].split(":")
    paths.each() { |path|
        $libSearchPaths << path
    }
end

$libSearchPaths << "/usr/local/lib"
$libSearchPaths << "/usr/lib"
$libSearchPaths << "/lib"
$libSearchPaths << "/lib64"

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

#$outputType="dot"
#ARGV[1]="out.dot"
if $outputType == "dot"
    $outFile = File.new(ARGV[1], "w")
    $outFile.write("graph \"#{ARGV[0]}\" {\n");
end

recurseInto(ARGV[0])

if $outputType == "dot"
    $outFile.write("}\n");
end
