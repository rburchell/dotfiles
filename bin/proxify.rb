#!/usr/bin/ruby

def stdin_nonblock_read
    text = ""
    loop do
        begin
            t = $stdin.read_nonblock(1)
        rescue
            break
        else
            text << t
        end
    end
    text.chomp
end

def debugPrint text
    puts "#{File.basename(__FILE__)}: #{text}"
end

# check they are actually trying to run something
if ARGV.length == 0
	debugPrint "you need to pass an argument"
end

stdinText = stdin_nonblock_read

if `/sbin/ifconfig eth0 | /bin/grep "inet addr" | /usr/bin/awk -F: '{ print $2 }' | /usr/bin/awk '{ print $1 }' |  /bin/grep -qE '172\.21\.[0-9]{1,3}\.[0-9]{1,3}'`
    debugPrint "tsocksifying";

    # check for started ssh tunnel
    command="ssh -fND 1234 w00t@dereferenced.net"
    `pgrep -f -x \"#{command}\" > /dev/null 2>&1 || #{command}`
    debugPrint "done tunnelling"

    `export TSOCKS_CONF_FILE=~/.tsocks.conf`

    commandToRun = ""

    if stdinText != ""
        commandToRun += "echo \"#{stdinText}\" | "
    end

    commandToRun += " tsocks #{ARGV.join(" ")}"

    system(commandToRun)
else
    puts `echo "#{stdinText}" | #{ARGV.join(" ")}`
    commandToRun = ""

    if stdinText != ""
        commandToRun += "echo \"#{stdinText}\" | "
    end

    commandToRun += "#{ARGV.join(" ")}"

    system(commandToRun)
end
