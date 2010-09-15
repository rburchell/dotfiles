#!/usr/bin/ruby
# A lot of this taken (with gratitude) from Dan Finnie's clipboard script.
#   See: http://ruby.pastebin.com/f5ef5f028

require "gtk2"
require "tempfile"

module Clipboard
    Gtk.init

    def self.set(text)
        text = text.to_s
        c = Gtk::Clipboard.get(Gdk::Selection::CLIPBOARD)
        c.set_text(text)
        c.store
        text
    end

    def self.get
        c = Gtk::Clipboard.get(Gdk::Selection::CLIPBOARD)
        c.request_text{|_, text| return text}
        Gtk::main
    end
end


def stdin_text_or_clipboard
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

    # if they don't provide anything, clipboard.
    if text.empty?
        text = Clipboard.get
    end

    if text == nil || text.empty?
        puts "nothing to do"
        Process.exit
    end

    text.chomp
end

def doPastebin(fileName, targetFileName)
    # sanity checking
    targetFileName.gsub!(/ /, "")

    fileName.gsub!(/ /, "\\ ")

    puts "Uploading #{fileName} to #{targetFileName}"
    path = "http://w00t.dereferenced.net/p/t/#{targetFileName}"
    Clipboard::set path
    `scp #{fileName} "w00t@dereferenced.net:/var/www/w00t.dereferenced.net/p/t/#{targetFileName}"`
    `ssh w00t@dereferenced.net 'chmod o+rw /var/www/w00t.dereferenced.net/p/t/#{targetFileName}'`
    puts "Uploaded snippet to #{path}"
end

text = stdin_text_or_clipboard

if File.file? text
    # TODO: it would be nice if we could verify somehow that they copied a file
    # to clipboard.

    # make name safe/readable:
    # - remove spaces
    # - remove extension (we add it after random bit later)
    # this is not exhaustive but should do.
    targetFileName = text.split('.').first
    targetExtension = text.split('.').last

    if targetExtension == targetFileName
        targetExtension = ""
    end

    # remove path bit
    targetFileName = File.basename(targetFileName)

    # create a temporary filename so we don't accidentally overwrite files
    Tempfile.open(targetFileName) { |tf|
        tf.close

        # add temporary bit
        targetFileName = File.basename(tf.path)

        # add extension if it exists
        if targetExtension != ""
            targetFileName += "." + targetExtension
        end

        # yuck, I really need server side magic to avoid this
        targetFileName += case targetExtension
                when "rb"
                    ".txt"
                when "php"
                    ".txt"
                else ""
            end

        doPastebin(text, targetFileName)
    }
else
    Tempfile.open("pastebin") { |tf|
        tf.write text
        tf.close
        doPastebin(tf.path, File.basename(tf.path) + ".txt")
    }
end

