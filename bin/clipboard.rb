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


def stdin_text
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

text = stdin_text

if text.empty?
    text = ARGF.read
    if text.empty?
        puts "No stdin to copy"
        Process.exit 1
    end
end

Clipboard::set text
puts "Copied to clipboard"
