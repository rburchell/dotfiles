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

# if they don't provide anything, clipboard.
if text.empty?
    text = `xsel`
end

if text.empty?
    text = Clipboard.get
end

if text.empty?
    puts "nothing to do"
    Process.exit
end

Tempfile.open("pastebin") { |tf|
    tf.write text
    tf.close
    fname = File.basename tf.path
    fname += ".txt"
    path = "http://w00t.dereferenced.net/p/t/#{fname}"
    Clipboard::set path
    `scp #{tf.path} "w00t@dereferenced.net:/var/www/w00t.dereferenced.net/p/t/#{fname}"`
    `ssh w00t@dereferenced.net 'chmod o+r /var/www/w00t.dereferenced.net/p/t/#{fname}'`
    puts "Uploaded snippet to #{path}"
}
