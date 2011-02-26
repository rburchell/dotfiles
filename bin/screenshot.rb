#!/usr/bin/ruby
# Some of this taken (with gratitude) from Dan Finnie's clipboard script.
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
end

Tempfile.open("imgbin") { |tf|
    # we don't really actually use the tempfile.
    tf.close

    if ARGV.length == 0
        # -b captures border,-s selects a window, -u is use focused
        `scrot -b -u #{tf.path}.jpg`
    else
        `scrot #{tf.path}.jpg`
    end

    fname = File.basename tf.path
    fname += ".jpg"
    path = "http://w00t.dereferenced.net/p/i/#{fname}"
    Clipboard::set path
    `scp #{tf.path}.jpg "w00t@dereferenced.net:/var/www/w00t.dereferenced.net/p/i/#{fname}"`
    `ssh w00t@dereferenced.net 'chmod o+rw /var/www/w00t.dereferenced.net/p/i/#{fname}'`
    puts "Uploaded screenshot to #{path}"
}
