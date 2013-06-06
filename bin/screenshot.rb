#!/usr/bin/ruby1.8
# Some of this taken (with gratitude) from Dan Finnie's clipboard script.
#   See: http://ruby.pastebin.com/f5ef5f028

require "gtk2"
require "tempfile"
require "rubygems"
require "mechanize"

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

cfg = File.new(File.expand_path("~/.pastebin.cfg"))
cfgVersion = cfg.readline.chomp # ignored for now
cfgUser = cfg.readline.chomp
cfgPass = cfg.readline.chomp

Tempfile.open(["imgbin", ".png"]) { |tf|
    if ARGV.length == 0
        # -b captures border,-s selects a window, -u is use focused
        `scrot -b -u #{tf.path}`
    else
        `scrot #{tf.path}`
    end

    f = File.new(tf.path, "rb")
    agent = Mechanize.new
    agent.basic_auth(cfgUser, cfgPass)
    reply = agent.post(
                "http://qtl.me/upload/",
                {
                    :file => f
                }
            )
    print reply.body + "\n"
    Clipboard::set reply.body
}
