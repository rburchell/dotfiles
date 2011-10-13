#!/usr/bin/ruby1.8
# A lot of this taken (with gratitude) from Dan Finnie's clipboard script.
#   See: http://ruby.pastebin.com/f5ef5f028

require "gtk2"
require "pp"
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

cfg = File.new(File.expand_path("~/.pastebin.cfg"))
cfgVersion = cfg.readline.chomp # ignored for now
cfgUser = cfg.readline.chomp
cfgPass = cfg.readline.chomp

text = stdin_text_or_clipboard

if File.file? text
    # TODO: it would be nice if we could verify somehow that they copied a file
    # to clipboard.

    f = File.new(text, "rb")
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
    exit
else
    agent = Mechanize.new
    agent.basic_auth(cfgUser, cfgPass)
    reply = agent.post(
                "http://qtl.me/upload/",
                {
                    :content => text
                }
            )
    print reply.body + "\n"
    Clipboard::set reply.body
end

