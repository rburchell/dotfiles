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

text = Clipboard.get
if text.empty?
    puts "Nothing on clipboard"
    Process.exit 1
end

isGitDiff = false
isGitDiff = true if text =~ /^diff --git a/m

Tempfile.open("patch") { |tf|
    tf.write text
    tf.close

    patchlevel = 0

    if isGitDiff
        patchlevel = 1
    end

    system("patch -p#{patchlevel}< #{tf.path}")
    puts "Applied"
}
