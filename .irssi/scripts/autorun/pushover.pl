# Copyright (c) 2014 Chip Marshall <chip@2bithacker.net>
# Modified from irssi-prowl - https://github.com/henrikbrixandersen/irssi-prowl
# 
# Copyright (c) 2012 Henrik Brix Andersen <henrik@brixandersen.dk>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

use strict;
use warnings;

use List::Util qw/min max/;

use Irssi;
require LWP::UserAgent;
use HTTP::Request::Common qw(POST);

our $VERSION = '1.0';
our %IRSSI = (
    authors     => 'Chip Marshall',
    contact     => 'chip@2bithacker.net',
    name        => 'pushover',
    description => 'Send Pushover notifications from Irssi',
    license     => 'BSD',
    url         => 'https://github.com/2bithacker/irssi-pushover',
    modules     => 'List::Util LWP::UserAgent HTTP::Request::Common',
    commands    => 'pushover',
    );

my $ua = LWP::UserAgent->new;
my %config = ( apikey => '', userkey => '');

# Settings
Irssi::settings_add_str('pushover', 'pushover_mode', 'AUTO');
Irssi::settings_add_str('pushover', 'pushover_apikey', 'ajBrWojskNwd6hGopj2FmSkvcdkyWq');
Irssi::settings_add_str('pushover', 'pushover_userkey', '');
Irssi::settings_add_bool('pushover', 'pushover_debug', 0);
Irssi::settings_add_int('pushover', 'pushover_priority_msgs', 0);
Irssi::settings_add_int('pushover', 'pushover_priority_hilight', 0);
Irssi::settings_add_int('pushover', 'pushover_priority_cmd', 0);
Irssi::settings_add_str('pushover', 'pushover_regex_include', '');
Irssi::settings_add_str('pushover', 'pushover_regex_exclude', '');

# Signals
Irssi::signal_add('setup changed' => 'setup_changed_handler');
setup_changed_handler();
Irssi::signal_add('print text' => 'print_text_handler');
Irssi::signal_add_first('complete word',  'complete_word_handler');

# Commands
Irssi::command_bind('help', 'help_command_handler');
Irssi::command_bind('pushover', 'pushover_command_handler');
Irssi::command_set_options('pushover', '-url @priority');

# Theme
Irssi::theme_register([
    'pushover_event_cmd',     'Manual Message',
    # $0 = channel, $1 = nick
    'pushover_event_msgs',    'Private Message', # from $1 is too long according to SIZE_TITLE in Pushover.pm
    'pushover_event_hilight', 'Hilighted in $0 by $1',
    # $0 = irc/ircs, $1 = server address, $2 = chatnet, $3 = server port, $4 = channel/nick
    'pushover_url_msgs',      '$0://$1:$3/',
    'pushover_url_hilight',   '$0://$1:$3/$4',
                      ]);

sub setup_changed_handler {
    $config{debug} = Irssi::settings_get_bool('pushover_debug');

    my $mode = Irssi::settings_get_str('pushover_mode');
    $mode =~ s/\s+$//g;
    if ($mode ne uc($mode)) {
        # Mimic uppercase Irssi bool settings for our tri-state setting
        $mode = uc($mode);
        Irssi::settings_set_str('pushover_mode', $mode);
        Irssi::signal_emit('setup changed');
    }
    if ($mode !~ /^(AUTO|ON|OFF)$/) {
        $mode = 'AUTO';
        Irssi::settings_set_str('pushover_mode', $mode);
        Irssi::signal_emit('setup changed');
    }
    $config{mode} = $mode;

    for (qw/msgs hilight cmd/) {
        my $priority = Irssi::settings_get_int("pushover_priority_$_");
        if ($priority < -2 || $priority > 2) {
            $priority = max($priority, -2);
            $priority = min($priority, 2);
            Irssi::settings_set_int("pushover_priority_$_", $priority);
            Irssi::signal_emit('setup changed');
        }
        $config{"priority_$_"} = $priority;
    }

    for (qw/include exclude/) {
        my $regex = Irssi::settings_get_str("pushover_regex_$_");
        if ($regex) {
            $config{$_} = eval { qr/$regex/ };
            Irssi::print("Invalid regular expression for 'pushover_regex_$_' setting: $@") if $@;
        } else {
            $config{$_} = undef;
        }
    }

    my $apikey = Irssi::settings_get_str('pushover_apikey');
    $apikey =~ s/\s+$//g;
    $config{apikey} = $apikey;

    my $userkey = Irssi::settings_get_str('pushover_userkey');
    $userkey =~ s/\s+$//g;
    $config{userkey} = $userkey;
}

sub _create_url {
    my ($server, $target, $format_name) = @_;
    my $url;

    if ($server->{chat_type} eq 'IRC') {
        my $format = Irssi::current_theme()->get_format('Irssi::Script::pushover', $format_name);

        my @data;
        push @data, $server->{use_ssl} ? 'ircs' : 'irc';
        push @data, ($server->{address}, $server->{chatnet}, $server->{port}, $target);

        $url = Irssi::parse_special($format, join(' ', @data));
    }

    return $url;
}

sub print_text_handler {
    my ($dest, $text, $stripped) = @_;
    my $server = $dest->{server};

    if (($server->{usermode_away} && $config{mode} eq 'AUTO') || $config{mode} eq 'ON') {
        my $target = $dest->{target};

        if ((!defined($config{include}) || $target =~ $config{include}) &&
            !(defined($config{exclude}) && $target =~ $config{exclude})) {
            my $level = $dest->{level};

            if (($level & MSGLEVEL_MSGS) || ($level & MSGLEVEL_HILIGHT && !($level & MSGLEVEL_NOHILIGHT))) {
                my $nick = $stripped;
                if ($level & MSGLEVEL_ACTIONS) {
                    $nick =~ s/^\s+.\s+(\S+)\s.*/$1/;
                } else {
                    $nick =~ s/^\<[@\+% ]?([^\>]+)\>.*/$1/;
                }

                unless ($server->{nick} eq $nick) {
                    my $type = ($level & MSGLEVEL_MSGS) ? 'msgs' : 'hilight';
                    my $url = _create_url($server, $target, "pushover_url_$type");
                    my $format = Irssi::current_theme()->get_format('Irssi::Script::pushover', "pushover_event_$type");
                    my $event = Irssi::parse_special($format, "$target $nick");

                    _pushover($event, $stripped, $config{"priority_$type"}, $url);
                }
            }
        }
    }
}

sub help_command_handler {
    my ($data, $server, $witem) = @_;
    $data =~ s/\s+$//g;

    if (lc($data) eq 'pushover') {
        Irssi::print("\nPROWL [-url <url>] [-priority <priority>] [text]\n\n" .
                     "Send a manual Pushover notification.\n\n" .
                     "See also: /SET PROWL, /FORMAT PROWL\n",
                     MSGLEVEL_CLIENTCRAP);
        Irssi::signal_stop;
    }
}

sub pushover_command_handler {
    my ($data, $server, $witem) = @_;
    $data =~ s/\s+$//g;

    my @options = Irssi::command_parse_options('pushover', $data);
    if (@options) {
        my $args = $options[0];
        my $text = $options[1] ? $options[1] : ' ';;

        my $format = Irssi::current_theme()->get_format('Irssi::Script::pushover', 'pushover_event_cmd');
        my $event = Irssi::parse_special($format);

        $args->{priority} //= $config{priority_cmd};
        $args->{priority} = max($args->{priority}, -2);
        $args->{priority} = min($args->{priority}, 2);

        _pushover($event, $text, $args->{priority}, $args->{url});
    }
}

sub complete_word_handler {
    my ($strings, $window, $word, $linestart, $want_space) = @_;

    if ($linestart =~ /^\/set pushover_mode/i) {
        push @$strings, grep(/^\Q$word\E/i, qw/AUTO ON OFF/);
        $$want_space = 0;
        Irssi::signal_stop;
    }
}

sub _pushover {
    my ($event, $description, $priority, $url) = @_;

    my %options = (title => $event, message => $description);
    $options{priority} = $priority if defined $priority;
    $options{url} = $url if defined $url;

    if ($config{debug}) {
        my $debuginfo = join(', ', map { "$_ => '$options{$_}'" } sort keys %options);
        Irssi::print("Sending Pushover notification: $debuginfo", MSGLEVEL_CLIENTCRAP);
    }

    unless ($config{apikey}) {
        Irssi::print('Pushover API key not set, use \'/SET pushover_apikey\' to set a valid key',
                     MSGLEVEL_CLIENTERROR);
        return;
    }
    unless ($config{userkey}) {
        Irssi::print('Pushover user key not set, use \'/SET pushover_userkey\' to set a valid key',
                     MSGLEVEL_CLIENTERROR);
        return;
    }

    $options{user} = $config{userkey};
    $options{token} = $config{apikey};

    #$pushover->message(%options);
    my $req = POST('https://api.pushover.net/1/messages.json', [ %options ] );
    my $response = $ua->request($req);
    if (!$response->is_success) {
        Irssi::print('Pushover request NOT OK ' . $response->status_line, MSGLEVEL_CLIENTCRAP);
        Irssi::print($response->decoded_content, MSGLEVEL_CLIENTCRAP);
        my $debuginfo = join(', ', map { "$_ => '$options{$_}'" } sort keys %options);
        Irssi::print("Data was: $debuginfo", MSGLEVEL_CLIENTCRAP);
    }
}

