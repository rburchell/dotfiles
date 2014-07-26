## term_notify.pl
##
## send notifications through terminal escape codes.
## requires iTerm2 with https://github.com/gnachman/iTerm2/pull/173 (or the
## #equivilant patch for your terminal of choice).

use strict;
use Irssi;
use MIME::Base64;
use vars qw($VERSION %IRSSI);

$VERSION = "0.1";
%IRSSI = (
    authors     => 'Robin Burchell',
    contact     => 'robin+git@viroteck.net',
    name        => 'term_notify.pl',
    description => 'Use terminal escape codes to send notifications',
    license     => 'GNU General Public License',
    url         => 'https://github.com/rburchell/dotfiles',
);

sub notify {
    my ($server, $summary, $message) = @_;

    $summary = encode_base64($summary);
    $message = encode_base64($message);

    STDERR->autoflush(1);
    print STDERR "\033]50;Notification=title=$summary\;description=$message\a"
}
 
sub print_text_notify {
    my ($dest, $text, $stripped) = @_;
    my $server = $dest->{server};

    return if (!$server || !($dest->{level} & MSGLEVEL_HILIGHT));
    my $sender = $stripped;
    $sender =~ s/^\<.([^\>]+)\>.+/\1/ ;
    $stripped =~ s/^\<.[^\>]+\>.// ;
    my $summary = sprintf("%s @ %s", $sender, $dest->{target});
    notify($server, $summary, $stripped);
}

sub message_private_notify {
    my ($server, $msg, $nick, $address) = @_;

    return unless $server;
    notify($server, sprintf("%s @ %s", $nick, $server->{address}), $msg);
}

Irssi::signal_add('print text', 'print_text_notify');
Irssi::signal_add('message private', 'message_private_notify');
