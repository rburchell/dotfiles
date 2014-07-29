## greplog.pl
##
## search my database for interesting things
## needs perl-DBI and perl-DBD-SQLite and perl-Time-Piece

use strict;
use Irssi;
use MIME::Base64;
use DBI;
use Time::Piece;
use vars qw($VERSION %IRSSI);

$VERSION = "0.1";
%IRSSI = (
    authors     => 'Robin Burchell',
    contact     => 'robin+git@viroteck.net',
    name        => 'greplog.pl',
    description => 'Search my database for interesting things',
    license     => 'GNU General Public License',
    url         => 'https://github.com/rburchell/dotfiles',
);

my $dbfile = Irssi::get_irssi_dir . '/logs/logs.db';

sub cmd_greplog
{
    my ($data, $server, $witem) = @_;

    if (!$data) {
        Irssi::print("greplog: need a string to search for");
        return;
    }

    my $dbh = DBI->connect("dbi:SQLite:dbname=$dbfile", "", "");

    if (!$dbh) {
        Irssi::print("greplog: " . $DBI::errstr);
        return;
    }

    Irssi::print("Searching for $data");

    my $stmt = $dbh->prepare("SELECT * FROM irc_logs WHERE message LIKE ? LIMIT 10");
    my $res = $stmt->execute("%" . $data . "%");

    while (my $row = $stmt->fetchrow_hashref()) {
        my $date =localtime($row->{ts})->strftime("%F %T");
        print("$row->{network}/$row->{place}: $date $row->{message}");
    }

    Irssi::print("End search for $data");
    my $dbh
}

Irssi::command_bind('greplog', 'cmd_greplog');
