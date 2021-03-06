#!/usr/bin/perl
use strict;

sub percent($$) {
    my $value = $_[0];
    my $total = $_[1];
    return "n/a" if $total == 0;
    return $value * 100 / $total;
}

my $prefix;
my %lines;
sub sharedPrivateReport($) {
    my $type = $_[0];
    my $clean = $lines{$type . "_Clean"};
    my $dirty = $lines{$type . "_Dirty"};
    my $anonymous =
	$lines{"Anonymous_${type}_Clean"} +
	$lines{"Anonymous_${type}_Dirty"};
    my $thread_stack =
	$lines{"Thread_Stack_${type}_Clean"} +
	$lines{"Thread_Stack_${type}_Dirty"} +
	$lines{"Main_Stack_${type}_Clean"} +
	$lines{"Main_Stack_${type}_Dirty"};
    my $code =
	$lines{"Code_${type}_Clean"} +
	$lines{"Code_${type}_Dirty"};
    my $rodata =
	$lines{"ROData_${type}_Clean"} +
	$lines{"ROData_${type}_Dirty"};
    my $rwdata =
	$lines{"RWData_${type}_Clean"} +
	$lines{"RWData_${type}_Dirty"};
    my $total = $clean + $dirty;
    my $other = $total - $anonymous - $thread_stack - $rodata - $rwdata - $code;

    printf("${prefix}                %7s: %d kB total (%.1f%% of RSS)\n",
          $_[0], $total, percent($total, $lines{'Rss'}));
    printf("${prefix}              Breakdown: %d kB clean (%.1f%%), %d kB dirty (%.1f%%)\n",
	   $clean, percent($clean, $total),
	   $dirty, percent($dirty, $total));
    printf("${prefix}                         %d kB code (%.1f%%), %d kB RO data (%.1f%%), %d kB RW data (%.1f%%)\n".
	   "${prefix}                         %d kB heap (%.1f%%), %d kB stack (%.1f%%), %d kB other (%.1f%%)\n",
	   $code, percent($code, $total),
	   $rodata, percent($rodata, $total),
	   $rwdata, percent($rwdata, $total),
	   $anonymous, percent($anonymous, $total),
	   $thread_stack, percent($thread_stack, $total),
	   $other, percent($other, $total));
}

for my $arg (@ARGV) {
    open DATA, "</proc/$arg/smaps" or die("Cannot open smaps file for PID $arg");
    my @header;
    my @lastheader;
    my $likely_thread_stack;
    %lines = ();
    while (<DATA>) {
	if (/^([0-9a-f-]+) ([rwxps-]{4}) ([0-9a-f-]+) ([0-9a-f:]+) (\d+)\s+(.*)$/) {
	    @lastheader = @header;
	    @header = ($1, $2, $3, $4, $5, $6)
	}

        /(\w+):\s*(\d+) kB/ or next;
        $lines{$1} += $2;
	if ($header[5] eq '[stack]' || $header[5] eq "[stack:$arg]") {
	    $lines{"Main_Stack_$1"} += $2;
        } elsif ($header[5] =~ m/\[stack:/) {
            $lines{"Thread_Stack_$1"} += $2;
	} elsif ($header[1] eq 'rw-p') {
	    $likely_thread_stack = ($2 == 8192 && $header[3] eq '00:00'
				    && $lastheader[1] eq '---p')
		if ($1 eq 'Size');
	    if ($likely_thread_stack) {
		$lines{"Thread_Stack_$1"} += $2;
	    } elsif ($header[3] eq '00:00') {
		$lines{"Anonymous_$1"} += $2;
	    } else {
		$lines{"RWData_$1"} += $2;
	    }
	} elsif ($header[1] eq 'rw-s' && $header[3] ne '00:00') {
	    $lines{"RWData_$1"} += $2;
	} elsif ($header[1] eq 'r--p' || $header[1] eq 'r--s') {
	    $lines{"ROData_$1"} += $2;
	} elsif ($header[1] eq 'r-xp') {
	    $lines{"Code_$1"} += $2;
	} elsif ($header[1] eq '---p') {
	    $lines{"Padding_$1"} += $2;
	}
    }
    close DATA;

    $prefix = "$arg:  " if (scalar @ARGV > 1);
    printf "${prefix}Total mapped memory:     %d kB\n", $lines{'Size'};
    printf "${prefix}    of which is padding: %d kB\n", $lines{'Padding_Size'};
    printf "${prefix}   of which swapped out: %d kB\n", $lines{'Swap'};
    printf "${prefix}  of which likely stack: %d kB mapped, %d kB resident (%d kB main, %d kB aux threads)\n",
          $lines{'Main_Stack_Size'} + $lines{'Thread_Stack_Size'},
          $lines{'Main_Stack_Rss'} + $lines{'Thread_Stack_Rss'},
          $lines{'Main_Stack_Rss'}, $lines{'Thread_Stack_Rss'};

    printf "${prefix}  of which are resident: %d kB, %d kB proportionally shared\n", $lines{'Rss'},
          $lines{'Pss'};
    printf "${prefix}        total anonymous: %d kB (%.1f%%)\n",
	$lines{'Anonymous'}, percent($lines{'Anonymous'}, $lines{'Rss'});

    sharedPrivateReport('Shared');
    sharedPrivateReport('Private');
}
