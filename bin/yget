#!/usr/bin/perl -T

use strict;
use warnings;

#
##  Calomel.org  ,:,  Download Youtube videos and music using wget
##    Script Name : youtube_wget_video.pl
##    Version     : 0.42
##    Valid from  : March 2014
##    URL Page    : https://calomel.org/youtube_wget.html
##    OS Support  : Linux, Mac OSX, OpenBSD, FreeBSD or any system with perl
#                `:`
## Two arguments
##    $1 Youtube URL from the browser
##    $2 prefix to the file name of the video (optional)
#

############  options  ##########################################

# Option: what file type do you want to download? The string is used to search
# in the youtube URL so you can choose mp4, webm, avi or flv.  mp4 seems to
# work on the most players like android, ipod, ipad, iphones, vlc and mplayer.
my $fileType = "mp4";

# Option: what visual resolution or quality do you want to download? List
# multiple values just in case the highest quality video is not available, the
# script will look for the next resolution. You can choose "highres" for 4k,
# "hd1080" for 1080p, "hd720" for 720p, "itag=18" which means standard
# definition 640x380 and "itag=17" which is mobile resolution 144p (176x144).
# The script will always prefer to download the highest resolution video format
# from the list if available.
my $resolution = "hd720,itag=18";

# Option: How many times should the script retry the download if wget fails for
# any reason? Do not make this too high as a reoccurring error will just hit
# youtube over and over again. 
my $retryTimes = 20;

# Option: do you want the resolution of the video in the file name? zero(0) is
# no and one(1) is yes. This option simply puts "_hd1080.mp4" or similar at the
# end of the file name.
my $resolutionFilename = 0;

# Option: Force all communication with YouTube to use SSL (https) links. The
# script will simply convert all URL's you pass to the script to use https
# instead of http. Encryption better protects your privacy and may help avoid
# ISP rate limiting. 
my $forceSSL = 1;

# Option: turn on DEBUG mode. Use this to reverse engineering this code if you are
# making changes or you are building your own youtube download script.
my $DEBUG=0;

#################################################################

# initialize retry loop and resolution variables
$ENV{PATH} = "/bin:/usr/bin:/usr/local/bin";
my $prefix = "";
my $retry = 1;
my $retryCounter = 0;
my $resFile = "unknown";
my $user_url = "";
my $user_prefix = "";

# collect the URL from the command line argument
chomp($user_url = $ARGV[0]);
my $url = "$1" if ($user_url =~ m/^([a-zA-Z0-9\_\-\&\?\=\:\.\/]+)$/ or die "\nError: Illegal characters in YouTube URL\n\n" );

# declare the user defined file name prefix if specified
if (defined($ARGV[1])) {
   chomp($user_prefix = $ARGV[1]);
   $prefix = "$1" if ($user_prefix =~ m/^([a-zA-Z0-9\_\-\.\ ]+)$/ or die "\nError: Illegal characters in filename prefix\n\n" );
}

# while loop to retry downloading the video if the script fails for any reason
while ( $retry != 0 && $retryCounter < $retryTimes ) {

# Force SSL (https) download of the html page
$url =~ s/http:\/\//https:\/\//gi if ($forceSSL == 1);

# download the html from the youtube page containing the page title and video
# url. The page title will be used for the local video file name and the url
# will be sanitized and passed to wget for the download.
my $html = `wget -4Ncq --convert-links=off --no-cookies --timeout=20 --user-agent='' --no-check-certificate "$url" -O-`  or die  "\nThere was a problem downloading the HTML page.\n\n";

# format the title of the page to use as the file name
my ($title) = $html =~ m/<title>(.+)<\/title>/si;
$title =~ s/[^\w\d]+/_/g or die "\nError: we could not find the title of the HTML page. Check the URL.\n\n";
$title =~ s/_youtube//ig;
$title =~ s/^_//ig;
$title = lc ($title);
$title =~ s/_amp//ig;

# filter the URL of the video from the HTML page
my ($download) = $html =~ /"url_encoded_fmt_stream_map"(.*)/ig;

# Print all of the separated strings in the HTML page
#print "\n$download\n\n" if ($DEBUG == 1);

# This is where we look through the HTML code and select the file type and
# video quality. 
my @urls = split(',', $download);
OUTERLOOP:
foreach my $val (@urls) {
#   print "\n$val\n\n";

    if ( $val =~ /$fileType/ ) {
       my @res = split(',', $resolution);
       foreach my $ress (@res) {
         if ( $val =~ /$ress/ ) {
         print "\n  html to url seperation complete.\n\n" if ($DEBUG == 1);
         print "$val\n" if ($DEBUG == 1);
         $resFile = $ress;
         $resFile = "sd640" if ( $ress =~ /itag=18/ );
         $resFile = "mobil176" if ( $ress =~ /itag=17/ );
         $download = $val;
         last OUTERLOOP;
         }
       }
    }
}

# clean up the url by translating unicode and removing unwanted strings
print "\n  Re-formatting url for wget...\n\n" if ($DEBUG == 1);
$download =~ s/\:\ \"//;
$download =~ s/%3A/:/g;
$download =~ s/%2F/\//g;
$download =~ s/%3F/\?/g;
$download =~ s/%3D/\=/g;
$download =~ s/%252C/%2C/g;
$download =~ s/%26/\&/g;
$download =~ s/sig=/signature=/g;
$download =~ s/\\u0026/\&/g;
$download =~ s/(type=[^&]+)//g;
$download =~ s/(fallback_host=[^&]+)//g;
$download =~ s/(quality=[^&]+)//g;

# clean up the url 
my ($youtubeurl) = $download =~ /(http?:.+)/;

# url title additon
my ($titleurl) = $html =~ m/<title>(.+)<\/title>/si;
$titleurl =~ s/ - YouTube//ig;
$titleurl =~ s/ /%20/ig;

# combine the youtube url and title string
$download = "$youtubeurl\&title=$titleurl";

# a bit more cleanup as youtube 
#$download =~ s/&+/&/g;
#$download =~ s/&itag=\d+&signature=/&signature=/g;

# combine file variables into the full file name
my $filename = "unknown";
if ( $resolutionFilename == 1 ) {
   $filename = "$prefix$title\_$resFile.$fileType";
  } else {
   $filename = "$prefix$title.$fileType";
}

# Process check: Are we currently downloading this exact same video? Two of the
# same wget processes will overwrite themselves and corrupt the video.
my $running = `ps auwww | grep [w]get | grep -c "$filename"`;
print "\n  Is the same file already being downloaded? $running\n" if ($DEBUG == 1);
if ($running >= 1)
  {
   print "\n  Already $running process, exiting." if ($DEBUG == 1);
   exit 0;
  };

# Force SSL (https) download of the video file.
$download =~ s/http:\/\//https:\/\//g if ($forceSSL == 1);

# Print the long, sanitized youtube url for testing and debugging
print "\n  The following url will be passed to wget:\n\n" if ($DEBUG == 1);
print "\n$download\n" if ($DEBUG == 1);

# print the file name of the video being downloaded for the user 
print "\n Download: $filename\n\n" if ($retryCounter < 1);

# Background the script before wget starts downloading. Use "ps" if you need to
# look for the process running or use "ls -al" to look at the file size and
# date.
fork and exit;

# Download the video 
system("wget", "-4Ncq", "--convert-links=off", "--no-cookies", "--timeout=20", "--no-check-certificate", "--user-agent=''" , "$download", "-O", "$filename");

# Print the error code of wget
print "\n  wget error code: $?\n" if ($DEBUG == 1);

# Exit Status: Check if the file exists and we received the correct error code
# from wget system call. If the download experienced any problems the script
# will run again and try continue the download until the retryTimes count limit
# is reached.

if( $? == 0 && -e "$filename" && ! -z "$filename" )
   {
      print "\n  Finished: $filename\n\n" if ($DEBUG == 1);
    # print "\n  Success: $filename\n\n";
      $retry = 0;
   }
 else
   {
      print STDERR "\n  FAILED: $filename\n\n" if ($DEBUG == 1);
    # print "\n  FAILED: $filename\n\n";
      $retry = 1;
      $retryCounter++;
    # sleep $retryCounter;
      sleep 1;
   }
}

#### EOF #####
