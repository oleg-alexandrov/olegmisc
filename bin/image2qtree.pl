#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw(min max);
use File::Basename;
use File::Spec;
use Cwd;
use lib dirname(File::Spec->rel2abs($0));
require 'utils.pl';

MAIN:{

  set_path();
  
  my $user = qx(whoami); $user =~ s/\s*$//g;
  my $url = "https://byss.arc.nasa.gov/$user";
  my $dir = "/byss/docroot/$user";

  if ( scalar(@ARGV) < 1 ){
    print "Usage: $0 name files.tif\n";
    exit(1);
  }

  # If requested, first scale the image
  if (scalar(@ARGV) >= 2 && $ARGV[0] =~/^\d+$/ ){
    my $pct = shift @ARGV;
    my $last = scalar(@ARGV)-1;
    my $file = $ARGV[$last];
    if ($file =~ /^(.*?)\.(tif|ntf)/i){ # can't handle cubes
      my $filen = "$1" . "_" . "$pct" . "pct.tif";
      my $cmd = "gdal_translate -outsize $pct" . "% " . "$pct" . "% $file $filen";
      print "$cmd\n";
      print qx($cmd) . "\n";
      $ARGV[$last] = $filen;
    }
  }
  
  maybe_call_itself_on_remote_host(@ARGV);
  
  if ( scalar(@ARGV) == 1 ){
    # Manufacture an output name out of the input
    @ARGV = ($ARGV[0], @ARGV);
  }

  my $name = shift @ARGV;
  $name =~ s/^\.\/*//g;
  $name =~ s/\//_/g;
  $name =~ s/\.[a-z]+\s*$//ig; # rm filename extension
  $name =~ s#\.#_#g;

  my $first = $ARGV[0];
  my $target = "earth";
  print "now will do gdalinfo\n";
  my $ans2 = qx(which gdalinfo);
  print "ans2 is $ans2\n";
  my $stats = qx(gdalinfo -stats $first);
  print "now done\n";
  if ($stats =~/spheroid(\s*\[\"moon|.*?\b1737400\b)/i){
    $target = "moon";
  }
  if ($stats =~/mars/i){
    $target = "mars";
  }
  print "Target is $target\n";

  my $is_float = 0;
  if ($stats =~ /Type=(int16|float)/i ){
    $is_float = 1;
  }
  
  if (scalar (@ARGV) != 1){
    print "ERROR: Expecting just one file!\n";
    exit(1);
  }

  # If a file is float, convert it to uint for display purposes
  my $file = $ARGV[0];
  my $file_int = $file;
  if ($is_float){
    $file_int =~ s/\.\w+$/_int.tif/g;
    qx(~/bin/float2int2.pl $file $file_int);
  }
  
  qx(mkdir -p $dir);
  my $cmd = "image2qtree -m kml $file_int -o $dir/$name";
  print "$cmd\n";
  system($cmd);

  # Make the paths in the index kml absolute, and write it
  # one level up, to make the url shorter.
  my $index = "$dir/$name/$name.kml";
  open(FILE, "<$index");
  my $text = join("", <FILE>);
  close(FILE);
  print "index is $index\n";

  # Note that we write to a new file
  my $index2 = "$dir/$name.kml";
  print "Writing: $index2\n";
  # for some reason this line prevents DEMs from displaying
  #$text =~ s!(\<kml)\s+(xmlns)!$1 . " hint=\"target=$target\" " . $2!e;
  $text =~ s!\>([^\<\>]*?\.(?:kml|png))!>$url/$name/$1!g;
  open(FILE, ">$index2");
  print FILE "$text\n";
  close(FILE);

  my $address = "$url/$name.kml";
  print "$address\n";

  $address = "<p> <a href=\"$address\">$address</a><br>\n";
  #print "$address\n";
  my $list = "$dir/list.html";
  $cmd = "tac $list > tmpFile.txt; echo '$address' >> tmpFile.txt; tac tmpFile.txt > $list";
  qx ($cmd);

  print "List is at: $url/list.html\n";

}
