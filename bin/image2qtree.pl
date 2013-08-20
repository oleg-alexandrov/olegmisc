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

  if ( scalar(@ARGV) < 1 ){
    print "Usage: $0 name files.tif\n";
    exit(1);
  }

  maybe_call_itself_on_remote_host(@ARGV);

  if ( scalar(@ARGV) == 1 ){
    # Manufacture an output name out of the input
    @ARGV = ($ARGV[0], @ARGV);
  }

  my $name = shift @ARGV;
  $name =~ s/^\.\/*//g;
  $name =~ s/\//_/g;
  $name =~ s/\.\w+\s*$//g; # rm filename extension
  $name =~ s#\.#_#g;

  my $first = $ARGV[0];
  my $target = "earth";
  my $stats = qx(gdalinfo -stats $first);
  if ($stats =~/spheroid\s*\[\"moon/i){
    $target = "moon";
    #$target = "mars"; # GE sucks in Moon mode
  }
  if ($stats =~/mars/i){
    $target = "mars";
  }
  print "Target is $target\n";

  my $user = qx(whoami); $user =~ s/\s*$//g;

  my $files = join(" ", @ARGV);
  qx(mkdir -p /byss/docroot/$user/);
  my $cmd = "image2qtree -m kml $files -o /byss/docroot/$user/$name";
  print "$cmd\n";
  print qx($cmd 2>&1 | grep -v Warn) . "\n";

  my $index = "/byss/docroot/$user/$name/$name.kml";
  open(FILE, "<$index");
  my $text = join("", <FILE>);
  close(FILE);

  $text =~ s!(\<kml)\s+(xmlns)!$1 . " hint=\"target=$target\" " . $2!e;
  $text =~ s!\>([^\<\>]*?\.(?:kml|png))!>https://byss.arc.nasa.gov/$user/$name/$1!g;

  open(FILE, ">$index");
  print FILE "$text\n";
  close(FILE);

  my $address = "https://byss.arc.nasa.gov/$user/$name/$name.kml";
  print "$address\n";

  $address = "<p> <a href=\"$address\">$address</a><br>\n";
  #print "$address\n";
  my $list = "/byss/docroot/$user/list.html";
  $cmd = "tac $list > tmpFile.txt; echo '$address' >> tmpFile.txt; tac tmpFile.txt > $list";
  qx ($cmd);

  print "List is at: https://byss.arc.nasa.gov/$user/list.html\n";

}
