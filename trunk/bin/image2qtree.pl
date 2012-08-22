#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw(min max);
MAIN:{

  if ( scalar(@ARGV) < 1 ){
    print "Usage: $0 name files.tif\n";
    exit(1);
  }

  if ( scalar(@ARGV) == 1 ){
    # Manufacture an output name out of the input
    my $name = $ARGV[0];
    $name =~ s/^\.\/*//g;
    $name =~ s/\//_/g;
    $name =~ s/\..*?$//g;
    @ARGV = ($name, @ARGV);
  }
  
  my $name  = shift @ARGV;
  my $files = join(" ", @ARGV);
  my $cmd = "~/visionworkbench/src/vw/tools/image2qtree -m kml $files -o /byss/docroot/oleg/$name";
  print "$cmd\n";
  print qx($cmd) . "\n";

  my $index = "/byss/docroot/oleg/$name/$name.kml";
  open(FILE, "<$index");
  my $text = join("", <FILE>);
  close(FILE);

  $text =~ s!(\<kml)\s+(xmlns)!$1 . " hint=\"target=earth\" " . $2!e; 
  $text =~ s!\>([^\<\>]*?\.(?:kml|png))!>https://byss.arc.nasa.gov/oleg/$name/$1!g;

  open(FILE, ">$index");
  print FILE "$text\n";
  close(FILE);

  my $address = "https://byss.arc.nasa.gov/oleg/$name/$name.kml";
  print "$address\n";

  $address = "<p> <a href=\"$address\">$address</a><br>\n";
  print "$address\n";
  my $list = "/byss/docroot/oleg/list.html";
  $cmd = "tac $list > tmpFile.txt; echo '$address' >> tmpFile.txt; tac tmpFile.txt > $list";
  qx ($cmd);
  
  print "List is at: https://byss.arc.nasa.gov/oleg/list.html\n";
  
}
