#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{

  # Change the color of an xg file
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 new_color file.xg \n";
    exit(0);
  }

  my $col   = shift @ARGV;
  my @files = @ARGV;

  foreach my $file (@files){
    print "$file: color = $col\n";

    open(FILE, "<$file");
    my $text = <FILE>;
    close(FILE);

    if ($text !~ /color/i){
      $text = "color = $col\n" . $text;
    }else{
      $text =~ s/color.*?\w+/color = $col/i;
    }
    
    open(FILE, ">$file");
    print FILE $text;
    close(FILE);
  }
  
}
