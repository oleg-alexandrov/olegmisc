#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Pre-process a message before writing it.
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 fileName\n";
    exit(0);
  }

  my $file = $ARGV[0];

  my $text = "";
  if ( -e $file){
    open(FILE, "<$file");
    $text = join("", <FILE>);
    close(FILE);
  }
  
  
  if ($text =~ /^.*?Hi .*?(On.*?at .*?:.*?:.*?,\s+\w+,\s+)(\w+)(.*?)$/s ){
    $text = "Hi $2,\n\n$1$2$3";
  }

  open(FILE, ">$file");
  print FILE $text;
  close(FILE);
  
}
