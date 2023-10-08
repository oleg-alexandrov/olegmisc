#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# Swap two strings
MAIN:{

  if (scalar(@ARGV) < 2){
    print "Usage: $0 val1 val2\n";
    exit(0);
  }

  my $a = $ARGV[0];
  my $b = $ARGV[1];
  #print "vals are $a $b\n";

  foreach my $line (<STDIN>){
    #print "line is $line\n";
    my $rare = ' ____xDFAAFAF___ ';
    $line =~ s/$a/$rare/g;
    $line =~ s/$b/$a/g;
    $line =~ s/$rare/$b/g;
    print "$line";
  }
  
}
