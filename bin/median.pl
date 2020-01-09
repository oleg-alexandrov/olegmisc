#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  my @vals;
  
  foreach my $line (<STDIN>){
    $line =~ s/\s//g;
    $line =~ s/[,:]//g;
    if ($line !~ /\d/){
      next;
    }

    push(@vals, $line);
  }

  @vals = sort { $a <=> $b } @vals;
  #print join("\n", @vals) . "\n";

  my $len = scalar(@vals);
  if ($len <= 0){
    print "NaN\n";
  } else{
    my $median = 0;
    if (($len % 2) == 0) {
      $median = ($vals[$len/2] + $vals[($len/2) - 1])/2.0;
    } else {
      $median = $vals[$len/2];
    }
    print "$median\n";
  }
  
}

