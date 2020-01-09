#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

sub round {
  return int($_[0]+0.5);
}

MAIN:{

  # Read some numbers from stdin and print the percentiles

  my @vals = ();
  
  foreach my $line (<STDIN>){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    next if ($line =~ /^\s*$/);
    
    push(@vals, $line);
  }

  @vals = sort {$a <=> $b} @vals;
  my $len = scalar(@vals);

  my $ind25 = round($len * 0.25) - 1;
  my $ind50 = round($len * 0.50) - 1;
  my $ind75 = round($len * 0.75) - 1;
  my $ind80 = round($len * 0.80) - 1;
  my $ind90 = round($len * 0.90) - 1;
  
  print "percentiles\%: 25\%: $vals[$ind25] 50\%: $vals[$ind50] 75\%: $vals[$ind75] 80\%: $vals[$ind80] 90\%: $vals[$ind90]\n";
  
}
