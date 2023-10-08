#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# Extract medians from a report output by bundle_adjust's
# run-final_residuals_no_loss_function_raw_pixels.txt

MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 fileName.txt\n";
    exit(0);
  }

  open(FILE, "<$ARGV[0]");
  my $text = join(",", <FILE>);
  close(FILE);

  $text =~ s/\n//g;

  my $sep = "XXfdAA";
  $text =~ s/M/$sep . "M"/eg;
  
  my @lines = split($sep, $text);

  foreach my $line (@lines){
    $line =~ s/\s*,\s*/,/g;
    my @vals = split(",", $line);
    my $len = scalar(@vals);
    #print "$line\n";
    if ($len < 2){
      next;
    }
    my $name = $vals[0];

    my @norm;
    
    @vals = splice (@vals, 2, $len - 3);
    $len = scalar(@vals);
    my $half = int($len/2);
    
    #print "$name $len\n";
    #print "half is $half\n";

    #if (2*$half != $len){
      #print "mismatch!\n";
      #exit(1);
    #}
    for (my $i = 0; $i < $half; $i++){
      my $x = $vals[2*$i+0];
      my $y = $vals[2*$i+1];
      
      my $d = sqrt($x*$x + $y*$y);
      #print "$d\n";
      push (@norm, $d);
    }

    @norm = sort { $a <=> $b } @norm;
    $len = scalar(@norm);

    foreach my $d (@norm){
      #print "$d\n";
    }
    
    my $median = 0;
    if ($len > 0){
      $median = $norm[$len/2];
    }

    print "$name $len $median\n";
    
    #print "$line\n";
  }
}
