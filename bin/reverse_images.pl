#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # given a set of images in the given range, swap the first with the last,
  # second with the one next to the last, etc.
  
  if (scalar(@ARGV) < 4){
    print "Usage: $0 min max vals\n";
    exit(0);
  }
  my $min     = int(shift @ARGV);
  my $max     = int(shift @ARGV);
  my @vals = @ARGV;

  my @out_vals = ();
  
  my $count = 0;
  my $prev_num = -1;
  foreach my $val (@vals){
    next unless ($val =~ /^.*?\/.*?(\d+)\./);
    my $num = int($1);
    #print "num is $val $min $num $max\n";
    if ($num >= $min && $num <= $max){
      print "val is $num $val\n";
      push(@out_vals, $val);
    }
  }


  my $len = scalar(@out_vals);
  for (my $i = 0; $i < $len; $i++){
    my $j = $len - 1 - $i;
    last if ($i >= $j);
    print "Will swap $out_vals[$i] with $out_vals[$j]\n";
    qx(mv $out_vals[$i] tmp.jpg);
    qx(mv $out_vals[$j] $out_vals[$i]);
    qx(mv tmp.jpg $out_vals[$j]);
  }
  
}
