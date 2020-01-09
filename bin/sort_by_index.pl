#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# for images like: GR_20120330/processed/ba_fix/batch_990_992_3/out-DEM.tif
# sort by 990, so that say, image 9 shows up before image 12.

MAIN:{

  if (scalar(@ARGV) < 0){
    print "Usage: cat file.txt | $0 \n";
    exit(1);
  }

  my (%hash1, %hash2);
  my $count = 0;
  foreach my $line (<STDIN>){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g; # strip any newline (last line may not have one)
    my @vals = split(/_/, $line);
    my $sum = 0;
    foreach my $val (@vals){
      next unless ($val =~ /^\d+$/);
      $sum += int($val);
    }

    # Need the count to not collapse unique lines
    my $key = $line . "___" . "$sum";
    $hash1{$key} = $sum;
    $hash2{$key} = $line;
    $count++;
  }

  foreach my $line ( sort { $hash1{$a} <=> $hash1{$b} } keys %hash1 ){
    print $hash2{$line} . "\n";
  }
}

