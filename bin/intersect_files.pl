#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 file1 file2\n";
    exit(0);
  }

  my $file1 = $ARGV[0];
  open(FILE, "<$file1"); my @lines1 = <FILE>; close(FILE);

  my $file2 = $ARGV[1];
  open(FILE, "<$file2"); my @lines2 = <FILE>; close(FILE);
  
  my (%vals1, %vals2);

  my $count = 0;
  foreach my $val (@lines1){
    $vals1{$val} = $count;
    $count++;
  }
  
  foreach my $val (@lines2){
    $vals2{$val} = $count;
    $count++;
  }

  foreach my $key (sort { $vals1{$a} <=> $vals1{$b} } keys %vals1){
    if (exists $vals2{$key}){
      print "$key";
    }
  }
  
}
