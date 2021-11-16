#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use POSIX;

# shift a set of columns by a given amount in each columns
MAIN: {

  if (scalar (@ARGV) < 4  ){
    print "Usage: input.txt out.txt shift_to_add\n";
    exit(0);
  }

  my $in   = shift @ARGV;
  my $out  = shift @ARGV;

  my @shift = @ARGV;
  
  open(FILE, "<$in") || die "Can't open file $in";
  my @lines = <FILE>; 
  close(FILE);

  my $index = 0;
  my $total = scalar(@lines);
  
  open(FILE, ">$out");

  foreach my $line (@lines){
    $line =~ s/\n//g;
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    $line =~ s/,/ /g;
    $line =~ s/\s+/ /g;
    
    if ($line =~ /^\s*$/){
      next;  
    }
    
    my @vals = split(/\s/, $line);

    print "doing line $index/$total\n";
    
    if (scalar(@vals) != scalar(@shift)){
      print "Wrong number of shifts. Line is $line with " . scalar(@vals) . " elements.\n";
      exit(1);
    }

    my $count = 0;
    foreach my $val (@vals){
      $val += $shift[$count];
      $count++;
      print FILE "$val ";
    }
    print FILE "\n";

    $index++;
  }

  close(FILE);
    
}
