#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use POSIX;

# Scale a polygon by a given amount
MAIN: {

  if (scalar (@ARGV) < 4  ){
    print "Usage: input.txt out.txt shift_to_subtr\n";
    exit(0);
  }

  my $in   = shift @ARGV;
  my $out  = shift @ARGV;

  my @shift = @ARGV;
  
  open(FILE, "<$in") || die "Can't open file $in";
  my @lines = <FILE>; 
  close(FILE);

  open(FILE, ">$out");

  foreach my $line (@lines){
    $line =~ s/\n//g;
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    
    if ($line =~ /^\s*$/){
      next;  
    }
    
    my @vals = split(/\s/, $line);

    if (scalar(@vals) != scalar(@shift)){
      print "Wrong number of shifts\n";
      exit(1);
    }

    my $count = 0;
    foreach my $val (@vals){
      $val += $shift[$count];
      $count++;
      print FILE "$val ";
    }
    print FILE "\n";
    
  }

  close(FILE);
    
}
