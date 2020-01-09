#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw(max);
undef $/;          # read one whole file in one scalar

MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 spreadSheet1.csv spreadSheet2.csv ...\n";
    exit(0);
  }

  my @files = @ARGV;
  my @data = ();

  my $file;
  my $max_num_rows = 0;
  foreach $file (@files){
    open(FILE, "<$file"); my @sp = split("\n", <FILE>); close(FILE);
    $max_num_rows = scalar (@sp) if (scalar(@sp) > $max_num_rows);
    push(@data, \@sp);
  }

  # print each row
  for (my $count = 0; $count < $max_num_rows; $count++){
    foreach my $ptr (@data){
      if (scalar (@$ptr) < $count + 1){
        print ",";
      }else{
        print $ptr->[$count] . ",";
      }
    }
    print "\n"; # Go to the next row
  }
  
}

