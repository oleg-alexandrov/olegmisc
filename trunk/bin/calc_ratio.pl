#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 3){
    print "Usage: $0 file1.txt file2.txt column(>=1)\n";
    exit(0);
  }

  my $file1 = $ARGV[0];
  print "Reading $file1\n";
  open(FILE, "<$file1"); my @lines1 = <FILE>; close(FILE);

  my $file2 = $ARGV[1];
  print "Reading $file2\n";
  open(FILE, "<$file2"); my @lines2 = <FILE>; close(FILE);

  my $column = $ARGV[2] - 1;

  for (my $count  =  0; $count < scalar(@lines1); $count++){
    my @vals1 = split(/\s+/, $lines1[$count]);
    my @vals2 = split(/\s+/, $lines2[$count]);

    my $v1 = $vals1[$column]; $v1 =~ s/[A-Z-a-z]//g;
    my $v2 = $vals2[$column]; $v2 =~ s/[A-Z-a-z]//g;
    my $ratio = 1-$v2/$v1;
    print "$ratio $v1 $v2 $lines1[$count]\n";
  }
}
