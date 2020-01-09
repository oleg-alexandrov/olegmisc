#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 2){
    print "Print lines common in file1 and file2. Usage: $0 file1 file2\n";
    exit(0);
  }
  my $file1 = shift @ARGV;
  my $file2 = shift @ARGV;

  open(FILE, "<$file1"); my @lines1 = <FILE>; close(FILE);
  open(FILE, "<$file2"); my @lines2 = <FILE>; close(FILE);

  my %hash2;
  foreach my $line (@lines2){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    $hash2{$line} = 1; 
  }

  foreach my $line (@lines1){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    if (exists $hash2{$line}){
      print "$line\n";
    }
  }
  
}
