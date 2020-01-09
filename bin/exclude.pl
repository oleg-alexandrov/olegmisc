#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# Exclude from first file the entires in the second file and write the remaining one to third file.
MAIN:{
  
  if (scalar(@ARGV) < 3){
    print "Usage: $0 file1.txt file2.txt file3.txt\n";
    exit(0);
  }

  my $file1 = $ARGV[0];
  open(FILE, "<$file1");
  my @lines1 = <FILE>;
  close(FILE);

  my $file2 = $ARGV[1];
  open(FILE, "<$file2");
  my @lines2 = <FILE>;
  close(FILE);

  my %hash;
  foreach my $line (@lines2){
    $hash{$line} = $1;
  }

  my $file3 = $ARGV[2];
  open(FILE, ">$file3");
  foreach my $line (@lines1){
    if (exists $hash{$line}){
      next;
    }
    print FILE "$line";
  }

}
