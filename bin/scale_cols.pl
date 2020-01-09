#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  # Scale given columns (column index starts from 1 by given number).
  # Overwrite the file.

  if (scalar(@ARGV) < 3){
    print "Usage: $0 file scale cols\n";
    exit(0);
  }

  my $file = $ARGV[0]; shift @ARGV;
  open(FILE, "<$file");
  my @lines = <FILE>;
  close(FILE);

  my $scale = $ARGV[0]; shift @ARGV;

  open(FILE, ">$file");
  foreach my $line (@lines){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    my @vals = split(/\s+/, $line);
    foreach my $ind (@ARGV){
      if (scalar(@vals) >= $ind){
        print "scaling col: $ind $vals[$ind-1] by $scale\n";
        $vals[$ind-1] *= $scale;
      }
    }
    $line = join(" ", @vals);
    print FILE "$line\n";
  }
  close(FILE);
}
