#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 2){
    print "Usage: $0 file_in file_out\n";
    exit(0);
  }

  my $file_in = $ARGV[0];
  my $file_out = $ARGV[1];
  
  open(FILE, "<$file_in");
  my @lines_orig = <FILE>;
  close(FILE);
  my @lines = @lines_orig;

  foreach my $line (@lines) {
    $line =~ s/\n//g;
    $line =~ s/8\d\.\.\.//g;
    $line =~ s/ 0/ /g;
    $line =~ s/^\*\d+\+//g;
    my @vals = split(/\s+/, $line);
    next unless (scalar(@vals) >= 4);
    my $id = int($vals[0]);
    my $x = int($vals[1])/1000.0;
    my $y = int($vals[2])/1000.0;
    my $z = int($vals[3])/1000.0;
    $line = "$x $y $z # id: $id";
  }

  my $text = "";
  foreach my $line (@lines) {
    $text .= "$line\n";
  }

  print "Writing: $file_out\n";
  open(FILE, ">$file_out");
  print FILE "$text";
  close(FILE);
}
