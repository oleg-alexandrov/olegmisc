#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

my $height = 1737400.0;
MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 file.ply\n";
    exit(0);
  }

  my $ply_file = $ARGV[0];
  my $csv_file = $ply_file;
  $csv_file =~ s/\.\w+$/.csv/g;

  if ($ply_file eq $csv_file){
    print "Input is already csv.\n";
    exit(1);
  }
  open(FILE, "<$ply_file");
  open(FILE2, ">$csv_file");
  print "Writing: $csv_file\n";
  foreach my $line (<FILE>){
    if ($line =~ /^[a-z]/i || $line =~ /^\s*$/){
      next;
    }
    
    $line =~ s/^\s*//g;
    my @vals = split(/\s+/, $line);
    
    printf(FILE2 "%0.17g %0.17g %0.17g\n",
           $vals[0], $vals[1], $vals[2] + $height);
  }
  close(FILE);
  close(FILE2);
}
