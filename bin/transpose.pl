#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw(max);

# Take a table of values and transpose it so rows becomes columns
MAIN:{

  if (scalar(@ARGV) < 2){
    print "Usage: $0 input.txt output.txt\n";
    exit(0);
  }

  my $fileIn  = $ARGV[0];
  my $fileOut = $ARGV[1];

  my @data = ();

  print "Reading $fileIn\n";
  open(FILE, "<$fileIn");
  my @lines = <FILE>;
  close(FILE);

  print "Writing: $fileOut\n";
  open(FILE, ">$fileOut");
  
  my $num_cols = 0;
  foreach my $line (@lines){
    $line =~ s/[\[\]]//g;
    $line =~ s/,/ /g;
    $line =~ s/^\s*//g;
    $line =~ s/\s*\n//g;
    $line =~ s/\s+/ /g; 
    next if ($line =~ /^\s*$/);

    my @vals = split(/\s+/, $line);
    #print "vals are " . join("*", @vals) . "\n";
    my $len = scalar(@vals);
    if ($len > $num_cols){
      $num_cols = $len;
    }
    push(@data, \@vals);
  }

  for (my $col = 0; $col < $num_cols; $col++){
    for (my $row = 0; $row < scalar(@data); $row++){
      my $row_vals = $data[$row];
      my $row_len = scalar(@$row_vals);

      if ($col >= $row_len){
        print FILE "NaN ";
      }else{
        print FILE $row_vals->[$col] . " ";
      }
    }
    print FILE "\n";
  }
  
}

