#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Scalar::Util;

MAIN:{

  # Sort lines by given column. Column index starts from 1.
  # Column 0 is the last one. Column -1 is second from last.
  # Columns are separated by spaces or commas.
  
  if (scalar(@ARGV) < 1){
    print "Usage: cat file.txt | $0 colNum\n";
    exit(1);
  }
  my $colNum = $ARGV[0];

  my (%hash1, %hash2);
  my $count = 0;
  foreach my $line (<STDIN>){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g; # strip any newline (last line may not have one)

    # To use comma as separator
    my $line2 = $line;
    $line2 =~ s/,//g;
    my @vals = split(/\s+/, $line2);

    if (scalar(@vals) < $colNum){
      print "Error: Not enough elements on line: $line\n";
      next;
    }

    my $index = $colNum;
    $index = $index - 1; # make the index start from 0

    # if to start from the other end
    if ($index < 0){
      $index = $index + scalar(@vals);
    }

    # Need the count to not collapse unique lines
    my $key = $line . "___" . "$count";

    my $val = $vals[$index];
    if (!Scalar::Util::looks_like_number($val) || $val =~ /nan/i || $val =~ /inf/i){
      print "Skip non-number value: $val\n";
      next;
    }
    
    $hash1{$key} = $val;
    $hash1{$key} =~ s/[\s,]//g;
    $hash2{$key} = $line;
    $count++;
  }

  foreach my $line ( sort { $hash1{$a} <=> $hash1{$b} } keys %hash1 ){
    print $hash2{$line} . "\n";
  }
}
