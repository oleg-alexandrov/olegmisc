#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# Exclude from first file the entires in the second file and write the remaining one to third file.
MAIN:{

  # Exclude from file these lines. Line numbers start from 0.
  if (scalar(@ARGV) < 3){
    print "Usage: cat myfile.txt | $0 <list of lines>\n";
    exit(0);
  }

  my %lines;
  my $count = 0;
  foreach my $arg (@ARGV){
    if ($arg !~ /^\d+$/){
      print "Must specify line numbers.\n";
    }
    $lines{$arg} = $count++;
  }

  $count = -1;
  foreach my $line (<STDIN>){
    $count += 1;
    next if (exists $lines{$count});
    print "$line";
  }
  
}
