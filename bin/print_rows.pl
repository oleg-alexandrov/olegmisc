#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Print the specified rows. Rows start from 1.

  if (scalar(@ARGV) < 1){
    print "Usage: cat someting | $0 rowNumbers\n";
    exit(0);
  }

  # See if instead of printing a set of columns we should instead exclude
  # a set of columns.
  #my $do_exclude = 0;
  #if ($ARGV[0] eq '-v'){
  #  $do_exclude = 1;
  #}

  my %rows;
  my $count = 0;
  foreach my $arg (@ARGV){
    next unless ($arg =~ /^\-*\d+$/);
    $rows{$arg} = $count++;
  }

  $count = 0;
  foreach my $line (<STDIN>){

    next if ($line =~ /^\s*$/);
    $count++;
    next unless (exists $rows{$count});

    print "$line";
  }

}
