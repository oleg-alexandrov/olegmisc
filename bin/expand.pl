#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 4){
    print "Usage: $0 train.txt test.txt query.txt out.txt\n";
    exit(0);
  }

  my $train_file = $ARGV[0];
  my $test_file = $ARGV[1];
  my $query_file = $ARGV[2];
  my $out_file = $ARGV[3];

  my @train;
  open(FILE, "<$train_file");
  foreach my $line (<FILE>){
    chomp($line);
    $line =~ s/key/jpg/g;
    #$line =~ s/^.*?\///g;
    push(@train, $line);
  }
  close(FILE);

  my @test;
  open(FILE, "<$test_file");
  foreach my $line (<FILE>){
    chomp($line);
    $line =~ s/key/jpg/g;
    #$line =~ s/^.*?\///g;
    push(@test, $line);
  }
  close(FILE);

  my @query;
  open(FILE, "<$query_file");
  foreach my $line (<FILE>){
    chomp($line);
    push(@query, $line);
  }
  close(FILE);

  print "Writing: $out_file\n";

  open(FILE, ">$out_file");
  foreach my $line (@query){
    next unless ($line =~ /^(\d+)\s+(\d+)\s+(.*?)$/);
    print FILE "$test[$1] $train[$2] $3\n";
  }
  close(FILE);

}
