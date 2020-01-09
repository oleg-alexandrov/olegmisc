#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use POSIX qw/ceil/;

MAIN:{

  # Prepend a line to a file
  if (scalar(@ARGV) < 1){
    print "Usage: file.txt num \n";
    exit(0);
  }

  my $in = $ARGV[0];
  my $num = $ARGV[1];
  my $preamble;
  if (scalar(@ARGV) > 2){
    $preamble = $ARGV[2];
  }
  
  print "in and num are $in $num $preamble\n";

  open(FILE, "<$in");
  my @lines = <FILE>;
  close(FILE);

  my @pre = ();
  if ($preamble != ""){
    open(FILE, "<$preamble");
    @pre = <FILE>;
    close(FILE);
  }
  
  my $total = scalar(@lines);
  print "Total is $total\n";
  
  my $num_batches = ceil ($total * 1.0 / $num);
  print "num of jobs is $num_batches\n";

  for (my $i = 0; $i < $num_batches; $i++){
    my $out = $in . "_part$i.txt";
    print "Writing $out\n";
    my $beg = $i * $num;
    my $end = $i * $num + $num;
    if ($end > $total){
      $end = $total;
    }

    open(FILE, ">$out");
    foreach my $line (@pre){
      print FILE "$line";
    }
    
    for (my $j = $beg; $j < $end; $j++){
      print FILE "$lines[$j]";
    }
    
    close(FILE);
  }
  
}
