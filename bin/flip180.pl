#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{
  
  if (scalar(@ARGV) < 4){
    print "Usage: $0 in.xg flipx flipy out.xg\n";
    exit(0);
  }

  my $file_in  = $ARGV[0];
  my $flipx    = $ARGV[1];
  my $flipy    = $ARGV[2];
  my $file_out = $ARGV[3];

  open(FILE, "<$file_in");
  my $text = <FILE>;
  close(FILE);

  my @lines = split("\n", $text);
  foreach my $line (@lines){
    next unless ($line =~ /^([e\-\+\.\d]+)(\s+)([e\-\+\.\d]+)(.*?)$/);
    my $num1 = $flipx*$1;
    my $num2 = $flipy*$3;
    $line = $num1 . $2 . $num2 . $4;
  }

  $text = join("\n", @lines);
  open(FILE, ">$file_out");
  print FILE $text;
  close(FILE);
  
}
