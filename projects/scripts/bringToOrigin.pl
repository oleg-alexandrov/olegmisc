#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar
use lib '/home/olegalex/bin';

require 'find_bdbox.pl';

MAIN:{
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 in.xg out.xg\n";
    exit(0);
  }

  my $file_in  = $ARGV[0];
  my $file_out = $ARGV[1];

  open(FILE, "<$file_in");
  my $text = <FILE>;
  close(FILE);

  my ($xll, $yll, $xur, $yur) = &find_bdbox($text);

  my @lines = split("\n", $text);
  foreach my $line (@lines){
    next unless ($line =~ /^([e\-\+\.\d]+)(\s+)([e\-\+\.\d]+)(.*?)$/);
    my $num1 = $1 - $xll;
    my $num2 = $3 - $yll;
    $line = $num1 . $2 . $num2 . $4;
  }

  $text = join("\n", @lines);
  open(FILE, ">$file_out");
  print FILE $text;
  close(FILE);
  
}
