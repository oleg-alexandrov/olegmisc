#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar
use lib '/home/olegalex/bin';

require 'find_bdbox.pl';

MAIN:{
  
  if (scalar(@ARGV) < 3){
    print "Usage: $0 in.xg bias out.xg\n";
    exit(0);
  }

  my $file_in  = $ARGV[0];
  my $bias     = $ARGV[1];
  my $file_out = $ARGV[2];

  open(FILE, "<$file_in");
  my $text = <FILE>;
  close(FILE);
  
  open(FILE, ">$file_out");
  print FILE "color = red\n";

  my @polys = split("NEXT", $text);
  foreach my $poly (@polys){
    
    next unless ($poly =~ /[e\-\+\.\d]+\s+[e\-\+\.\d]+/);
    my ($xll, $yll, $xur, $yur) = &find_bdbox($poly);
    $xll -= $bias; $xur += $bias;
    $yll -= $bias; $yur += $bias;

    print FILE "$xll $yll\n";
    print FILE "$xur $yll\n";
    print FILE "$xur $yur\n";
    print FILE "$xll $yur\n";
    print FILE "$xll $yll\n";
    print FILE "NEXT\n";
  }
  
  close(FILE);
  
}
