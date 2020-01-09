#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 1){
    print "Usage: $0 file\n";
    exit(0);
  }

  my $file = $ARGV[0];
  open(FILE, "<$file");
  my @lines = <FILE>;
  close(FILE);

  open(FILE, ">$file");
  foreach my $line (@lines){
    $line =~ s/\n//g;
    if ($line =~ /(^.*?)([^\s]*?\"\w)\s+([^\s]*?\"\w)(.*?)$/){
      my $a = $1;
      my $b = $2;
      my $c = $3;
      my $d = $4;
      $line = $a . do_conv($b) . " " . do_conv($c) . $d;
    }
    print FILE "$line\n";
  }
  close(FILE);
}

sub do_conv {
  my $a = shift;
  if ($a =~ /(\d+).*?(\d+).*?([\d\.]+).*?(\w)/){
    my $deg = $1;
    my $min = $2;
    my $sec = $3;
    my $dir = $4;

    my $val = $deg + $min/60.0 + $sec/3600.0;
    if ($dir =~ /S/i || $dir =~ /W/i){
      $val = -$val;
    }
    return $val;
  }
  return "   ";
}
