#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use POSIX;
undef $/;          # read one whole file in one scalar

MAIN:{
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 file.svg file.xg\n";
    exit(0);
  }

  my $file_svg = $ARGV[0];
  open(FILE, "<$file_svg"); my $text = <FILE>; close(FILE);

  my $ctx = 0;
  my $cty = 0;
  my $ctFile = "center.xg";
  if ( -e $ctFile){
    open(FILE, "<$ctFile") || die "Cannot open file $ctFile $!";
    my $ct = <FILE>;
    close(FILE);
    if ($ct !~ /^(.*?)\s+(.*?)\n/){
      print "Error reading the center from $ctFile\n";
      exit(0);
    }
    $ctx = $1;
    $cty = $2;
  }
  
  my $out = "color=red\n";
  foreach my $line (split("\n", $text)){
    next unless ($line =~ /\bd=\"(.*?)\"/s);
    $line = $1;
    $line =~ s/,/ /g;
    $line =~ s/\s*L\s*/\n/g;
    $line =~ s/\s*M\s*/\n/g;
    $line =~ s/\s*z\s*/\n/g;
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    
    my @verts;
    foreach my $vert (split("\n", $line)){
      next unless ($vert =~ /^([^\s]*?)\s+([^\s]*?)($|\s)/);
      my $v1 = $1; my $v2 = $2;
      if ( $v1 !~ /^[\d\.\-]*$/ || $v2 !~ /^[\d\.\-]*$/ ){
        print "Error!\n";
        print "$line\n";
      }
      $vert = floor($v1 + $ctx+0.5) . " " . floor($v2 + $cty+0.5);
      push(@verts, $vert);
    }
    $line = join("\n", @verts);
    
    $out .= $line . "\nNEXT\n"
    
  }

  my $file_xg = $ARGV[1];
  open(FILE, ">$file_xg"); print FILE $out; close(FILE);
  
}

