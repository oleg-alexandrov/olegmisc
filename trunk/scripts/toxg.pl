#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use POSIX;
undef $/;          # read one whole file in one scalar

MAIN:{
  
  my $ctx = 0;
  my $cty = 0;
  my $layer = "";
  my $metaFile = "meta.txt";
  if ( -e $metaFile){
    open(FILE, "<$metaFile") || die "Cannot open file $metaFile $!";
    my $meta = <FILE>;
    close(FILE);
    if ($meta =~ /center\s+(.*?)\s+(.*?)\n/){
      $ctx = $1;
      $cty = $2;
    }
    if ($meta =~ /svg file\s*(.*?)\n/ && scalar(@ARGV) < 2 ){
      @ARGV = ($1, @ARGV);
    }
    if ($meta =~ /layer(\s*;\s*\d+\s*:\s*\d+)/ ){
      $layer = $1;
    }
  }
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 file.svg file.xg\n";
    exit(0);
  }

  my $file_svg = $ARGV[0];
  open(FILE, "<$file_svg"); my $text = <FILE>; close(FILE);

  my $out = "color = red\n";
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
      $vert = ( floor($v1 + 0.5) + $ctx ) . " " . ( -floor($v2 + 0.5) + $cty ) . $layer;
      push(@verts, $vert);
    }
    $line = join("\n", @verts);
    
    $out .= $line . "\nNEXT\n"
    
  }

  my $file_xg = $ARGV[1];
  open(FILE, ">$file_xg"); print FILE $out; close(FILE);

  print "Reading $file_svg\n";
  print "Writing $file_xg\n";
  
}

