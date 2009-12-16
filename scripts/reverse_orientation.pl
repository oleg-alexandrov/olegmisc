#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{

  # Reverse the orientation of a polygon stored in an xg file
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 file.xg \n";
    exit(0);
  }

  my @files = @ARGV;

  foreach my $file (@files){

    print "Reversing the orientation of $file\n";

    open(FILE, "<$file");
    my $text = <FILE>;
    close(FILE);

    my $color = "";
    if ($text =~ /^(.*?color.*?\n)(.*?)$/is){
      $color = $1;
      $text  = $2;
    }

    my @polys = split(/NEXT\s*?\n/, $text);
    
    foreach my $poly (@polys){

      my @vals = split("\n", $poly);
      @vals    = rv_lines(@vals);
      $poly    = join("\n", @vals) . "\n";

    }

    $text = $color . join("NEXT\n", @polys) . "\nNEXT\n";
    
    open(FILE, ">$file");
    print FILE $text;
    close(FILE);
  }
  
}

sub rv_lines{

  my @lines_in  = @_;
  my @lines_out = ();
  
  foreach my $line (@lines_in){
    @lines_out = ($line, @lines_out);
  }

  return @lines_out;
}

