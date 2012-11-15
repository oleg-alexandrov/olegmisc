#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw(min max);
use File::Basename;
use File::Spec;
use Cwd;
use lib dirname(File::Spec->rel2abs($0));
require 'make_kml_utils.pl';

MAIN:{

  if ( scalar(@ARGV) < 2 ){
    print "Usage: $0 output_kml input_kmls\n";
    exit(1);
  }

  my $output = shift @ARGV;
  my $list = join(" ", @ARGV);
  $list =~ s/^\s*//g;
  $list =~ s/\s*$//g;
  $list =~ s/\s+/ /g;
  @ARGV = split(" ", $list);
  
  create_combined_kml($output, \@ARGV);
  
}
