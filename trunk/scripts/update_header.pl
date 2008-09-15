#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# Extract C++ function information from a .cpp file and
# dump to corresponding .h file.

MAIN: {

  print "hi\n";

  &parse_header();

}

sub parse_header (){

  
}
  
