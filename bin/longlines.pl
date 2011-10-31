#!/usr/bin/perl
use strict;		          # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# See if any lines in the fortran code are longer than 72 characters
MAIN:{

  my ($text, $line, $line_no, $file);
  
  $file = $ARGV[0];
  print "Lines in $file that are longer than 72 characters:\n";

  open(FILE, "<$file"); $text = <FILE>; close(FILE);

  $line_no = 0;
  foreach $line (split ("\n", $text)){

    $line_no++;
    
    # strip comments
    $line =~ s/^c.*?$//g;
    $line =~ s/\!.*?$//g;

    # strip whitespace at the end
    $line =~ s/\s*$//g;
    
    if (length ($line) > 72){
      print "$line_no: $line\n";
    }
  }
  
}

