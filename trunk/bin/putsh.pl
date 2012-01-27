#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# use main to avoid the curse of global variables
MAIN: {

  # put #!/bin/bash at the top of a code
  
  if (scalar @ARGV < 1){
    print "Usage: $0 filename\n";
  }

  my $file = $ARGV[0];
  open(FILE, "<$file") || die "Cannot open $file\n";
  my $text = <FILE>;
  close(FILE);

  $text =~ s/\#.*?\n//g;
  $text =~ s/^\s*//g;
  $text =~ s/\s*$//g;
  
  $text = '#!/bin/bash' . "\n" . $text;

  open(FILE, ">$file");
  print FILE "$text\n\n";
  close(FILE);

  system ("chmod a+x $file");
  
}
: 
