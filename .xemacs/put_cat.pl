#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings

undef $/; # undefines the separator. Can read one whole file in one scalar.

my $text=<>;
$text =~ s/\s*$//g;
if ($text =~ /^In\s+\[\[(.*?)\]\]/){
  
  $text = $text . "\n[[Category:$1]]";
}else{
  $text = $text . "\n[[Category:]]";
}

print "$text\n";
