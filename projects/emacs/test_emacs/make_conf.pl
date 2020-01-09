#!/usr/bin/perl
use strict;                # 'strict' insists that all variables be declared
use diagnostics;           # 'diagnostics' expands the cryptic warnings

my @files=`ls ../dot*el`;
my ($in, $out);
`rm -f .emacs .xemacs/*.el  ../xemacs_files.zip  ../xemacs_files.tar.gz`;
undef $/;
my $contents;

foreach $in (@files) {
  $in =~ s/\n//g;

  $out=$in;
  $out =~ s/\.\.\///g;
  $out =~ s/dotemacs.*?$/.xemacs\/init.el/g;
  $out =~ s/dotxemacs_/.xemacs\//g;
  $out =~ s/\d+\.el/\.el/g;

  open (FILE, "<$in"); 
  $contents = <FILE>;
  close(FILE);

  open (FILE, ">>$out"); 
  print FILE "$contents";
  close(FILE); 

  print "$in -> $out\n";

}



print `tar czfv ../xemacs_files.tar.gz  .xemacs/*.el .xemacs/icons/*xpm`;
print `zip ../xemacs_files.zip .xemacs/*el .xemacs/icons/*xpm`;
