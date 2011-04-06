#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{

  my $data = qx(cvs_status.pl);
  my @lines = split("\n", $data);

  foreach my $line (@lines){
    print "$line\n";
    next unless ($line =~ /^File:\s+([^\s]+)(\s+)([^\s].*?)$/);
    my $file   = $1;
    my $status = $3;
    next unless ($status =~ /Locally Modified/);
    qx(tkdiff $file);
  }

}
