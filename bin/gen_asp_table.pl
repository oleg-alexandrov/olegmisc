#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Save to a file the result of running --help on an asp tool. Pass
  # that file as an input argument to this tool. Will format that text
  # in LaTeX for inclusion in the doc. Subsequent tweaks may be
  # necessary.
  # Usage: asp_tool --help | gen_asp_table.pl


  my $text = "";
  foreach my $line (<STDIN>){
    $text .= $line;
  }
  
  # Get the usage
  my $tool = "TOOL";
  if ($text =~ /Usage:(.*?)\n/){
    my $usage = $1;
    $usage =~ s/^.*?\.libs\///g;
    if ($usage =~ /(\w+)\s/){
      $tool = $1;
    }

    print "Usage:\n\\begin\{verbatim}\n  $usage\n\\end\{verbatim\}\n";
  }

  print '
\begin{longtable}{|l|p{7.5cm}|}
\caption{Command-line options for ' . $tool . '}
\label{tbl:' . $tool . '}
\endfirsthead
\endhead
\endfoot
\endlastfoot
\hline
Option & Description \\\\ \hline \hline' . "\n";

  # Get the remaining text
  $text =~ s/^.*?Usage.*?\n.*?-/-/sg;

  # Escape any underscore
  $text =~ s/_/\\_/g;

  my $sep = " zzz sep "; # not very clever
  $text =~ s/\n\s*-/$sep-/g;
  my @lines = split($sep, $text);
  foreach my $line (@lines){

    my ($opt, $val);
    #print "$line\n";
    if ($line =~ /^\s*(-.*?)(\s\s|\)\s)\s*(.*?)\s*$/s){
      # This is fragile
      $opt = $1 . $2;
      $val = $3;
    }else{
      print "Could not match $line\n";
      exit(1);
    }

    $opt =~ s/--/-\\\/-/g;
    $opt =~ s/\]//g;
    $opt =~ s/\[/\|/g;

    $opt = '\texttt{' . $opt . '}';
    $val =~ s/\s+/ /g;
    $val .= '\\\\ \hline' . "\n";
    print "$opt \& $val";

  }

  print '\end{longtable}' . "\n";
}
