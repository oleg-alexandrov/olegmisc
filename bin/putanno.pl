#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# A small script which takes an xg file and prints a copy of it
# annotating the vertices of each polygon
# (first vertex gets annotated as 1, second as 2, etc.)
MAIN: {

  if (! @ARGV){
    print "Usage: $0 inputPoly.xg\n";
    exit(0);
  }
  
  my ($text, @polys, $line, $ctr, $vert_count, @lines);
  open(FILE, "<$ARGV[0]");
  $text = <FILE>;
  close(FILE);

  $text =~ s/\nanno.*?\n/\n/g; # strip existing annotations
  @lines = split ("\n", $text);

  $vert_count = 1;
  
  for ($ctr = 0; $ctr < scalar(@lines); $ctr++){

    $line = $lines[$ctr];
    
    if ($line =~ /next/i){
      
      # if at a "NEXT" line, reset the vertices counter
      $vert_count = 1;
      next;
      
     }

    if ( ( $ctr < $#lines ) && ($lines[$ctr+1] !~ /next/i ) ){

      # don't put an annotation at the last vertex, as that's just the
      # first one repeated
      next unless ($line =~ /^\s*([\d\-\+\.]+\s+[\d\-\+\.]+)/);
      $lines[$ctr] = $line . "\n" . "anno " . $1 . " " . $vert_count;
      $vert_count++;
      
    }
  }

  $text = join ("\n", @lines);
  
  my $outfile ="Annotated.xg";
  print "Writng annotated file to $outfile\n";
  open(FILE, ">$outfile");
  print FILE $text . "\n";
  close(FILE);

  #system("xgraph -ng -geometry 1000x800+0+0 Annotated.xg")
}
