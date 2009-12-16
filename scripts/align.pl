#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings

undef $/; # undefines the separator. Can read one whole file in one scalar.

MAIN:{

  # Align paragraphs to given tag. For example, transofrm
  # int a = 5;
  # double q = 6;
  # into
  # int a    = 5;
  # double q = 6;
  
  my $tag   = shift @ARGV;
  my @files = @ARGV;

  print "Tag is '$tag'\n";

  foreach my $file (@files){

    next if (-d $file);
    
    open(FILE, "<$file"); my $text = <FILE>; close(FILE);

    my $endspace;
    next unless ($text =~ /(\s*)$/);
    $endspace = $1;
    
    $text = do_align($tag, $text);

    $text =~ s/\s*$//g;
    $text = $text . $endspace;
    
    open(FILE, ">$file"); print FILE $text; close(FILE);
  }
}

sub do_align{

  my $tag  = shift;
  my $text = shift;

  my @lines = split("\n", $text);
  my ($beg, $end, $k, $numLines, $maxcol);

  $numLines = scalar(@lines);
  
  $beg = 0;

  while ($beg < $numLines){

    if ($lines[$beg] !~ /\Q$tag\E/){
      $beg++;
      next;
    }
    
    $maxcol = 0;
    $end    = $beg;

    while ($end < $numLines && $lines[$end] =~ /^(.*?)(\s*\Q$tag\E.*?)$/){
      
      $maxcol = length($1) if ( length($1) > $maxcol );
      $end++;
      
    }
    $end--;

    for ($k = $beg; $k <= $end; $k++){
      
      next unless ($lines[$k] =~ /^(.*?)(\s*\Q$tag\E.*?)$/);
      my $before = $1;
      my $after  = $2;
      $after     =~ s/^\s*//g;
      
      $before = $before . " " x ($maxcol - length($before) + 1);

      $lines[$k] = $before . $after;
    }
    
    $beg = $end + 1;
    
  }

  $text = join("\n", @lines);
  return $text;
  
}
