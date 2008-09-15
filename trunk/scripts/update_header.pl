#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# Extract C++ function information from a .cpp file and
# use that to update the corresponding .h file.

MAIN: {

  my $text;
  
  open(FILE, "<$ARGV[0]");
  $text = <FILE>;
  close(FILE);

  my %map = &parse_cpp($text);
  
  open(FILE, "<$ARGV[1]");
  $text = <FILE>;
  close(FILE);

  $text = &parse_h($text, \%map);

   open(FILE, ">$ARGV[2]");
   print FILE "$text";
   close(FILE);
  
}


sub parse_cpp {

  my $text = shift;
  my @blocks = &extract_blocks($text);

  my %map;
  
  my $block;
  foreach $block (@blocks){

    #print "----\n$block\n---\n";
    next unless ( $block =~ /(\w+[\s\*\&]+[\w]+\:\:[\w]+)(\s*\(.*?\))\s*\{/s );
    # matches: void  *  myname::myfun ( double x, double y){

    my $key = $1;
    my $fun = $1 . $2 . ";\n\n";

    $key =~ s/\w+:://g;
    $fun =~ s/\w+:://g;

    $key =~ s/\s+/ /g;

    $map{$key} = $fun;
  }

  return %map;
}
 
sub parse_h {

  my $text = shift;
  my $map = shift;
  
  my @blocks = &extract_blocks($text);

  my $block;
  foreach $block (@blocks){

    #print "----\n$block\n---\n";
    next unless ( $block =~ /(\w+[\s\*\&]+[\w]+)(\s*\(.*?\))/s );
    # matches: void  *  myfun ( double x, double y){

    my $key = $1;
    my $fun = $1 . $2;

    $key =~ s/\s+/ /g;

    # overwrite a .h entry with the corresponding .cpp entry
    if (exists $map->{$key}){

      #print "overwriting: $block\n";
      #print "with $map->{$key}\n";

      $block = $map->{$key};
      
    }
  }

  $text = join ("", @blocks);

  return $text;
}
  
sub extract_blocks {

  my $text = shift;

  $text =~ s/\r//g; 
  
  my $sep = "x8o8A8newline";
  $text =~ s/(\n[ \t]*\n)/$1$sep/g;

  my @blocks = split($sep, $text);
  

  return @blocks;
}
