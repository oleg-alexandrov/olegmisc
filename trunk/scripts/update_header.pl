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

    # rm namespace from fun declaration
    $key =~ s/(\w+::)//g;  
    $fun =~ s/(\w+::)(\w+\s*\()/$2/g;
    
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
    next unless ( $block =~ /(\w+[\s\*\&]+[\w]+)(\s*\(.*\))/s );
    # matches: void  *  myfun ( double x, double y){

    my $key = $1;
    my $fun = $1 . $2;

    $key =~ s/\s+/ /g;

    # overwrite a .h entry with the corresponding .cpp entry
    if (exists $map->{$key} &&
        $block !~ /=/ # ignore functions with preset params
       ){

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


sub indent_block {
  
  # indent a function's prototype declaration to opening parenthesis
  
  my $block = shift;
  
  $block =~ s/^\s*/  /g;
  $block =~ s/\s*$//g;
  
  my $indent_spaces = "";
  
  my @lines = split("\n", $block);
  my $line;
  
  my $is_first_line = 1;
  foreach $line ( @lines ){
    
    if ($is_first_line == 1){
      
      if ($line !~ /^(.*?\()/){
	print "Error, can't match start of function\n";
      }
      
      $indent_spaces = " " x length($1);
      $is_first_line = 0;
      next;
    }
    
    $line =~ s/^\s*//g;
    $line = $indent_spaces . $line;
  }

  $block = join ("\n", @lines);
  $block = $block . "\n\n";

  return $block;

}
