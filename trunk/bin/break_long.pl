#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# Break long line in code by inserting extra newlines
MAIN: {
  
  # Warning: this code is buggy

  if (scalar (@ARGV) <= 0  ){
    print "Please enter a file to process\n";
    exit(0);
  }

  my ($line, @lines, $text, $line_len, $comment, $sep, $spaces, @tokens, $line2);
  my $token;
  
  $line_len = 88; # Make all lines shorter than this
  $sep = ' aX89oA '; # Something unlikely
  
  open(FILE, "<$ARGV[0]");
  $text = <FILE>;
  close(FILE);

  @lines = split("\n", $text);
  foreach $line (@lines){

    # Ignore lines that have comments
    next if ($line =~ /\/\//);

    $line =~ s/\s*$//g;
    
    # Line is short enough
    next if (length($line) <= $line_len);

    next unless ($line =~ /^(\s*)/);
    $spaces = $1;

    #print "****\n$line\n****\n";

    # break line at , -, +, =, &&, ||  
    $line =~ s/(,|\-|\+|\&\&|\|\|)/$1$sep/g;
    @tokens = split($sep, $line);

    $line  = "";
    $line2 = "";

    # Start putting back the tokens in $line. If $line gets too long,
    # put things instead in $line2.
    foreach $token (@tokens){
      if (length($line . $token) <= $line_len){
        $line = $line . $token;
      }else{
        $line2 = $line2 . $token; 
      }
    }

    # Append $line2 to $line while putting $line2 on newline
    # and indenting it at the level of $line (which is not
    # always the right level of indentation)
    $line2 =~ s/^\s*//g;

    $line = $line . "\n" . $spaces . $line2;

    $line =~ s/^s\*/$spaces/g;
    $line =~ s/\s*$//g;

    #print "\n--st--\n$line\n--ed--\n\n";
  }

  print join("\n", @lines);
  
  
}
