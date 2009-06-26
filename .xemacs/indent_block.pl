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
        print "$block\n";
        return $block;
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

1;
