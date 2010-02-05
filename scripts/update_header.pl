#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# Extract C++ function information from a .cpp file and
# use that to update the corresponding .h file.

require $ENV{HOME} . '/.xemacs/indent_block.pl';

MAIN: {

  if (scalar @ARGV < 2){
    print "Must have at least two files as input\n";
    exit(0);
  }
  
  my ($cpp_file, $header_file, %cpp_map);

  $cpp_file    = $ARGV[0];
  $header_file = $ARGV[1];
  
  open(FILE, "<$cpp_file") || die "Cannot open $cpp_file\n";
  my $cpptext = <FILE>;
  close(FILE);

  # Override the header file name if available
  if ($cpptext =~ /\n\#\s*include\s*\"(.*?)\".*?header file/){
    $header_file = $1;

    if ($cpp_file =~ /^(.*\/)/){
      $header_file = $1 . $header_file;
    }
  }
  
  open(FILE, "<$header_file") || die "Cannot open $header_file\n";
  my $h_text = <FILE>;
  close(FILE);

  # Do the work
  my $namespace = get_namespace($h_text);
  &parse_cpp($cpptext, \%cpp_map, $namespace); # %cpp_map is output
  $h_text = &parse_h($h_text, \%cpp_map, $namespace); # %cpp_map is input
  
  open(FILE, ">$header_file") || die "Cannot open $header_file\n";
  print FILE $h_text;
  close(FILE);

  print "$header_file updated\n";
}

sub parse_cpp {

  my $text      = shift;
  my $cpp_map   = shift;
  my $namespace = shift;

  my @blocks = &extract_blocks_cpp($text);

  my $block;
  foreach $block (@blocks){

    $block =~ s/\/\*.*?\*\///sg; # get rid of C-style comments

    # Will match things like: std::string mynamespace::myfun ( double x, double y) const{
    next unless ( $block =~
                  /(?:^|\n)(\w+[^\n]*?\s+$namespace\:\:\w+)\s*(\(.*?\).*?)\{/s );

    my $key = $1;
    my $fun = $1 . $2 . ";\n\n";

    $fun =~ s/\s*;/;/g;
    
    # rm namespace from fun declaration, so std::string namespace::myfun()
    # becomes simply ...                    std::string myfun()
    $key =~ s/$namespace\:\://;  
    $key =~ s/\s+/ /g;

    $cpp_map->{$key} = $fun;

  }

}
 
sub parse_h {

  my $text      = shift;
  my $cpp_map   = shift;
  my $namespace = shift;

  my (%h_map, $key);
  
  my @blocks = &extract_blocks_h($text);
  
  # Overwrite function prototypes that changed in the cpp file
  my $block;
  foreach $block (@blocks){

    $block =~ s/\/\*.*?\*\///sg; # get rid of C-style comments

    # Strip the static keyword for now
    my $is_static = 0;
    my $static   = "";
    if ($block =~ /^(\s*(?:static|virtual)\s+)(.*?)$/s){
      $static = $1;
      $block  = $2;
    }
    
    # match things like: std::string  *  myfun ( double x, double y){
    if  ( $block =~ /[^|\n](\w.*?\s\w+)\s*\(.*?\)/s ){
                    
      $key = $1;
      $key =~ s/\s+/ /g;
    }else{
      # no match, put back the static and go on to the next block
      $block = $static . $block;
      next;
    }
    
    $block = $static . $block;
    
    # We'll need this map later
    $h_map{$key} = $block; 
    
    # Skip functions not having a match in the cpp file
    next unless (exists $cpp_map->{$key});
    
    # Ignore functions with preset params (having the equal sign somehwere)
    next if ($block =~ /=/);

    # Save the number of newlines
    my $oldnewlines = "";
    if ($block =~ /(\s*)$/){
      $oldnewlines = $1;
    }

    # Overwrite a .h entry with the corresponding .cpp entry.
    #print "Overwriting: $block\n";
    $block = $cpp_map->{$key};
    
    # Put back the static keyword
    if ($is_static){
      $block = $static . $block;
    }
    
    # rm the namespace and indent
    $block =~ s/($namespace\:\:)(\w+\s*\()/$2/; # Must escape : here
    $block = &indent_block ($block);

    # Use the original number of newlines
    $block =~ s/\s*$//g;
    $block = $block . $oldnewlines;
    
    #print "Overwriting with $block\n";
  }

  # See what new functions were declared in the cpp file and are missing in
  # the h file
  my @new_blocks = ();
  foreach $key (keys %$cpp_map){

    next if (exists $h_map{$key});

    my $new_block = $cpp_map->{$key};

    next unless ($new_block =~ /$namespace\:\:/ ); # must be same namespace

    # rm the namespace and indent
    $new_block =~ s/($namespace\:\:)(\w+\s*\()/$2/;
    
    $new_block =~ s/^\s*/  /g;
    $new_block = &indent_block ($new_block);

    push (@new_blocks, $new_block);
    
  }

  my $new_chunk = "";
  if ( scalar (@new_blocks) != 0 ) {
    $new_chunk = join ("", @new_blocks);
  }
  
  $text = join ("", @blocks);

  # Append the new blocks under the very last private: or public:
  if ($new_chunk !~ /^\s*$/){

    if ($text =~ /(^|\n)private:/){
      
      $text =~ s/^(.*private:(?:[ ]*\n)*)(.*?)$/$1$new_chunk$2/sg;
      
    }elsif ($text =~ /(^|\n)public:/){
      
      $text =~ s/^(.*public:(?:[ ]*\n)*)(.*?)$/$1$new_chunk$2/sg;
      
    }elsif ($text =~ /(^|\n)class\s+$namespace/){
      
      $text =~ s/^(.*class $namespace.*?\{(?:[ ]*\n)*)(.*?)$/$1$new_chunk$2/sg;
      
    }else{
     print "Must have public: private: or class{\n"; 
    }
    
  }

  $text =~ s/\s*$/\n/g;
  
  return $text;
}
  
sub extract_blocks_cpp {

  # The separator between blocks is an empty line
  
  my $text = shift;

  $text =~ s/\r//g; 
  $text =~ s/\t/ /g; 

  # Insert a newline after these keywords
  $text =~ s/(public|private|protected)\s*:\s*([^\}])/$1:\n\n  $2/g;
  
  # If a line ends with comma (followed optionally by comments),
  # remove any empty lines following that line, so that later we don't
  # think the empty line separates two blocks.  Don't break the
  # indentation.
  
  while ( $text =~ /(^.*?)(,[ ]*)(\/\/[^\n]*?\n|\n)([ ]*\n\s*)(.*?)$/s ){

    my $before = $1 . $2 . $3;
    my $spaces = $4;
    my $after  = $5;

    $spaces =~ s/^.*\n//g; 
    $text = $before . $spaces . $after;
  }
  
  my $sep = "x8o8A8newline";
  $text =~ s/(\n[ \t]*\n)/$1$sep/g;

  my @blocks = split($sep, $text);

  return @blocks;
}

sub extract_blocks_h{

  # Split into blocks by newline. If one block does not have a
  # balanced paranthesis set, merge it with the next block
  
  my $text = shift;

  $text =~ s/\r//g; 
  $text =~ s/\t/ /g; 

  my @blocks = split(/\n/, $text);

  my @nblocks = ();
  my $nblock = "";

  my $count = 0;
  foreach my $block (@blocks){

    $count++;
    
    $nblock = $nblock . $block;

    # Add the newline back except for the last field
    $nblock .= "\n" if ( $count < scalar(@blocks) );
    
    if (balanced_parens($nblock)){
      
      push (@nblocks, $nblock);
      $nblock = "";
      next;
      
    }
    
  }

  if ($nblock !~ /^\s*$/){
    push (@nblocks, $nblock);
  }

  return @nblocks;
}

sub balanced_parens{

  # If the current text has a balanced number of parentheses
  
  my $text = shift;
  
  $text =~ s/\/\/.*?(\n|$)//g;
  
  my $lpars = scalar ($text =~ /\(/g);
  my $rpars = scalar ($text =~ /\)/g);

  return ($lpars == $rpars);
}

sub get_namespace{

  # identify the namespace in the h class
  # Look at the last of all namespaces (this is a bit hackish)

  my $text = shift;
  
  my $namespace = "";
  if ($text =~ /^.*\n\s*(class|struct|namespace)\s+(\w+)\s*:*.*?\{/s){
    $namespace = $2;
  }else{
    print "Can't identify the namespace!\n";
    exit(0);
  }
  return $namespace;
}
