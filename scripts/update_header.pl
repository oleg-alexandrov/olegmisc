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
  
  my ($cpp_file, $header_file, $text, %cpp_map);

  $cpp_file    = $ARGV[0];
  $header_file = $ARGV[1];
  
  open(FILE, "<$cpp_file") || die "Cannot open $cpp_file\n";
  $text = <FILE>;
  close(FILE);

  &parse_cpp($text, \%cpp_map);

  # override header file name if available
  if ($text =~ /\n\#\s*include\s*\"(.*?)\".*?header file/){
    $header_file = $1;

    if ($cpp_file =~ /^(.*\/)/){
      $header_file = $1 . $header_file;
    }
  }
  
  open(FILE, "<$header_file") || die "Cannot open $header_file\n";
  $text = <FILE>;
  close(FILE);

  $text = &parse_h($text, \%cpp_map);
  
  open(FILE, ">$header_file") || die "Cannot open $header_file\n";
  print FILE "$text";
  close(FILE);

  print "$header_file updated\n";
}

sub key_sig{

  # Signature is the number of commas
  
  my $block = shift;
  $block =~ s/\{.*?$//sg;

  $block = $block . "\n";
  $block =~ s/\/\/.*?\n//g;

  $block =~ s/[^,]//g;

  return $block;
}

sub parse_cpp {

  my $text    = shift;
  my $cpp_map = shift;

  my @blocks = &extract_blocks($text);

  my $block;
  foreach $block (@blocks){

    $block =~ s/\/\*.*?\*\///sg; # get rid of C-style comments

    # Will match things like: void  *  myname::myfun ( double x, double y) const{
    next unless ( $block =~ /(\w+[\s\*\&]+\w+\:\:\w+)\s*(\(.*?\)\s*\w*\s*)\{/s );

    my $key = $1;
    my $fun = $1 . $2 . ";\n\n";

    $fun =~ s/\s*;/;/g;
    
    # rm namespace from fun declaration
    $key =~ s/(\w+::)//;  
    $key =~ s/\s+/ /g;

    $cpp_map->{$key} = $fun;

  }

}
 
sub parse_h {

  my $text    = shift;
  my $cpp_map = shift;

  # identify the namespace in the h class
  my $namesp;
  # Look at the last of all namespaces (this is a bit hackish)
  if ($text =~ /^.*\n\s*(class|struct)\s+(\w+)\s*:*.*?\{/s){
    $namesp = $2;
  }else{
    print "Can't identify the namespace!\n";
    exit(0);
  }

  my (%h_map, $key);
  
  my @blocks = &extract_blocks($text);

  # Overwrite function prototypes that changed in the cpp file
  my $block;
  foreach $block (@blocks){

    $block =~ s/\/\*.*?\*\///sg; # get rid of C-style comments

    my $is_static = ($block =~ /^\s*static\s+/);

    # Strip the static keyword for now
    my $static = "";
    if ($is_static){
      $block =~ s/^(\s*static\s+)//g;
      $static = $1;
    }
    
    # match things like: void  *  myfun ( double x, double y){
    if  ( $block =~ /(\w+[\s\*\&]+[\w]+)(\s*\(.*\))/s ){
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

    # Overwrite a .h entry with the corresponding .cpp entry.

    #print "overwriting: $block\n";
    #print "with $cpp_map->{$key}\n";
    
    $block = $cpp_map->{$key};

    # Put back the static keyword
    if ($is_static){
      $block = "static " . $block;
    }
    
    # rm the namespace and indent
    $block =~ s/(\w+::)(\w+\s*\()/$2/;
    $block = &indent_block ($block);
    
  }

  # See what new functions were declared in the cpp file and are missing in
  # the h file
  my @new_blocks;
  @new_blocks = ();
  foreach $key (keys %$cpp_map){

    next if (exists $h_map{$key});

    my $new_block = $cpp_map->{$key};

    next unless ($new_block =~ /$namesp\:\:/ ); # must be same namespace

    # rm the namespace and indent
    $new_block =~ s/(\w+::)(\w+\s*\()/$2/;

    $new_block =~ s/^\s*/  /g;
    $new_block = &indent_block ($new_block);

    push (@new_blocks, $new_block);
    
  }

  my $new_chunk = "";
  if ( scalar (@new_blocks) != 0 ) {
    $new_chunk = join ("", @new_blocks);
  }
  
  $text = join ("", @blocks);

  # Append the new blocks under the very last private:
  if ($new_chunk !~ /^\s*$/){
    $text =~ s/^(.*private:\s*)(.*?)$/$1$new_chunk$2/sg;
  }

  $text =~ s/\s*$/\n/g;
  
  return $text;
}
  
sub extract_blocks {

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

