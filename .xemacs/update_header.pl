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
  
}


sub parse_cpp {

  my $text    = shift;
  my $cpp_map = shift;

  my @blocks = &extract_blocks($text);

  my $block;
  foreach $block (@blocks){

    # Will match things like: void  *  myname::myfun ( double x, double y){
    next unless ( $block =~ /(\w+[\s\*\&]+[\w]+\:\:[\w]+)(\s*\(.*?\))\s*\{/s );
    #print "----\n$block\n---\n";

    my $key = $1;
    my $fun = $1 . $2 . ";\n\n";

    # rm namespace from fun declaration
    $key =~ s/(\w+::)//g;  
    $key =~ s/\s+/ /g;

    $cpp_map->{$key} = $fun;

  }

}
 
sub parse_h {

  my $text    = shift;
  my $cpp_map = shift;

  # identify the namespace in the h class
  my $namesp;
  if ($text =~ /\n\s*(class|struct)\s+(\w+)\s*:*.*?\{/){
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

    # match things like: void  *  myfun ( double x, double y){
    next unless ( $block =~ /(\w+[\s\*\&]+[\w]+)(\s*\(.*\))/s );

    $key = $1;
    $key =~ s/\s+/ /g;

    # We'll need this map later
    $h_map{$key} = $block; 
    
    # Overwrite a .h entry with the corresponding .cpp entry.
    # Ignore functions with preset params (having the equal sign somehwere)
    next unless (exists $cpp_map->{$key} && $block !~ /=/);

    #print "overwriting: $block\n";
    #print "with $cpp_map->{$key}\n";
    
    $block = $cpp_map->{$key};
    
    # rm the namespace and indent
    $block =~ s/(\w+::)(\w+\s*\()/$2/g;
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
    $new_block =~ s/(\w+::)(\w+\s*\()/$2/g;
    $new_block = &indent_block ($new_block);
    
    push (@new_blocks, $new_block);
    
  }
  
  # Append that new block to the private functions by appending
  # them to the block with the "private" keyword
  foreach $block (@blocks){
    if ($block =~ /^\s*private:/){

      if ( scalar (@new_blocks) != 0 ) {
        $block = $block . join ("", @new_blocks); 
      }

      last;
    }
  }
  
  $text = join ("", @blocks);

  # Minor touchup
  $text =~ s/\(\s*/\(/g;
  
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

