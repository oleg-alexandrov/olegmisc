#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{
  
  open(FILE, "<styles.css");  my $text = <FILE>;  close(FILE);
  
  my %styles;
  get_styles($text, \%styles);
  
  my @files = <*shtml>;
  foreach my $file (@files){
    print "$file\n";

    open(FILE, "<$file");  my $text = <FILE>;  close(FILE);
    get_styles($text, \%styles);
    
    $text =~ s/\<span\s+class\s*=\s*"(.*?)\"\>(.*?)\<\/span\>/&do_rep($1, $2, \%styles)/egsi;

    my $file_proc = $file . "_proc";
    open(FILE, ">$file_proc");  print FILE "$text";  close(FILE);
    
  }
}

sub get_styles{

  my $text   = shift;
  my $styles = shift;
  
  my @blocks = split(/\}/, $text);
  foreach my $block (@blocks){
    
    next unless ($block =~ /\.([\w\-\_]+)\s*\{\s*color:\s*(.*?);/s);
    $styles->{lc($1)} = lc($2);
  }

}

sub do_rep{
  my $type   = shift;
  my $text   = shift;
  my $styles = shift;

  $type = lc($type);
  
  if (!exists $styles->{$type}){
    print "$type does not exist!\n";
    exit;
  }

  my $nl ="";
#   if ($text =~ /(\s*)$/){
#     $nl   = $1;
#     $text =~ s/\s*$//g;
#   }
  return "<font color=\"" . $styles->{$type} . '">' . $text . "</font>" . $nl;
}
