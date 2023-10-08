#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# Given a list of dependencies in conda_list.txt, as created by 'conda
# list' for a given environment, and a feedstock directory, having
# inside of it recipe/meta.yaml, for each depenendency in the meta.yaml
# file copy the version from conda_list.txt.
# TODO(oalexan1): Convert to python and ship with BinaryBuilder.

MAIN:{
  if (scalar(@ARGV) < 2){
    print "Usage: $0 conda_env.yaml feestock_dir\n";
    exit(0);
  }

  my $conda_file = $ARGV[0];
  if (! -f $conda_file){
    print "Missing: $conda_file\n";
    exit(1);
  }

  my $feedstock_dir = $ARGV[1];
  my $meta_file = "$feedstock_dir/recipe/meta.yaml";
  if (! -f $meta_file){
    print "Missing $meta_file\n";
    exit(1);
  }
  
  my %conda_env;
  open(FILE, "<$conda_file");
  foreach my $line (<FILE>){
    $line =~ s/\#.*?$//g;
    if ($line !~ /^\s*-\s*(.*?)\s*=+\s*(.*?)(=|\s|$)/){
      next;
    }
    my $package = $1;
    my $version = $2;
    if ($package =~ /^\s*$/){
      next;
    }
    $conda_env{$1} = $2;
  }
  close(FILE);

  open(FILE, "<$meta_file");
  my @lines = <FILE>;
  close(FILE);

  for (my $count = 0; $count < scalar(@lines); $count++){
    my $line = $lines[$count];
    if ($line !~ /^(\s+-)\s*([^\s]+)(\s+)(.*?)$/){
      next;
    }
    my $pre = $1;
    my $package = $2;
    my $spaces = $3;
    my $old_version = $4;
    $old_version =~ s/^\s*//g;
    $old_version =~ s/\s*$//g;

    if (!exists $conda_env{$package}){
      next;
    }

    my $version = $conda_env{$package};
    if ($old_version ne $version){
      print "For $package, replacing version '$old_version' with '$version'\n";
      $lines[$count] = $pre . $package . $spaces . $version . "\n";
    }
  }

  print "Replaced all versions that changed.\n";
  
  open(FILE, ">$meta_file");
  foreach my $line (@lines){
    print FILE $line;
  }
  close(FILE);
  print "Wrote: $meta_file\n";
  
}
