#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Cwd;
use File::Spec;

sub clean_path{
  # From /path/to/../dir create /path/dir
  my $path = shift;
  while ($path =~ /^(.*?)\/\w+\/\.\.(.*?)$/){
    $path = $1 . $2;
  }

  # Wipe /. at the end, it makes rsync do the wrong thing
  $path =~ s/\/*\.$//g;

  return $path;
}

sub get_path_in_home_dir{

  # Make the input path absolute. Then, from
  # /home/user/abc/something.txt extract abc/something.txt.

  my $relpath = shift;
  my $custom_home = "";
  if (scalar(@_) > 0){
    $custom_home = shift;
  }

  # For paths in current directory, use pwd,
  # this avoids dereferencing sym links.
  my $currDir=qx(/bin/pwd -L);
  $currDir =~ s/\s$//g; # wipe trailing white space

  # The full path. Don't use File::Spec yet as that one
  # may dereference external links.
  my $path;
  if ($relpath =~ /^\//){
    $path = $relpath;
  }else{
    $path = $currDir . '/' . $relpath;
  }
  
  my $parent_path = $path;
  $parent_path =~ s/\/*\s*$//g;
  if ($parent_path =~ /(^.*)\//){
    $parent_path = $1;
  }

  # Do not use this as it dereferences sym links making a mess.
  ## Use rel2abs as our naive handling missed something.
  #if ( (! -f $parent_path) && (!-d $parent_path) ){
  #  $path = File::Spec->rel2abs($relpath);
  #}

  my $home = get_home_dir();
  my $whoami = qx(whoami); $whoami =~ s/\s*$//g;

  # Sometimes home dir is /home/user, while file is in
  # /nobackup/user symlinked to /home/user. Handle this.
  if ($custom_home eq ""){
    if ($path =~ /^\/.*?\/$whoami\/(.*?)$/){
      $path = $1;
    }elsif ($path =~ /^.*?\/$whoami$/){
      $path = "";
    }else{
      print "Error: Expecting $path to be in $home.\n";
      exit(1);
    }
  }else{
    if ($path =~ /^$custom_home\/*(.*?)$/){
      $path = $1;
    }else{
      print "Error. Expecting $path to be in $custom_home.\n";
      exit(1);
    }
  }
  
  $path =~ s/^\/*//g;
  if ($path =~ /^\s*$/){
    $path = ".";
  }

  $path = clean_path($path);

  return $path;
}

sub get_home_dir{
  my $home = $ENV{'HOME'};
  $home =~ s/\s*$//g;
  return $home;
}

1;
