#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Cwd;
use File::Basename;
use File::Spec;
use lib dirname(File::Spec->rel2abs($0));
require 'utils.pl';

MAIN:{

  # Generate and run the command: rsync -avz user@machine:path/to/currDir/file path/to/currDir/file
  # or                            rsync -avz dir/file user@machine:/path/to/currDir/dir

  # Pass the first several arguments starting with dash to rsync
  my $opts = "";
  while (scalar(@ARGV) >= 1 && $ARGV[0] =~ /^-/){
    $opts .= " " . splice(@ARGV, 0, 1);
  }

  my $numArgs = scalar(@ARGV);
  if ($numArgs < 2){
    print "Usage: $0 <options> user\@machine files\n";
    print "or   : $0 <options> files user\@machine\n";
    exit(1);
  }

  my $dir  = getcwd;
  $dir = get_path_in_home_dir($dir);

  if ($ARGV[0] =~ /\@/){

    # Copy from current directory on remote machine to same directory
    # on local machine
    my $from = splice(@ARGV, 0, 1);
    foreach my $file (@ARGV){

      # Get the file name relative to home directory
      if ($file =~ /^\//){
        # input path is absolute
        $file = get_path_in_home_dir($file);
      }else{
        # input path is relative
        $file = "$dir/$file";
      }
      $file = clean_path($file);

      # Create the destination directory on the local machine
      my $destDir = get_home_dir() . "/" . get_dir_path($file);
      qx(mkdir -p $destDir);

      my $cmd = "rsync -avz $opts $from:$file $destDir 2>/dev/null";
      print "$cmd\n";
      print qx($cmd) . "\n";
    }

  }elsif ($ARGV[$numArgs - 1] =~ /\@/){
    # Copy from current directory on local machine to same directory
    # on remote machine
    my $to = splice @ARGV, $numArgs - 1, 1;

    foreach my $file (@ARGV){
      my $abs_path = File::Spec->rel2abs($file);
      my $rel_path = get_path_in_home_dir($abs_path);
      my $subDir = get_dir_path($rel_path);
      print "subdir is $subDir\n";

      my $randDir = generate_random_string(10);

      $subDir = clean_path($subDir);
      my $cmd = "rsync -avz $opts $file $to:~/$subDir 2>/dev/null";
      print "$cmd\n";
      print qx($cmd) . "\n";
      my $exitStatus = ($? >> 8);

      # If the copy failed, then perhaps the directory is missing on the remote
      # machine, then create it and redo the copy.
      if ($exitStatus != 0 && $subDir =~ /\// && $subDir ne "./") {
        # First create the subdirectory on the remote machine
        my $baseDir = get_base_dir($rel_path);
        print "file is $file\n";
        print "base dir is $baseDir\n";
        qx(mkdir -p /tmp/$randDir/$subDir);
        my $dir_cmd = "rsync -avz /tmp/$randDir/$baseDir $to: 2>/dev/null";
        print "$dir_cmd\n";
        print qx($dir_cmd) . "\n";
        if ($baseDir !~ /^\.*\/*$/ && $baseDir !~ /\.\./ ){
          # Careful with what we wipe
          print qx(rm -rfv /tmp/$randDir) . "\n";
        }
      }
      print qx($cmd) . "\n";


    }
  }else{
    print "Invalid usage: Must copy either from or to a remote machine\n";
    exit(1);
  }

}

sub strip_stuff{

  my $file = shift;
  $file =~ s/^\.\///g; # strip leading ./
  $file =~ s/^\~\///g; # strip leading ~/
  $file =~ s/^\/home\w*\/\w*\///g; # from /home/user/someDir get just someDir
  $file =~ s/\/*\.$//g; # strip trailing .

  return $file;
}

sub get_dir_path{

  # from dir1/dir2/dir3/dir4.txt return dir1/dir2/dir3

  my $file = shift;
  $file = strip_stuff($file);

  my $dir = ".";
  if ($file =~ /^(.*)\//){
    $dir = $1;
  }

  if ($dir eq ""){
    print "ERROR: Could not parse: $file\n";
    exit(1)
  }

  return $dir;
}

sub get_base_dir{

  # from dir1/dir2/dir3/dir4.txt return dir1

  my $file = shift;
  $file = strip_stuff($file);

  my $dir = ".";
  if ($file =~ /^(.*?)\//){
    $dir = $1;
  }

  if ($dir eq ""){
    print "ERROR: Could not parse: $file\n";
    exit(1)
  }

  return $dir;
}
