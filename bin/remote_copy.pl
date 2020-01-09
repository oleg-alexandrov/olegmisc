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

  my $homeDir = get_home_dir();
  
  # Pass the arguments starting with dash to rsync
  my $opts = "";
  my @other;
  foreach my $arg (@ARGV){
    if ($arg =~ /^\-/){
      $opts .= " $arg";
    }else{
     push(@other, $arg);
    }
  }
  @ARGV = @other;

  # See if to use a custom destination directory on the other side.
  # Prepend it with +.
  my $remoteDir = "";
  for (my $i = 0; $i < scalar(@ARGV); $i++){
    if ($ARGV[$i] =~ /^\+/){
      $remoteDir = splice(@ARGV, $i, 1);
      $remoteDir =~ s/^\+//g;
    }
  }

  # See if to use a custom local home directory on this side.
  # Prepend it with \=.
  my $localHomeDir = "";
  for (my $i = 0; $i < scalar(@ARGV); $i++){
    if ($ARGV[$i] =~ /^\=/){
      $localHomeDir = splice(@ARGV, $i, 1);
      $localHomeDir =~ s/^\=//g;
    }
  }

  my $numArgs = scalar(@ARGV);
  if ($numArgs < 2){
    print "Usage: $0 <options> user\@machine files\n";
    print "or   : $0 <options> files user\@machine\n";
    exit(1);
  }

  if ($ARGV[0] =~ /\@/){

    # Copy from current directory on remote machine to same directory
    # on local machine
    my $from = splice(@ARGV, 0, 1);
    foreach my $file (@ARGV){

      # Get the file name relative to home directory
      $file = get_path_in_home_dir($file);

      # Create the destination directory on the local machine
      my $destDir = get_home_dir() . "/" . get_dirname($file);
      qx(mkdir -p $destDir);

      # Stop complaining about untrusted host
      my $cmd = "rsync -P -avz $opts -e 'ssh $from -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no' :$file $destDir 2>/dev/null";
      print "$cmd\n";
      system($cmd);
    }

  }elsif ($ARGV[$numArgs - 1] =~ /\@/){
    # Copy from current directory on local machine to same directory
    # on remote machine
    my $to = splice @ARGV, $numArgs - 1, 1;

    foreach my $file (@ARGV){
      my $rel_path = get_path_in_home_dir($file, $localHomeDir);

      # Place in custom location
      if ($remoteDir ne ""){
        $rel_path = $remoteDir . "/" . $file;
      }

      my $subDir = get_dirname($rel_path);

      # Ensure that $subDir exists on remote machine
      if ($subDir =~ /\// && $subDir ne "./") {
        my $res = qx(ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no $to mkdir -p $subDir 2>&1);
        my $exitStatus = ($? >> 8);
        if ($exitStatus != 0){
          print "Failed: $res\n";
        }
      }

      $file = getcwd if ($file eq "."); # bugfix

      $subDir = clean_path($subDir);
      # Stop complaining about untrusted host
      my $cmd = "rsync -P -avz $opts $file  -e 'ssh $to -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no' :~/$subDir 2>/dev/null";

      if ($remoteDir ne ""){
        $cmd =~ s/$to:~\//$to:/g;
      }

      print "$cmd\n";
      system($cmd);
      #print qx($cmd) . "\n";
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

sub get_dirname{

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
