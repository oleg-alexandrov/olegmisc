#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Cwd;
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
  
  my $home = $ENV{HOME};
  my $dir  = getcwd;
  my $whoami = qx(whoami); $whoami =~ s/\s*$//g;
  
  if ($dir !~ /^.*?$whoami(.*?)$/){
    print "Error: Expecting $dir to be a subdirectory of $home.\n";
    exit(1);
  }
  $dir = $1;
  $dir =~ s/^\/*//g;
  #print "Home and pwd are $home $dir\n";

  if ($ARGV[0] =~ /\@/){ 
    # copy from current directory on remote machine to same directory on local machine
    my $from = splice(@ARGV, 0, 1);
    foreach my $file (@ARGV){

      # First create the subdirectory on the local machine
      my $subDir = get_basename($file);
      print "subdir is $subDir\n";
      qx(mkdir -p $subDir);

      my $cmd = "rsync -avz $opts $from:~/$dir/$file $subDir 2>/dev/null";
      print "$cmd\n";
      print qx($cmd) . "\n";
    }
  }elsif ($ARGV[$numArgs - 1] =~ /\@/){
    # Coopy from current directory on local machine to same directory on remote machine
    my $to = splice @ARGV, $numArgs - 1, 1;
    foreach my $file (@ARGV){

      my $subDir = get_basename($file);
      print "subdir is $subDir\n";
      if ($subDir =~ /\//) {
        # First create the subdirectory on the remote machine
        my $baseDir = get_base_dir($file);
        print "file is $file\n";
        print "base dir is $baseDir\n";
        qx(mkdir -p /tmp/$subDir);
        my $cmd = "rsync -avz /tmp/$baseDir $to:~/$dir/ 2>/dev/null";
        print "$cmd\n";
        print qx($cmd) . "\n";
        print qx(rm -rfv /tmp/$baseDir) . "\n";
      }
      
      my $cmd = "rsync -avz $opts $file $to:~/$dir/$subDir 2>/dev/null";
      print "$cmd\n";
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

  return $file;
}

sub get_basename{

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
