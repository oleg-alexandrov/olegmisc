#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Cwd;
use File::Spec;
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

      # Create the destination directory on the local machine
      my $destDir = $ENV{'HOME'} . "/" . get_dir_path($file);
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
      
      if ($subDir =~ /\// && $subDir ne "./") {
        # First create the subdirectory on the remote machine
        my $baseDir = get_base_dir($rel_path);
        print "file is $file\n";
        print "base dir is $baseDir\n";
        print "file is $file\n";
        print "base dir is $baseDir\n";
        qx(mkdir -p /tmp/$subDir);
        my $cmd = "rsync -avz /tmp/$baseDir $to: 2>/dev/null";
        print "$cmd\n";
        print qx($cmd) . "\n";
        if ($baseDir !~ /^\.*\/*$/ && $baseDir !~ /\.\./ ){
          # Careful with what we wipe
          print qx(rm -rfv /tmp/$baseDir) . "\n";
        }
      }
      
      my $cmd = "rsync -avz $opts $file $to:~/$subDir 2>/dev/null";
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

sub get_path_in_home_dir{
  
  # From /home/user/abc/something.txt
  # return abc/something.txt
  
  my $path = shift;
  my $home = $ENV{HOME};
  my $whoami = qx(whoami); $whoami =~ s/\s*$//g;
  if ($path !~ /^.*?$whoami(.*?)$/){
    print "Error: Expecting $path to be in $home.\n";
    exit(1);
  }
  $path = $1;
  $path =~ s/^\/*//g;

  if ($path =~ /^\s*$/){
    $path = ".";
  }
  
  return $path;
}
