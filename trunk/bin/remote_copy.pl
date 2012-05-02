#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Cwd;
MAIN:{

  # Generate and run the command: rsync -avz user@machine:/path/to/currDir/file .
  # or                            rsync -avz file user@machine:/path/to/currDir
  
  my $numArgs = scalar(@ARGV);
  if ($numArgs < 2){
    print "Usage: $0 user\@machine files\n";
    print "or   : $0 files user\@machine\n";
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
    my $from = splice @ARGV, 0, 1;
    foreach my $file (@ARGV){
      my $cmd = "rsync -avz $from:~/$dir/$file . 2>/dev/null";
      print "$cmd\n";
      print qx($cmd) . "\n";
    }
  }elsif ($ARGV[$numArgs - 1] =~ /\@/){
    # Coopy from current directory on local machine to same directory on remote machine
    my $to = splice @ARGV, $numArgs - 1, 1;
    my $list = join(" ", @ARGV);
    my $cmd = "rsync -avz $list $to:~/$dir/ 2>/dev/null";
    print "$cmd\n";
    print qx($cmd) . "\n";
  }else{
    print "Invalid usage: Must copy either from or to a remote machine\n";
    exit(1);
  }

}
