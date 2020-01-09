#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Cwd;
MAIN:{

  # Generate and run the command: rsync -avz user@machine:/path/to/currDir /path/to/currDir
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 user\@machine files\n";
    exit(0);
  }

  my $home = $ENV{HOME};
  my $dir  = getcwd;
  if ($dir !~ /^$home(.*?)$/){
    print "Error: Expecting $dir to be a subdirectory of $home\n";
    exit(1);
  }
  $dir = $1;
  $dir =~ s/^\/*//g;
  #print "Home and pwd are $home $dir\n";

  my $from = splice @ARGV, 0, 1;
  my @files = @ARGV;
  foreach my $file (@files){
    my $cmd = "rsync -avz $from:~/$dir/$file . 2>/dev/null";
    print "$cmd\n";
    print qx($cmd) . "\n";
  }
  
}
