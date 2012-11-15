#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use POSIX;

MAIN:{

  my $numGroups = 10;
  my $driver = "driver.sh";
  my $list = "list.txt";
  
  open(FILE, "<$list");
  my @jobs = <FILE>;
  close(FILE);
  foreach my $job (@jobs){
    $job =~ s/\s*$//g;
  }

  my $numJobs = scalar(@jobs);
  print "There are $numJobs jobs\n";

  print "Num groups is $numGroups\n";
  
  my $groupSize = ceil($numJobs/$numGroups);
  print "Group size is $groupSize\n";
  
  for (my $groupIter = 0; $groupIter < $numGroups; $groupIter++){
    my $file = "list$groupIter.txt";

    print "Will write: $file\n";
    open(FILE, ">$file");
    for (my $i = 0; $i < $groupSize; $i++){
      my $pos = $groupIter*$groupSize + $i;
      if ($pos < $numJobs){
        print FILE $jobs[$pos] . "\n";
      }
    }
    
    open(FILE, "<$driver");
    my $text = join("", <FILE>);
    if ( $text !~ /^(.*?num=)\d+(.*?)$/sg ){
      print "No match 1!\n";
      exit(1);
    }
    $text = $1 . $groupIter . $2;
    
    if ( $text !~ /^(.*?PBS.*?group)\d+(.*?)$/sg ){
      print "No match 2!\n";
      exit(1);
    }
    $text = $1 . $groupIter . $2;

    $file = $driver;
    $file =~ s/.sh/$groupIter.sh/g;
    print "Will write: $file\n";
    open(FILE, ">$file");
    print FILE $text . "\n";
    close(FILE);

    my $cmd = "chmod a+x $file; remote_copy.pl $file \$B; qsub $file";
    print "$cmd\n";
    print qx($cmd) . "\n";
    sleep 5;
  }
  
}
