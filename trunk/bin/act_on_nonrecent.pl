#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Do something on files matching a given pattern which are older than one hour.
  # Mark the done files, as to not do them again. So delay the action.
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 pattern\n";
    exit(0);
  }

  my $pattern = shift;

  my $doneFile = "done.txt";
  if (-e $doneFile){
    print "ERROR: File: $doneFile exists\n";
    exit(1);
  }
  
  my $machine = qx(uname -n);
  print "Running on machine: $machine\n";

  my $numDays = 5;
  my $sleep = 10;
  my $total = $numDays*24*3600/$sleep;
  for (my $val = 0; $val < $total; $val++){
    
    sleep $sleep;

    print "Pattern is $pattern\n";

    my $cmd = "ls -d $pattern";
    print "$cmd\n";
    my @files = split("\n", qx($cmd));
    my %list;
    foreach my $file (@files){
      $file =~ s/^\s*(.*?)\s*$/$1/g;
      next unless ( $file !~ /^\s*$/ && -e $file);
      $list{$file} = 1;
    }
    
    my $success = 0;
    my %done;
    if (-e $doneFile){
      open(FILE, "<$doneFile");
      my @lines = <FILE>;
      foreach my $file (@lines){
        $file =~ s/^\s*(.*?)\s*$/$1/g;
        next unless ( $file !~ /^\s*$/ && -e $file);
        $done{$file} = 1;
      }
      close(FILE);
    }
    
    foreach my $file (keys %list){
      next if ( exists $done{$file} );

      my @array=stat($file);
      my $mod_time = $array[9];
      my $cur_time = time;
      
      my $diff_time = $cur_time - $mod_time;
      print "File: $file modified $diff_time seconds ago.\n";

      if ($diff_time  > 600){
        my $cmd = "show_dems.pl $file";
        print "$cmd\n";
        print qx($cmd) . "\n";
        $success = 1;
        $done{$file} = 1;
      }else{
        print "File: $file too new, will wait.\n";
      }
    }
    
    if ($success){
      print "Will write to: $doneFile\n";
      open(FILE, ">$doneFile");
      foreach my $file ( sort {$a cmp $b} keys %done)  {
        print FILE "$file\n";
      }
      close(FILE);
    }
    
  }

}
