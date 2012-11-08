#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Cwd;

sub maybe_call_itself_on_remote_host{

  print "\nDirectory: " . getcwd . "\n";
  print $0 . " " . join(" ", @ARGV) . "\n\n";
  
  my @args = @_;
  my $u_host = get_u_host();

  if ( !exists $ENV{"L2"} || $ENV{"L2"} !~ /\@/ ){
    print "ERROR: Incorrect value of environmental variable: L2\n";
    exit(1);
  }
  my $r_u_host = $ENV{"L2"};
  
  if ($u_host ne $r_u_host){

    # Connect to the right machine and relaunch itself
    # But first copy the data there.
    
    foreach my $arg (@args){
      if (-e $arg){
        print qx(remote_copy.pl $arg $r_u_host) . "\n";
      }
    }
    
    my $cmd = "ssh $r_u_host 'source .bashenv; " . basename($0) . " --dir " .
       get_path_in_home_dir(getcwd) . " " . join(" ", @args) . "'";
    print qx($cmd) . "\n";
    exit(0);
  }

  # If on the right machine, but not in the right dir,
  # fix that, and call itself
  if (scalar(@args) >= 2 && $args[0] eq '--dir'){
    shift @args;
    my $dir = shift @args;
    chdir $dir;
    my $cmd = $0 . " " . join(" ", @args);
    print qx($cmd) . "\n";
    exit(1);
  }
  
}

sub get_u_host{

  my $host = qx(uname -n);
  $host =~ s/\..*?$//g;
  
  my $ans = qx(whoami) . '@' . $host;
  $ans =~ s/\n//g;
  return $ans;
}

sub get_path_in_home_dir{
  
  # From /home/user/abc/something.txt
  # return abc/something.txt
  
  my $path = shift;
  my $home = get_home_dir();
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

sub generate_random_string{
  
  my $len=shift;# the length of 
  # the random string to generate
  
  my @chars=('a'..'z','A'..'Z','0'..'9','_');
  my $random_string;
  foreach (1..$len){
    # rand @chars will generate a random 
    # number between 0 and scalar @chars
    $random_string.=$chars[rand @chars];
  }
  return $random_string;
}

sub get_home_dir{
  my $home;
  my $machine = qx(uname -n);
  if ($machine =~ /pfe/){
    $home = "/nobackupnfs1/" . qx(whoami);
    $home =~ s/\s*$//g;
  }else{
    $home = $ENV{'HOME'};
  }
  return $home;
}

1;
