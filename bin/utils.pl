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

sub set_path{
  $ENV{'PATH'} = $ENV{'HOME'} . '/projects/BinaryBuilder/latest/bin:' . $ENV{'HOME'} . '/projects/base_system/bin:/byss/packages/gdal-1.8.1/bin/:' . $ENV{'PATH'};
}

sub maybe_call_itself_on_remote_host{

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

    # Use pwd -L, this avoids dereferencing sym links.
    my $currDir=qx(/bin/pwd -L);
    $currDir =~ s/\s*$//g; # wipe trailing white space

    my $cmd = "ssh $r_u_host 'source .bashenv; nohup nice -20 " . basename($0) . " --dir " .
       get_path_in_home_dir($currDir) . " " . join(" ", @args) . "' 2>/dev/null";
    
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
    exit(0);
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
  my $home = $ENV{'HOME'};
  $home =~ s/\s*$//g;
  return $home;
}

1;
