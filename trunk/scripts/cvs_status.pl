#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use Cwd;
undef $/; # undefines the separator. Can read one whole file in one scalar.

# use main to avoid the curse of global variables
MAIN: {

  # Clean up cvs status
  if ( ( -d ".svn" ) && ( ! -d "CVS" ) ){
    print `svn status --show-updates`;
    exit(0);
  }
  
  my $pwd = getcwd;
  $pwd = $pwd . "/" unless ($pwd =~ /\/$/);

  my $user = $ENV{HOME};
  $user =~ s/\/$//g;
  $user =~ s/^.*\///g;

  my @lines = split("===", `cvs status`);
  
  my ($line, $file, $status, @files, @stats, $file_len);

  @files = ();
  @stats = ();
  $file_len = 0;
  
  foreach $line (@lines){

    next if ($line =~ /Status:\s+Up-to-date/i);

    next unless ($line =~ /Status:\s*(.*?)\n/i);
    $status = $1;
    
    next unless ($line =~ /Repository\s+revision:.*?(\/.*),/i);
    $file = $1;

    # Make file path relative
    $file =~ s/^.*?(matlabRD|doc_plan|dev|baseline)\///g;

    # Strip some other odd markup
    $file =~ s/Attic\///g;
        
    push(@files, $file);
    push(@stats, $status);

    if ($file_len < length($file)){
      $file_len = length($file);
    }
    
  }

  my $counter = 0;

  foreach $file (@files){

    $status = $stats[$counter]; $counter++;

    $file = &make_local_to_cur_dir($pwd, $file);
    
    # pad with spaces after the file name
    $file = $file . " " x ($file_len - length($file));

    print "File: $file $status\n";
  }

}

sub make_local_to_cur_dir {

  # if pwd is /lan/sso/grp_apsae_work01/olegalex/mpc/dev/modellib/libacusip/
  # and file name is modellib/libacusip/src/acusipepc.h
  # transform the file name into src/acusipepc.h

  my $pwd  = shift;
  my $file = shift;

  my $pwd_len = length($pwd);

  for (my $i = 0 ; $i < $pwd_len;  $i++){
    
    my $fragment = substr $pwd, $i;

    if ($file =~ /^\Q$fragment\E(.*?)$/){
      $file = $1;
      return $file;
    }
  }

  return $file;
}
