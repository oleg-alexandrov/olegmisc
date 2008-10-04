#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use Encode;
require 'mail_utils.pl';

undef $/; # undefines the separator. Can read one whole file in one scalar.
$| = 1; # flush the buffer each line

# Get the ids from a mailbox, put all of them in a file

MAIN: {

  #my $mode = "old_mail";
  my $mode = "gmail";

  my $id_file  = $mode . "_ids.txt";
  my $mail_dir = "/home/aoleg/" . $mode;

  # make file blank before we start
  open(FILE, ">$id_file");
  print FILE "";
  close(FILE);
  
  my $file_line = `find $mail_dir`;
  my @files = split("\n", $file_line);

  my $file;
  foreach $file (@files){

    if ( -d $file){
      print "$file is a directory, skipping\n";
      next;
    }

    if ($file =~ /All Mail/){
      print "Skip $file as too big\n";
      next;
    }

    print "Will do $file\n";
    
    my @ids;
    @ids = &extract_ids($file);

    open(FILE, ">>$id_file");
    print "Appending ids to $id_file\n";
    foreach my $id (@ids){
      print FILE "$id\n";
    }
    close(FILE);

    print "\n";
  }

}

sub extract_ids {
  
  my $folder = shift;
  my @ids = ();
  
  my @mails;
  &read_mailbox($folder, \@mails);

  my $message;
  foreach $message (@mails){

    next unless ($message =~ /^From /);
    
    my ($header, $body) = &extract_header_body ($message);

    $header = &add_message_id_if_needed($header);
       
    my $id = extract_message_id($header);

    push (@ids, $id);
  }

  return @ids;
}


sub write_missing {
  
  my $folder    = shift;
  my $existing  = shift;
  my $outfile   = shift;
  
  my @mails;
  &read_mailbox($folder, \@mails);

  my $message;
  foreach $message (@mails){

    next unless ($message =~ /^From /);
    
    my ($header, $body) = &extract_header_body ($message);

    $header = &add_message_id_if_needed($header);
       
    my $id = extract_message_id($header);

    if ( exists $existing->{$id} ) next;  

    $message = &combine_header_body($header, $body);

    open(FILE, ">>$outfile");
    print FILE $message;
    close(FILE);
    
  }

}
