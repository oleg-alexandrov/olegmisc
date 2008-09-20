#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use Encode;
require 'mail_utils.pl';

undef $/; # undefines the separator. Can read one whole file in one scalar.
$| = 1; # flush the buffer each line

# Get the ids from a mailbox, put all of them in a file

MAIN: {

  my $id_file = "old_mail_ids.txt";

  # make file blank before we start
  open(FILE, ">$id_file");
  print FILE "";
  close(FILE);
  
  my $file_line = `find /home/aoleg/old_mail`;
  my @files = split("\n", $file_line);

  my $file;
  foreach $file (@files){

    if ( -d $file){
      print "$file is a directory, skipping\n";
      next;
    }

    my @ids = &extract_ids($file);

    open(FILE, ">>$id_file");
    print "Appending ids to $id_file\n";
    foreach my $id (@ids){
      print FILE "$id\n";
    }
    close(FILE);
        
  }

}

sub extract_ids {
  
  my $folder = shift;
  my @ids = ();
  
  my @mails = &read_mailbox($folder);

  my $message;
  foreach $message (@mails){

    my ($header, $body) = &extract_header_body ($message);

    $header = &add_message_id_if_needed($header);
       
    my $id = extract_message_id($header);

    push (@ids, $id);
  }

  return @ids;
}
