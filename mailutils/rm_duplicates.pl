#!/usr/bin/perl
use strict;		  # 'strict' insists that all variables be declared
use diagnostics;	  # 'diagnostics' expands the cryptic warnings
use lib $ENV{HOME} . '/bin/mailutils';

require 'mail_utils.pl';

undef $/; # undefines the separator. Can read one whole file in one scalar.

MAIN: {

  my @files = @ARGV;
  my $file;

  # A hash that will contain all ids encountered in the mail,
  # together with the smallest size of all messages with that id
  my %ids;
  
  my ($id, $mail, @mails);

  foreach $file (@files){

    &read_mailbox($file, \@mails);

    foreach $mail (@mails){
      
      my $id = &extract_message_id($mail);

      # Find the message of smallest length with given id
      if ( ( !exists $ids{$id}                 ) ||
           ( length($ids{$id}) > length($mail) )
         ){
        
        $ids{$id} = $mail;
        
      }
      
    } # end visiting the current folder
    
  } # end going over mail folders

  my $out = "UniqueMails";
  print "Writing unique mails to $out\n";
  open(FILE, ">$out");
  foreach $id (keys %ids){
    print FILE $ids{$id} . "\n\n";
  }
  close(FILE);
  
}


