#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use IO::Socket;

MAIN:{
  
#   if (scalar(@ARGV) < 2){
#     print "Usage: $0 \n";
#     exit(0);
#   }
  
  
  my $sock = new IO::Socket::INET ( LocalHost => 'oleg-laptop',
                                    LocalPort => '7070',
                                    Proto => 'tcp',
                                    Listen => 1,
                                    Reuse => 1,
                                  );
  die "Could not create socket: $!\n" unless $sock;


  print "Listening\n";
  
  my $new_sock = $sock->accept();
  while(<$new_sock>) {
    print "Received the message: $_";
  }
  close($sock);
  
}
