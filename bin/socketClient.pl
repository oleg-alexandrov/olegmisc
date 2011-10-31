#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
  use IO::Socket;

MAIN:{
  
#   if (scalar(@ARGV) < 2){
#     print "Usage: $0 \n";
#     exit(0);
#   }

  my $sock = new IO::Socket::INET (
                                   PeerAddr => 'oleg-laptop',
                                   PeerPort => '7070',
                                   Proto => 'tcp',
                                  );
    
  die "Could not create socket: $!\n" unless $sock;
  
  print "Enter message to send to server\n";
  my $line = <>; # ends in newline
  print "Will send: $line";
  print $sock "$line";
  close($sock);
  
}
