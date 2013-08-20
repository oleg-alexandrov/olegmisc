#!/usr/bin/perl

# Create a master kml file which points to individual
# kml files. This way there is just one kml file
# to put into Google Earth.
use LWP::Simple;

sub create_combined_kml{

  my $index = shift;
  my $urls  = shift;

  open(FILE, ">$index") || die "File $index does not exist\n";
  
  print "Writing: $index\n";

  # Try to find the target planet by fetching one of the kmls we want to combine
  my $success = 0;
  my $planet = "moon";
  foreach my $url (@$urls){
    my $text = get ($url);
    if ($text =~ /target=(\w+)/){
      $planet = $1;
      $success = 1;
      print "Got planet $planet from $url\n";
      last;
    }
  }
  
  print FILE '<?xml version="1.0" encoding="UTF-8"?>
<kml hint="target=' . $planet . '" xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
<Folder>
';

  foreach my $url (@$urls){
    my $name = $url;
    $name =~ s/^.*\/(.*?)\.kml$/$1/g;
    
    print FILE '<NetworkLink><name>' . $name . '</name><Link><href>'
       . $url
          . '</href><viewRefreshMode>onRegion</viewRefreshMode></Link></NetworkLink>'
             . "\n";
    
  }

  print FILE '</Folder>
</kml>
';

  close(FILE);
  
}

1;
