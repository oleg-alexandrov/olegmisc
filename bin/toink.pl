#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{

  if (scalar(@ARGV) == 1){
    push(@ARGV, $ENV{HOME} . "/poly.svg");
  }
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 file.xg file.svg\n";
    exit(0);
  }

  my $file_xg = $ARGV[0];
  print "Reading $file_xg\n";

  my ($color, $layer, $center, @polys);
  get_polys($file_xg,                           # input
            \$color, \$layer, \$center, \@polys # outputs
           );

  my $out   = get_top();
  my $count = 0;
  foreach my $poly (@polys){
    $out .= get_path() . $poly . get_id($count);
    $count++;
  }
  $out .= get_bot();

  my $file_svg = $ARGV[1];
  print "Writing $file_svg\n";
  open(FILE, ">$file_svg"); print FILE $out; close(FILE);

  my $meta = "meta.txt";
  print "Writing meta info to $meta\n";
  open(FILE, ">$meta");
  print FILE "color    $color\n";
  print FILE "layer    $layer\n";
  print FILE "center   $center\n";
  print FILE "svg file $file_svg\n";
  close(FILE);
  
}

sub get_polys{

  my $file_xg = shift;
  my $color   = shift;
  my $layer   = shift;
  my $center  = shift;
  my $polys   = shift;
  
  open(FILE, "<$file_xg"); my $text = <FILE>; close(FILE);

  $text =~ s/[\!\#].*?\n/\n/g; # Wipe comments
  
  if ($text =~ /color\b.*?(\w+)/){
    $$color = $1;
  }else{
   $$color = "red"; 
 }
  
  $$layer = "";
  if ($text =~ /;\s*(\d+)\s*:\s*(\d+)/){
    $$layer = "$1:$2";
  }
  
  $text =~ s/color\s*=.*?\n//g;
  $text =~ s/\s*;.*?(\n|$)/$1/g;
  $text =~ s/anno.*?\n//g;
  
  my ($ctx, $cty);
  if ($text !~ /^([^\s]+?)\s+([^\s]+?)(\s|$)/){
    print "Failed to match the text!\n";
    exit(0);
  }
  $ctx = $1;
  $cty = $2;
  $ctx =~ s/\..*?$//g;
  $cty =~ s/\..*?$//g;
  
  $$center = "$ctx $cty";

  my $scale = 1;
  @$polys = split(/NEXT\s*/, $text);

  foreach my $poly (@$polys){
    $poly =~ s/(.*?)\s+(.*?)\n/($1 - $ctx)*$scale . " " . -($2 - $cty)*$scale . "\n"/eg;
    $poly =~ s/\s*$//g;
    $poly =~ s/^\s*//g;
    $poly =~ s/[ ]+/,/g;
    $poly =~ s/\n/ L /g;

    $poly = "M " . $poly . " z ";
    
  }
  
}

sub get_top{

  return '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->
<svg
   xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:cc="http://web.resource.org/cc/"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   width="744.09448819"
   height="1052.3622047"
   id="svg2"
   sodipodi:version="0.32"
   inkscape:version="0.45.1"
   sodipodi:docbase="Y:\"
   sodipodi:docname="drawing.svg"
   inkscape:output_extension="org.inkscape.output.svg.inkscape">
  <defs
     id="defs4" />
  <sodipodi:namedview
     id="base"
     pagecolor="#ffffff"
     bordercolor="#666666"
     borderopacity="1.0"
     gridtolerance="10000"
     guidetolerance="10"
     objecttolerance="10"
     inkscape:pageopacity="0.0"
     inkscape:pageshadow="2"
     inkscape:zoom="0.7"
     inkscape:cx="375"
     inkscape:cy="520"
     inkscape:document-units="px"
     inkscape:current-layer="layer1"
     inkscape:window-width="1280"
     inkscape:window-height="1005"
     inkscape:window-x="-4"
     inkscape:window-y="-4"
     showgrid="true" />
  <metadata
     id="metadata7">
    <rdf:RDF>
      <cc:Work
         rdf:about="">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource="http://purl.org/dc/dcmitype/StillImage" />
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g
     inkscape:label="Layer 1"
     inkscape:groupmode="layer"
     id="layer1">';
}

sub get_bot{

  return '  </g>
</svg>
';

}

sub get_path{
  
  return '
  <path
       style="fill:#c2bcc2;fill-opacity:0.74901961;stroke:#000000;stroke-width:1.5;stroke-linecap:square;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:4.5, 4.5;stroke-dashoffset:0;stroke-opacity:1"
       d="';
}

sub get_id{

  my $id = shift;
  return '"
       id="' . $id . '" />
';
 
}
