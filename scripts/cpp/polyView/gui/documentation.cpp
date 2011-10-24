#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include "utils.h"
using namespace std;

std::string utils::getDocText(){

  // Begin auto-generated text
  char docText[] =
"<img src=\":xsample.png\">"    
"<br>\n"
"<br>\n"
"<h1>PolyView</h1>\n"
" is a free and open source software program for Linux designed to quickly load and\n"
" visualize multiple sets of polygon files from the command line. It can\n"
"zoom and pan, show polygons as edges, points, and filled, display text\n"
"labels, print the coordinates of vertices and measure distances,\n"
"change the order in which polygons are displayed, choose which polygons\n"
"to show/hide, etc.<br>\n"
"<br>\n"
"PolyView can also do basic polygon editing, such as creating/deleting/moving individual polygons and polygon sets, adding/removing/modifying vertices, as well as wholesale changes on the entire set of currently open polygons, such as cutting them to a given box or applying shift/scale/rotation\n"
"transformations.<br>\n"
"<br>\n"
"PolyView accepts a lengthy list of command line arguments, and the view can be also manipulated interactively from the GUI once the polygons are loaded. A complete list of features and functions is shown below.<br>\n"
"<br>\n"
"PolyView is implemented in C++ using Qt 4. Its source code is freely available under the MIT license as and can be used for any purpose, academic or commercial.<br>\n"
"\n"
"\n"
"<h2>Documentation</h2>\n"
"<h3>Features of polyView accessible from the GUI</h3>\n"
"<h4>Mouse buttons<br>\n"
"</h4>\n"
"<ul><li>The left mouse click snaps to the closest polygon vertex and prints its coordinates. A subsequent click also prints the distance from the previous vertex to the current one.</li>\n"
"<li>The middle mouse prints the coordinates of where the mouse clicked, without snapping to the closest vertex.</li>\n"
"<li>Dragging the mouse from lower-left to upper-right zooms in.</li>\n"
"<li>Dragging mouse from upper-right to lower-left zooms out.</li>\n"
"<li>Dragging the mouse while keeping the Control key pressed creates a highlight which can be used to cut the polygons to highlight or paste/move/delete them. <br>\n"
"</li></ul>\n"
"<h4>\n"
"File menu\n"
"</h4>\n"
"<ul>\n"
"<li> Load a polygon file in addition to existing files\n"
"</li>\n"
"<li> Save the polygons as one file\n"
"</li>\n"
"<li> Save the polygons as individual files\n"
"</li>\n"
"<li> Overwrite the existing polygons&nbsp; <br>\n"
"</li>\n"
"</ul>\n"
"<h4>View menu</h4>\n"
"\n"
"<ul>\n"
"\n"
"<li> Choose which files to show/hide\n"
"</li>\n"
"<li> Zoom and pan\n"
"</li>\n"
"<li> Reset the view to contain all polygons with a small padding\n"
"</li>\n"
"<li> Change the order in which the polygons are displayed\n"
"</li>\n"
"<li> Show/hide annotations\n"
"</li>\n"
"<li> Show polygons as filled with color\n"
"</li>\n"
"<li> Show vertices as points\n"
"</li>\n"
"<li> Show the index of each vertex\n"
"</li>\n"
"<li> Show the layer ids (if present) \n"
"</li></ul>\n"
"<div>\n"
"<h4>Edit menu\n"
"</h4>\n"
"<ul>\n"
"\n"
"<li> Undo and redo\n"
"</li>\n"
"<li> Create a polygon with integer vertices and edge angles multiple \n"
"  of 45 degrees\n"
"</li>\n"
"<li> Enforce integer vertices and edge angles multiple of 45 degrees\n"
"</li>\n"
"<li> Create a polygon with arbitrary angles <br>\n"
"</li>\n"
"\n"
"</ul>\n"
"<h4>Transform menu\n"
"</h4>\n"
"<ul>\n"
"<li> Translate/rotate/scale polygons <br>\n"
"</li>\n"
"\n"
"</ul>\n"
"<h4>Selection menu\n"
"</h4>\n"
"<ul>\n"
"<li> Create a highlight (the polygons in the highlight are automatically\n"
"  selected and copied to a buffer)\n"
"</li>\n"
"<li> Cut the polygons to the current highlight\n"
"</li>\n"
"<li> Delete the selected polygons\n"
"</li>\n"
"<li> Paste the selected polygons\n"
"</li>\n"
"<li> Move the selected polygons (use Shift-Mouse)\n"
"</li>\n"
"<li> Deselect all polygons and delete all highlights <br>\n"
"</li>\n"
"\n"
"</ul>\n"
"<h4>Grid menu\n"
"</h4>\n"
"<ul>\n"
"<li> Show/hide the grid\n"
"</li>\n"
"<li> Enforce that edges have angles multiple of 45 degrees and snap the \n"
"  vertices to the grid\n"
"</li>\n"
"<li> Set the grid size\n"
"</li>\n"
"<li> Set the grid linewidth\n"
"</li>\n"
"<li> Set the grid color</li>\n"
"\n"
"</ul>\n"
"<h4> Diff menu\n"
"</h4>\n"
"<ul>\n"
"<li> Change the colors of polygons so that polygons from different files \n"
"  have different colors\n"
"</li>\n"
"<li> Enter diff mode (a mode in which two similar polygon files can be \n"
"  compared)\n"
"</li>\n"
"<li> Show the next/previous difference between given two given polygon \n"
"  files (starting with the largest difference)\n"
"\n"
"Options menu\n"
"</li>\n"
"<li> Set the linewidth of polygon edges\n"
"</li>\n"
"<li> Set the background color <br>\n"
"</li>\n"
"\n"
"</ul>\n"
"<h4>Right-click menu\n"
"</h4>\n"
"<ul>\n"
"<li> Save/show mark at point\n"
"</li>\n"
"<li> Use the nm scale when printing the coordinates of vertices \n"
"  (assuming that the polygons are at the dbu scale)\n"
"</li>\n"
"<li> Create a polygon with integer vertices and edge angles multiple \n"
"  of 45 degrees\n"
"</li>\n"
"<li> Enforce integer vertices and edge angles multiple of 45 degrees\n"
"</li>\n"
"<li> Create a polygon with arbitrary angles\n"
"</li>\n"
"<li> Delete the polygon at mouse cursor\n"
"</li>\n"
"<li> Enter align mode (a mode in which, given two polygon files, the \n"
"  second polygon file is kept fixed, while the first one can be \n"
"  interactively shifted using Shift-Mouse and rotated/flipped from \n"
"  the right-click menu until it aligns to the second one)\n"
"</li>\n"
"<li> Enter move polygons mode (use Shift-Mouse to move a polygon)\n"
"</li>\n"
"<li> Enter move vertices mode (use Shift-Mouse to move a vertex)\n"
"</li>\n"
"<li> Enter move edges mode (use Shift-Mouse to move an edge)\n"
"</li>\n"
"<li> Insert a vertex on the edge closest to the mouse cursor\n"
"</li>\n"
"<li> Delete the vertex closest to the mouse cursor\n"
"</li>\n"
"<li> Copy the polygon closest to the mouse cursor\n"
"</li>\n"
"<li> Paste the polygon closest to the mouse cursor\n"
"</li>\n"
"<li> Reverse the orientation of the polygon closest to the mouse cursor\n"
"</li></ul>\n"
"<h3>Features of polyView accessible from the command line\n"
"   </h3>\n"
"\n"
"polyView will open simultaneously all polygon files supplied as inputs\n"
"on the command line. Various command line options can modify how the\n"
"polygons are displayed.<br>\n"
"<br>\n"
"<table>\n"
"<tbody>\n"
"<tr>\n"
"<td>-h | -help   </td>\n"
"<td>                        Show the available command line \n"
"                                     options\n"
"   \n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-geo[metry] (for example 1000x800)   </td>\n"
"<td> The window size in pixels\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-bg | -backgroundColor    </td>\n"
"<td> Color of the background (default is black)\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-c | -color    </td>\n"
"<td>          All polygons after this option will show\n"
"                                     up in the given color\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-fs | -fontSize    </td>\n"
"<td>         The text font size in pixels\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-lw | -lineWidth    </td>\n"
"<td>        All polygons after this option will show\n"
"                                     up with given linewidth\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-p | -points   </td>\n"
"<td>                      All polygons after this option will show\n"
"                                     up as points (a subsequent -p option undoes\n"
"                                     the previous one)\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-cp | -closedPoly   </td>\n"
"<td>                 All polygons after this option will show \n"
"                                     up as closed (the last vertex is connected\n"
"                                     to the first vertex)\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-nc | -nonClosedPoly   </td>\n"
"<td>              Interpret the vertices as belonging to a \n"
"                                     polygonal line rather than to a polygon\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-f | -filledPoly   </td>\n"
"<td>                  All polygons after this option will show\n"
"                                     up as filled\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-nf | -nonFilledPoly   </td>\n"
"<td>              All polygons after this option will show \n"
"                                     up as not filled\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-grid on | off   </td>\n"
"<td>                    Turn on/off the grid display\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-gridSize    </td>\n"
"<td>               Grid size\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-gridWidth    </td>\n"
"<td>              Grid width in pixels\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-gridColor    </td>\n"
"<td>                Grid color<br>\n"
"<br>\n"
"</td>\n"
"</tr>\n"
"</tbody>\n"
"</table>\n"
"Author's contact information: oleg.alexandrov at gmail.com<br>\n"
"<br>\n"
"</div>\n";
  // End auto-generated text
 return docText;
}










