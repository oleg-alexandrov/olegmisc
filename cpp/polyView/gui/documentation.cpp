// MIT License Terms (http://en.wikipedia.org/wiki/MIT_License)
// 
// Copyright (C) 2011 by Oleg Alexandrov
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
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
char docText[] ="<div style=\"color:rgb(0, 0, 0)\">\n"
"<div dir=\"ltr\">\n"
"<div style=\"background-color:rgb(255, 255, 255)\" align=\"center\"><a href=\":pvLogo.png?attredirects=0\" imageanchor=\"1\"><img alt=\"PolyView logo\" src=\":pvLogo.png\" border=\"0\"></a></div>\n"
"<div style=\"background-color:rgb(255, 255, 255)\" align=\"left\">\n"
"<br>\n"
"<br>\n"
"<b>PolyView</b> is a free and open source software program for Linux designed to quickly load and visualize multiple sets of polygon files from the command line. It can zoom and pan, show polygons as edges, points, and filled, display text labels, print the coordinates of vertices and measure distances, change the order in which polygons are displayed, choose which polygons to show, etc.<br>\n"
"<br>\n"
"PolyView can also do basic polygon editing, such as creating/deleting/moving/rotating/scaling polygons, adding/removing/modifying vertices, as well as cutting polygons to a box.<br>\n"
"<br>\n"
"PolyView accepts a lengthy list of command line arguments, and the view can be also manipulated interactively from the GUI once the polygons are loaded. A complete list of features and functions is shown below.<br>\n"
"<br>\n"
"PolyView is implemented in C++ using Qt 4. Its source code is freely available under the MIT license and can be used for any purpose, academic or commercial.\n"
"\n"
"<h2>Download</h2>\n"
"<ul>\n"
"<li><a href=\"https://sites.google.com/site/polyview2d/polyview_0.5.deb?attredirects=0\">polyview_0.5.deb</a> - Ubuntu 10.10 Debian package</li>\n"
"<ul><li>depends on the Qt4 run-time libraries</li>\n"
"<li>install with: dpkg -i polyview_0.5.deb</li>\n"
"<li>path to executable is /usr/bin/polyview</li>\n"
"<li>remove with: <span>dpkg -r polyview</span></li></ul></ul>\n"
"</div>\n"
"<div style=\"background-color:rgb(255, 255, 255)\" align=\"left\">\n"
"<ul><li><a href=\"https://sites.google.com/site/polyview2d/polyview.tgz?attredirects=0\">polyview.tgz</a><span> - C++ source code</span></li>\n"
"<ul><li><span>can be compiled on Linux and Mac OS X, and in principle on Microsoft Windows as well</span></li>\n"
"<li><span>depends on the Qt4 development libraries, which can be installed on Ubuntu with command: apt-get install qmake\n"
"</span></li></ul>\n"
"</ul>\n"
"</div>\n"
"<div style=\"background-color:rgb(255, 255, 255)\" align=\"left\">\n"
"<h2>Documentation</h2>\n"
"<h3>File format</h3>\n"
"PolyView was inspired by <a href=\"http://www.xgraph.org/xgraph.html\">xgraph</a>,\n"
"and it uses xgraph's file format, in which the x and y coordinates of \n"
"each polygon are stored as two columns in a plain text file followed by \n"
"the \"NEXT\" statement. Here is a sample polygon file storing a rectangle \n"
"and a triangle.\n"
"</div>\n"
"\n"
"<br>\n"
"<div style=\"background-color:rgb(255, 255, 255)\" align=\"left\">\n"
"<div style=\"margin-left:40px\">color = red<br>\n"
"60 50<br>\n"
"70 50<br>\n"
"70 70<br>\n"
"60 70<br>\n"
"60 50<br>\n"
"NEXT<br>\n"
"color = green<br>\n"
"80 30<br>\n"
"90 30<br>\n"
"80 40<br>\n"
"80 30<br>\n"
"NEXT<br>\n"
"<br>\n"
"</div>\n"
"</div>\n"
"\n"
"Notice that the first vertex of each polygon is repeated again before \n"
"the \"NEXT\" statement, to signal to the viewer that the polygon is \n"
"closed. \n"
"\n"
"\n"
"<h3>Features of PolyView accessible from the GUI</h3>\n"
"<h4>Mouse buttons</h4>\n"
"\n"
"<ul><li>The left mouse button snaps to the closest polygon vertex and prints its coordinates. A subsequent click also prints the distance from the previous vertex to the current one.</li>\n"
"<li>The middle mouse button prints the coordinates of where the mouse clicked, without snapping to the closest vertex.</li>\n"
"<li>Dragging the mouse from lower-left to upper-right zooms in, and doing it in reverse zooms out.</li>\n"
"<li>Dragging the mouse while keeping the Control key pressed creates a highlight which can be used to cut the polygons to the highlight or to paste/move/delete them.\n"
"</li>\n"
"</ul>\n"
"\n"
"<h4>Command box</h4>\n"
"<ul><li>Many GUI operations (such as zoom) echo their action as a command in the terminal used to start PolyView. That command can be pasted in the command box at the bottom of the PolyView GUI to reproduce the original operation. This provides a basic level of scripting and reproducibility.</li></ul>\n"
"<h4>File menu</h4>\n"
"\n"
"<ul>\n"
"<li> Load a polygon file in addition to existing files\n"
"</li>\n"
"<li> Save the polygons as one file\n"
"</li>\n"
"<li> Save the polygons as individual files\n"
"</li>\n"
"<li> Overwrite the existing polygons\n"
"</li>\n"
"\n"
"</ul>\n"
"<h4>View menu</h4>\n"
"<ul>\n"
"\n"
"<li> Choose which files to hide/show\n"
"</li>\n"
"<li> Zoom and pan\n"
"</li>\n"
"<li> Reset the view to contain all polygons with a small padding\n"
"</li>\n"
"<li> Change the order in which the polygons are displayed\n"
"</li>\n"
"<li> Show/hide annotations (text labels)\n"
"</li>\n"
"<li> Show the polygons filled with color\n"
"</li>\n"
"<li> Show the vertices only (no edges)\n"
"</li>\n"
"<li> Show the index of each vertex\n"
"</li>\n"
"<li> Show the layer ids (if present) \n"
"</li></ul>\n"
"<div>\n"
"<h4>Edit menu</h4>\n"
"<ul>\n"
"<li> Undo and redo\n"
"</li>\n"
"<li> Create a polygon with integer vertices and edge angles multiple of 45 degrees\n"
"</li>\n"
"<li> Enforce integer vertices and edge angles multiple of 45 degrees\n"
"</li>\n"
"<li> Create a polygon with arbitrary angles\n"
"</li>\n"
"\n"
"</ul>\n"
"<h4>Transform menu\n"
"</h4>\n"
"<ul>\n"
"<li> Translate/rotate/scale the polygons\n"
"</li>\n"
"\n"
"</ul>\n"
"<h4>Selection menu\n"
"</h4>\n"
"<ul>\n"
"<li> Create a highlight (the polygons in the highlight are automatically selected and copied to a buffer)\n"
"</li>\n"
"<li> Cut the polygons to the current highlight\n"
"</li>\n"
"<li> Delete the selected polygons\n"
"</li>\n"
"<li> Paste the selected polygons\n"
"</li>\n"
"<li> Move the selected polygons (use Shift-Mouse)\n"
"</li>\n"
"<li> Deselect all polygons and delete all highlights\n"
"</li>\n"
"<li>Translate/rotate/scale/transform selected polygons<br>\n"
"</li>\n"
"\n"
"</ul>\n"
"<h4>Grid menu\n"
"</h4>\n"
"<ul>\n"
"<li> Show/hide the grid\n"
"</li>\n"
"<li> Enforce that all polygon edges have angles multiple of 45 degrees and snap the vertices to the grid\n"
"</li>\n"
"<li> Set the grid size\n"
"</li>\n"
"<li> Set the grid linewidth\n"
"</li>\n"
"<li> Set the grid color</li>\n"
"</ul>\n"
"\n"
"<h4> Diff menu</h4>\n"
"\n"
"<ul>\n"
"<li> Change the colors of polygons so that polygons from different files have different colors\n"
"</li>\n"
"<li> Enter diff mode (a mode in which two similar polygon files can be compared)\n"
"</li>\n"
"<li> Show the next/previous difference between given two given polygon files (starting with the largest difference)\n"
"</li>\n"
"</ul>\n"
"\n"
"<h4>Options menu</h4>\n"
"<ul>\n"
"<li> Set the linewidth of polygon edges\n"
"</li>\n"
"<li> Set the background color\n"
"</li>\n"
"</ul>\n"
"\n"
"<h4>Right-click menu</h4>\n"
"\n"
"<ul>\n"
"<li> Show and save a mark at the current point\n"
"</li>\n"
"<li> Use the nm scale when printing the coordinates of vertices \n"
"  (assuming that the polygons are at the dbu scale)\n"
"</li>\n"
"<li> Create a polygon with integer vertices and edge angles multiple of 45 degrees\n"
"</li>\n"
"<li> Enforce integer vertices and edge angles multiple of 45 degrees\n"
"</li>\n"
"<li> Create a polygon with arbitrary angles\n"
"</li>\n"
"<li> Delete the polygon at mouse cursor\n"
"</li>\n"
"<li> Enter align mode (a mode in which, given two polygon files, the second polygon file is kept fixed, while the first one can be interactively translated using Shift-Mouse and rotated/flipped from the right-click menu until it aligns to the second one)\n"
"</li>\n"
"<li> Enter move polygons mode (use Shift-Mouse to move a polygon; if some polygons are selected using a highlight, then all selected polygons will be moved)\n"
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
"\n"
"<h3>Features of PolyView accessible from the command line</h3>\n"
"PolyView will open simultaneously all polygon files supplied as inputs on the command line. Various command line options can modify how the polygons are displayed.\n"
"<br>\n"
"<table>\n"
"<tbody>\n"
"<tr>\n"
"<td>-h | -help</td>\n"
"<td>Show the available command line options</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-geo[metry] &lt;width&gt;x&lt;height&gt;</td>\n"
"<td>The window size in pixels (for example, 800x800)</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-bg | -backgroundColor&lt;color&gt;</td>\n"
"<td>Color of the background (the default is black)\n"
"\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-c | -color&lt;color&gt;</td>\n"
"<td>All polygons after this option will show up in the given color (the default is to use the colors specified in the polygon files)\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-fs | -fontSize&lt;integer&gt;</td>\n"
"<td>The text font size in pixels\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-lw | -lineWidth&lt;integer&gt;</td>\n"
"<td>All polygons after this option will show up with given linewidth\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-p | -points   </td>\n"
"<td>All polygons after this option will show up as vertices rather than edges (a subsequent -p option undoes this behavior)</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-cp | -closedPoly   </td>\n"
"<td>All polygons after this option will show up as closed (the last vertex is connected to the first one)</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-nc | -nonClosedPoly   </td>\n"
"<td>Interpret the polygons after this option as polygonal lines (the last vertex is not connected to the first one)\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-f | -filledPoly   </td>\n"
"<td>All polygons after this option will show up as filled\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-nf | -nonFilledPoly   </td>\n"
"<td>All polygons after this option will show up as not filled\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-grid on | off   </td>\n"
"<td>Turn on/off the grid display\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-gridSize&lt;integer&gt;</td>\n"
"<td>Grid size\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-gridWidth&lt;integer&gt;</td>\n"
"<td>Grid width in pixels\n"
"</td>\n"
"</tr>\n"
"<tr>\n"
"<td>-gridColor&lt;color&gt;</td>\n"
"<td>Grid color<br>\n"
"<br>\n"
"</td>\n"
"</tr>\n"
"</tbody>\n"
"</table>\n"
"<b>Author:</b> Oleg Alexandrov &lt;<a href=\"mailto:oleg.alexandrov@gmail.com\">oleg.alexandrov@gmail.com</a>&gt;\n"
"</div>\n"
"\n"
"</div>\n"
"</div>\n";
  // End auto-generated text
 return docText;
}
