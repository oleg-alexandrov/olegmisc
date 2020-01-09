// __BEGIN_LICENSE__
//  Copyright (c) 2006-2013, United States Government as represented by the
//  Administrator of the National Aeronautics and Space Administration. All
//  rights reserved.
//
//  The NASA Vision Workbench is licensed under the Apache License,
//  Version 2.0 (the "License"); you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
// __END_LICENSE__


/// \file dump_ip.cc
///
/// Dump the subscaled left and right images and the interest points.

#include <vw/Core/FundamentalTypes.h>
#include <vw/Core/Log.h>
#include <vw/Math/BBox.h>
#include <vw/Math/Geometry.h>
#include <vw/Math/RANSAC.h>
#include <vw/Image/AlgorithmFunctions.h>
#include <vw/Image/Algorithms.h>
#include <vw/Image/ImageIO.h>
#include <vw/Image/ImageView.h>
#include <vw/Image/Manipulation.h>
#include <vw/Image/MaskViews.h>
#include <vw/FileIO/DiskImageView.h>
#include <vw/Mosaic/ImageComposite.h>
#include <vw/Camera/CameraGeometry.h>
#include <vw/InterestPoint/InterestData.h>
#include <vw/InterestPoint/Matcher.h>

#include <vector>
#include <string>
#include <sstream>
#include <iostream>

using namespace vw;
using namespace vw::ip;

#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
namespace po = boost::program_options;

#include <boost/filesystem/path.hpp>
namespace fs = boost::filesystem;

// Draw the two images side by side with matching interest points
// shown with lines.
static void write_match_image(std::string const& out_file_name,
                              std::string const& file1,
                              std::string const& file2,
                              std::vector<InterestPoint> const& matched_ip1,
                              std::vector<InterestPoint> const& matched_ip2) {
  // Skip image pairs with no matches.
  if (matched_ip1.empty())
    return;

  boost::scoped_ptr<vw::DiskImageResource> irsrc1( DiskImageResource::open(file1) );
  boost::scoped_ptr<vw::DiskImageResource> irsrc2( DiskImageResource::open(file2) );

  // Work out the scaling to produce the subsampled images. These
  // values are choosen just allow a reasonable rendering time.
  float sub_scale =
    sqrt(1500.0 * 1500.0 / float(irsrc1->format().cols * irsrc1->format().rows));
  sub_scale +=
    sqrt(1500.0 * 1500.0 / float(irsrc2->format().cols * irsrc2->format().rows));
  sub_scale /= 2;
  if ( sub_scale > 1 ) sub_scale = 1;

  mosaic::ImageComposite<PixelRGB<uint8> > composite;
  if ( irsrc1->has_nodata_read() ) {
    composite.insert( pixel_cast_rescale<PixelRGB<uint8> >(resample(apply_mask(normalize(create_mask(DiskImageView<PixelGray<float> >(*irsrc1),
                                                                                                     irsrc1->nodata_read()))), sub_scale)),
                      0, 0 );
  } else {
    composite.insert( pixel_cast_rescale<PixelRGB<uint8> >(resample(normalize(DiskImageView<PixelGray<float> >(*irsrc1)), sub_scale)),
                      0, 0 );
  }
  if ( irsrc2->has_nodata_read() ) {
    composite.insert(pixel_cast_rescale<PixelRGB<uint8> >(resample(apply_mask(normalize(create_mask(DiskImageView<PixelGray<float> >(*irsrc2),
                                                                                                    irsrc2->nodata_read()))), sub_scale)),
                     int32(irsrc1->format().cols * sub_scale), 0 );
  } else {
    composite.insert(pixel_cast_rescale<PixelRGB<uint8> >(resample(normalize(DiskImageView<PixelGray<float> >(*irsrc2)), sub_scale)),
                     int32(irsrc1->format().cols * sub_scale), 0 );
  }
  composite.set_draft_mode( true );
  composite.prepare();

  // Rasterize the composite so that we can draw on it.
  ImageView<PixelRGB<uint8> > comp = composite;

  // Draw a red line between matching interest points
  for (size_t k = 0; k < matched_ip1.size(); ++k) {
    Vector2f start(matched_ip1[k].x, matched_ip1[k].y);
    Vector2f end(matched_ip2[k].x+irsrc1->format().cols, matched_ip2[k].y);
    start *= sub_scale;
    end   *= sub_scale;
    float inc_amt = 1/norm_2(end-start);
    for (float r=0; r<1.0; r+=inc_amt ){
      int i = (int)(0.5 + start.x() + r*(end.x()-start.x()));
      int j = (int)(0.5 + start.y() + r*(end.y()-start.y()));
      if (i >=0 && j >=0 && i < comp.cols() && j < comp.rows())
        comp(i,j) = PixelRGB<uint8>(255, 0, 0);
    }
  }

  boost::scoped_ptr<vw::DiskImageResource> rsrc( DiskImageResource::create(out_file_name, comp.format()) );
  block_write_image( *rsrc, comp,
                     TerminalProgressCallback( "tools.ipmatch", "Writing Debug:" ) );
}

int main(int argc, char** argv) {
  std::vector<std::string> input_file_names;

  po::options_description general_options("Options");

  po::options_description hidden_options("");
  hidden_options.add_options()
    ("input-files", po::value<std::vector<std::string> >(&input_file_names));

  po::options_description options("Allowed Options");
  options.add(general_options).add(hidden_options);

  po::positional_options_description p;
  p.add("input-files", -1);

  std::ostringstream usage;
  usage << "Usage: " << argv[0] << " <image1> <image2> <output-prefix>" << std::endl << std::endl;
  usage << general_options << std::endl;

  po::variables_map vm;
  try {
    po::store( po::command_line_parser( argc, argv ).options(options).positional(p).run(), vm );
    po::notify( vm );
  } catch (const po::error& e) {
    std::cout << "An error occured while parsing command line arguments.\n";
    std::cout << "\t" << e.what() << "\n\n";
    std::cout << usage.str();
    return 1;
  }

  if( vm.count("help") ) {
    vw_out() << usage.str();
    return 1;
  }

  if( input_file_names.size() < 3 ) {
    vw_out() << "Error: Must specify at least two input files and output prefix!"
             << std::endl << std::endl;
    vw_out() << usage.str();
    return 1;
  }

  std::string out_prefix = input_file_names[2];
  std::string match_filename
    = ip::match_filename(out_prefix, input_file_names[0], input_file_names[1]);
  std::vector<ip::InterestPoint> left_ip, right_ip;
  ip::read_binary_match_file( match_filename, left_ip, right_ip  );

  std::string imageName = out_prefix + "-ip.tif";
  vw_out() << "Writing: " << imageName << std::endl;
  write_match_image(imageName,
                    input_file_names[0], input_file_names[1],
                    left_ip, right_ip);
  
  return 0;
}
