// __BEGIN_LICENSE__
//  Copyright (c) 2009-2013, United States Government as represented by the
//  Administrator of the National Aeronautics and Space Administration. All
//  rights reserved.
//
//  The NGT platform is licensed under the Apache License, Version 2.0 (the
//  "License"); you may not use this file except in compliance with the
//  License. You may obtain a copy of the License at
//  http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
// __END_LICENSE__


/// \file bundle_adjust.cc
///



#include <iostream>
#include <fstream>
#include <vw/Core.h>
#include <vw/Image.h>
#include <vw/FileIO.h>
#include <vw/Cartography.h>
#include <vw/Camera/PinholeModel.h>

/// \file PinholeModel2.h
///
/// This file contains the pinhole camera model.
///
#ifndef __VW_CAMERAMODEL_PINHOLE2_H__
#define __VW_CAMERAMODEL_PINHOLE2_H__

#include <vw/Math/Quaternion.h>
#include <vw/Camera/CameraModel.h>

#include <asp/Core/PointUtils.h>
#include <asp/Core/OrthoRasterizer.h>
#include <asp/Core/Macros.h>
#include <asp/Core/Common.h>

#include <iostream>
#include <fstream>

namespace vw {
namespace camera {

  class LensDistortion;

  /// This is a simple "generic" pinhole camera model.
  ///
  /// To specify the EXTRINSIC paramters of the camera, we specify the
  /// position of the camera center in the world frame (m_camera_center)
  /// and the pose (or orientation) of the camera in world frame (m_rotation)
  /// (which is the transformation from the camera's frame to the world frame).
  /// In the default Vision Workbench camera frame, the camera's pointing vector
  /// is the +z unit vector, and the image plane is aligned such that the
  /// positive x-pixel direction (increasing image columns) is the camera frame's
  /// +x vector, and the positive y-pixel direction (increasing image
  /// rows) is the frame's -y vector.  Note that this discrepancy in y
  /// frames is due to the fact that images stored in memory are most
  /// naturally indexed starting in the upper left hand corner.
  ///
  /// --->The user can re-define the direction of increasing x-pixel,
  ///     increasing y-pixel, and pointing vector by specifying
  ///     orthonormal vectors u,v,w. These are intended to simplify
  ///     movement between different camera coordinate conventions,
  ///     rather than encoding the complete rotation between world
  ///     and camera coordinate frames.
  ///
  /// The INTRINSIC portion of the camera matrix is nominally stored as
  ///
  ///    [  fx   0   cx  ]
  /// K= [  0   -fy  cy  ]
  ///    [  0    0   1   ]
  ///
  /// with fx, fy the focal length of the system (in horizontal and
  /// vertical pixels), and (cx, cy) the pixel offset of the
  /// principal point of the camera on the image plane. --Note that
  /// the default v direction is <0,1,0>, so
  /// K will be create with a POSITIVE fy term in the center; it
  /// becomes negative when multiplied with the v_direction vector).
  ///
  /// Combining both the intrinsic camera matrix K with the
  /// extrinsic matrices, (u,v,w rotation, R and C) we see that a real-world point (x,
  /// y, z), to pixel p in an image by:
  ///
  ///     [  row  ]         [ -u- ]               [ x ]
  /// p = [  col  ]  =  K * [ -v- ] * [R | -R C]  [ y ]
  ///     [   w   ]         [ -w- ]               [ z ]
  ///
  /// p is then in homogenous coordinates, so the w has to be divided
  /// out so that w=1. Here R and C are the extrinsic parameters; R and -R*C
  /// rotate and translate a vector in world coordinates into camera coordinates.
  ///
  ///
  ///  The Tsai distortion model describes radial and tangential lens distortion. See below.
  ///

  class PinholeModel2 : public CameraModel {
    typedef boost::shared_ptr<const LensDistortion> DistortPtr;

    DistortPtr m_distortion;
    Matrix<double,3,4> m_camera_matrix;

    // Stored for easy access.
    Vector3 m_camera_center;
    Matrix<double,3,3> m_rotation;
    Matrix<double,3,3> m_intrinsics;
    Matrix<double,3,4> m_extrinsics;

    // Intrinsic parameters, in pixel units
    double m_fu, m_fv, m_cu, m_cv;

    // Vectors that define how the coordinate system of the camera
    // relate to the directions: +u (increasing image columns), +v
    // (increasing image rows), and +w (out along optical axis)
    Vector3 m_u_direction;
    Vector3 m_v_direction;
    Vector3 m_w_direction;

    // Pixel Pitch, if the above units were not in pixels this should
    // convert it to that. For example, if distortion and focal length
    // have been described in mm. Pixel pitch would then be described
    // in mm/px.
    double m_pixel_pitch;

    // Cached values for pixel_to_vector
    Matrix<double,3,3> m_inv_camera_transform;

  public:
    //------------------------------------------------------------------
    // Constructors / Destructors
    //------------------------------------------------------------------

    /// Initialize an empty camera model.
    PinholeModel2();

    /// Initialize from a file on disk.
    PinholeModel2(std::string const& filename);

    /// Initialize the pinhole model with explicit parameters.
    ///
    /// The user supplies the basic intrinsic camera parameters:
    ///
    /// f_u : focal length (in units of pixels) in the u direction
    /// f_v : focal length (in units of pixels) in the v direction
    /// c_u : principal point offset (in pixels) in the u direction
    /// c_v : principal point offset (in pixels) in the v direction
    ///
    /// The direction vectors define how the coordinate system of the
    /// camera relate to the directions: +u (increasing image
    /// columns), +v (increasing image rows), and +w (complete the RH
    /// coordinate system with u and v -- points into the image)
    ///
    /// If you start from a focal length in a physical unit
    /// (e.g. meters), you can find the focal length in pixels by
    /// dividing by the pixel scale (usually in meters/pixel).
    ///
    /// Remember that the VW standard frame of reference is such that
    /// (0,0) is the upper left hand corner of the image and the v
    /// coordinates increase as you move down the image. There is an
    /// illustration in the VisionWorkbook.
    ///
    PinholeModel2(Vector3 camera_center, Matrix<double,3,3> rotation,
                 double f_u, double f_v, double c_u, double c_v,
                 Vector3 u_direction, Vector3 v_direction,
                 Vector3 w_direction,
                 LensDistortion const& distortion_model);

    /// Initialize the pinhole model with explicit parameters.
    ///
    /// The user supplies the basic intrinsic camera parameters:
    ///
    /// f_u : focal length (in units of pixels) in the u direction
    /// f_v : focal length (in units of pixels) in the v direction
    /// c_u : principal point offset (in pixels) in the u direction
    /// c_v : principal point offset (in pixels) in the v direction
    ///
    /// The direction vectors defining the coordinate system of the
    /// camera are set to default values in this version of the
    /// constructor:
    ///
    ///   +u (increasing image columns)                     =  +X   [1 0 0]
    ///   +v (increasing image rows)                        =  +Y   [0 -1 0]
    ///   +w (complete the RH coordinate system with
    ///       u and v -- points into the image)             =  +Z   [0 0 1]
    ///
    /// If you start from a focal length in a physical unit
    /// (e.g. meters), you can find the focal length in pixels by
    /// dividing by the pixel scale (usually in meters/pixel).
    ///
    /// Remember that the VW standard frame of reference is such that
    /// (0,0) is the upper left hand corner of the image and the v
    /// coordinates increase as you move down the image.
    ///
    PinholeModel2(Vector3 camera_center, Matrix<double,3,3> rotation,
                 double f_u, double f_v, double c_u, double c_v,
                 LensDistortion const& distortion_model);

    /// Construct a basic pinhole model with no lens distortion
    PinholeModel2(Vector3 camera_center, Matrix<double,3,3> rotation,
                 double f_u, double f_v,
                 double c_u, double c_v);

    virtual std::string type() const;
    virtual ~PinholeModel2();

    /// Read / Write a pinhole model from a file on disk.
    /// Files will end in format .pinhole
    void read(std::string const& filename);
    void write(std::string const& filename) const;

    /// DEPRECATED FILE IO
    void read_file(std::string const& filename) VW_DEPRECATED;
    void write_file(std::string const& filename) const VW_DEPRECATED;
    void read_old_file(std::string const& filename) VW_DEPRECATED;

    //------------------------------------------------------------------
    // Methods
    //------------------------------------------------------------------

    //  Computes the image of the point 'point' in 3D space on the
    //  image plane.  Returns a pixel location (col, row) where the
    //  point appears in the image.
    virtual Vector2 point_to_pixel(Vector3 const& point) const;

    // Is a valid projection of point is possible?
    // This is equal to: Is the point in front of the camera (z > 0)
    // after extinsic transformation?
    virtual bool projection_valid(Vector3 const& point) const;

    // Returns a (normalized) pointing vector from the camera center
    //  through the position of the pixel 'pix' on the image plane.
    virtual Vector3 pixel_to_vector (Vector2 const& pix) const;

    virtual Vector3 camera_center(Vector2 const& /*pix*/ = Vector2() ) const;
    void set_camera_center(Vector3 const& position);

    // Pose is a rotation which moves a vector in camera coordinates
    // into world coordinates.
    virtual Quaternion<double> camera_pose(Vector2 const& /*pix*/ = Vector2() ) const;
    void set_camera_pose(Quaternion<double> const& pose);
    void set_camera_pose(Matrix<double,3,3> const& pose);

    //  u_direction, v_direction, and w_direction define how the coordinate
    //  system of the camera relate to the directions in the image:
    //  +u (increasing image columns),
    //  +v (increasing image rows), and
    //  +w (pointing away from the focal point in the direction of the imaged object).
    //
    //  All three vectors must be of unit length.
    void coordinate_frame(Vector3 &u_vec, Vector3 &v_vec, Vector3 &w_vec) const;
    void set_coordinate_frame(Vector3 u_vec, Vector3 v_vec, Vector3 w_vec);

    // Redundant...
    Vector3 coordinate_frame_u_direction() const;
    Vector3 coordinate_frame_v_direction() const;
    Vector3 coordinate_frame_w_direction() const;

    const LensDistortion* lens_distortion() const;
    void set_lens_distortion(LensDistortion const& distortion);

    //  f_u and f_v :  focal length in horiz and vert. pixel units
    //  c_u and c_v :  principal point in pixel units
    void intrinsic_parameters(double& f_u, double& f_v,
                              double& c_u, double& c_v) const VW_DEPRECATED;
    void set_intrinsic_parameters(double f_u, double f_v,
                                  double c_u, double c_v) VW_DEPRECATED;

    Vector2 focal_length() const;
    void set_focal_length(Vector2 const& f, bool rebuild=true );

    Vector2 point_offset() const;
    void set_point_offset(Vector2 const& c, bool rebuild=true );

    double pixel_pitch() const;
    void set_pixel_pitch( double pitch );

    // Ingest camera matrix
    // This performs a camera matrix decomposition and rewrites most variables
    void set_camera_matrix( Matrix<double,3,4> const& p );

    Matrix<double,3,4> camera_matrix() const;

  private:
    /// This must be called whenever camera parameters are modified.
    void rebuild_camera_matrix();
  };

  //   /// Given two pinhole camera models, this method returns two new camera
  //   /// models that have been epipolar rectified.
  //   template <>
  //   void epipolar(PinholeModel2<NoLensDistortion> const& src_camera0,
  //                 PinholeModel2<NoLensDistortion> const& src_camera1,
  //                 PinholeModel2<NoLensDistortion> &dst_camera0,
  //                 PinholeModel2<NoLensDistortion> &dst_camera1);

  PinholeModel2 scale_camera(PinholeModel2 const& camera_model,
                            float scale);
  PinholeModel2 linearize_camera(PinholeModel2 const& camera_model);

  std::ostream& operator<<(std::ostream& str, PinholeModel2 const& model){
    //std::ostream& vw::camera::operator<<(std::ostream& str,
    //                                   PinholeModel2 const& model) {
    str << "Pinhole camera: \n";
    str << "\tCamera Center: " << model.camera_center() << "\n";
    str << "\tRotation Matrix: " << model.camera_pose() << "\n";
    str << "\tIntrinsics:\n";
    str << "\t  focal: " << model.focal_length() << "\n";
    str << "\t  offset: " << model.point_offset() << "\n";

    str << "\tu direction: " << model.coordinate_frame_u_direction() << "\n";
    str << "\tv direction: " << model.coordinate_frame_v_direction() << "\n";
    str << "\tw direction: " << model.coordinate_frame_w_direction() << "\n";

    //str << "\tDistortion Model: " << model.lens_distortion()->name() << "\n";
    //str << "\t  " << *(model.lens_distortion()) << "\n";

    return str;
  }



}}      // namespace vw::camera

#include <vw/Core/Log.h>
#include <vw/config.h>
#include <vw/Math/Matrix.h>
#include <vw/Math/Vector.h>
#include <vw/Math/Quaternion.h>
#include <vw/Camera/PinholeModel.h>
#include <vw/Camera/LensDistortion.h>

#if defined(VW_HAVE_PKG_LAPACK) && VW_HAVE_PKG_LAPACK==1
#include <vw/Math/LinearAlgebra.h>
#endif

#include <algorithm>
#include <sstream>
#include <string>

#if defined(VW_HAVE_PKG_PROTOBUF) && VW_HAVE_PKG_PROTOBUF==1
#include <vw/Camera/TsaiFile.pb.h>
using google::protobuf::RepeatedFieldBackInserter;
#endif

#include <boost/filesystem/convenience.hpp>
namespace fs = boost::filesystem;

namespace vw { namespace camera {

  using namespace vw;
  using namespace camera;

PinholeModel2::PinholeModel2() : m_distortion(DistortPtr(new NullLensDistortion)),
                               m_camera_center(Vector3(0,0,0)),
                               m_fu(1), m_fv(1), m_cu(0), m_cv(0),
                               m_u_direction(Vector3(1,0,0)),
                               m_v_direction(Vector3(0,1,0)),
                               m_w_direction(Vector3(0,0,1)), m_pixel_pitch(1) {

  m_rotation.set_identity();
  this->rebuild_camera_matrix();
}

/// Initialize from a file on disk.
PinholeModel2::PinholeModel2(std::string const& filename) : m_distortion(DistortPtr(new NullLensDistortion)) {
  read(filename);
}

PinholeModel2::PinholeModel2(Vector3 camera_center, Matrix<double,3,3> rotation,
                           double f_u, double f_v, double c_u, double c_v,
                           Vector3 u_direction, Vector3 v_direction,
                           Vector3 w_direction,
                           LensDistortion const& distortion_model) : m_distortion(DistortPtr(distortion_model.copy())),
                                                                     m_camera_center(camera_center),
  m_rotation(rotation),
                                                                     m_fu(f_u), m_fv(f_v), m_cu(c_u), m_cv(c_v),
  m_u_direction(u_direction),
  m_v_direction(v_direction),
  m_w_direction(w_direction),
  m_pixel_pitch(1) {
  this->rebuild_camera_matrix();
}

PinholeModel2::PinholeModel2(Vector3 camera_center, Matrix<double,3,3> rotation,
                           double f_u, double f_v, double c_u, double c_v,
                           LensDistortion const& distortion_model) : m_distortion(DistortPtr(distortion_model.copy())),
                                                                     m_camera_center(camera_center),
                                                                     m_rotation(rotation),
  m_fu(f_u), m_fv(f_v), m_cu(c_u), m_cv(c_v),
                                                                     m_u_direction(Vector3(1,0,0)),
  m_v_direction(Vector3(0,1,0)),
  m_w_direction(Vector3(0,0,1)),
  m_pixel_pitch(1) {
  rebuild_camera_matrix();
}


/// Construct a basic pinhole model with no lens distortion
PinholeModel2::PinholeModel2(Vector3 camera_center, Matrix<double,3,3> rotation,
                           double f_u, double f_v,
                           double c_u, double c_v) : m_distortion(DistortPtr(new NullLensDistortion)),
                                                     m_camera_center(camera_center),
                                                     m_rotation(rotation),
                                                     m_fu(f_u), m_fv(f_v), m_cu(c_u), m_cv(c_v),
                                                     m_u_direction(Vector3(1,0,0)),
  m_v_direction(Vector3(0,1,0)),
  m_w_direction(Vector3(0,0,1)),
  m_pixel_pitch(1) {
  rebuild_camera_matrix();
}

std::string PinholeModel2::type() const { return "Pinhole"; }
PinholeModel2::~PinholeModel2() {}


// Old deprecated format of Pinhole I/O. Didn't support all distortion options.
// Reads in a file containing parameters of a pinhole model with
// a tsai lens distortion model. An example is provided at the end of this file.
void PinholeModel2::read_old_file(std::string const& filename) {

  char line[2048];
  double fu, fv, cu, cv;
  Vector3 u_direction, v_direction, w_direction;
  Vector3 C;
  Matrix3x3 R;
  Vector4 distortion_params(0,0,0,0);


  FILE *cam_file = fopen(filename.c_str(), "r");
  if (cam_file == 0) vw_throw( IOErr() << "PinholeModel2::read_file: Could not open file\n" );

  // Read intrinsic parameters
  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"fu = %lf", &fu) != 1) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read x focal length\n" );
  }

  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"fv = %lf", &fv) != 1) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read y focal length\n" );
  }

  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"cu = %lf", &cu) != 1) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read x principal point\n" );
  }

  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"cv = %lf", &cv) != 1) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read y principal point\n" );
  }

  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"u_direction = %lf %lf %lf", &u_direction(0), &u_direction(1), &u_direction(2)) != 3) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read u direction vector\n" );
  }

  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"v_direction = %lf %lf %lf", &v_direction(0), &v_direction(1), &v_direction(2)) != 3) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read v direction vector\n" );
  }

  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"w_direction = %lf %lf %lf", &w_direction(0), &w_direction(1), &w_direction(2)) != 3) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read w direction vector\n" );
  }

  // Read extrinsic parameters
  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"C = %lf %lf %lf", &C(0), &C(1), &C(2)) != 3) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file: Could not read C (camera center) vector\n" );
  }

  if ( !fgets(line, sizeof(line), cam_file) ||
       sscanf(line, "R = %lf %lf %lf %lf %lf %lf %lf %lf %lf",
              &R(0,0), &R(0,1), &R(0,2),
              &R(1,0), &R(1,1), &R(1,2),
              &R(2,0), &R(2,1), &R(2,2)) != 9 ) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read rotation matrix\n" );
  }

  // Read distortion parameters.
  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"k1 = %lf", &distortion_params[0] ) != 1) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read tsai distortion parameter k1\n" );
  }

  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"k2 = %lf", &distortion_params[1] ) != 1) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read tsai distortion parameter k2\n" );
  }

  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"p1 = %lf", &distortion_params[2] ) != 1) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read tsai distortion parameter p1\n" );
  }

  if (!fgets(line, sizeof(line), cam_file) ||
      sscanf(line,"p2 = %lf", &distortion_params[3] ) != 1) {
    fclose(cam_file);
    vw_throw( IOErr() << "PinholeModel2::read_file(): Could not read tsai distortion parameter p2\n" );
  }

  fclose(cam_file);

  m_u_direction = u_direction;
  m_v_direction = v_direction;
  m_w_direction = w_direction;
  m_pixel_pitch = 1;

  m_fu = fu;
  m_fv = fv;
  m_cu = cu;
  m_cv = cv;
  m_camera_center = C;

  m_rotation = R;
  this->rebuild_camera_matrix();

  if( distortion_params == Vector4(0,0,0,0))
    m_distortion.reset(new NullLensDistortion());
  else
    m_distortion.reset(new TsaiLensDistortion(distortion_params));
}


// Reads in a file containing parameters of a pinhole model with
// a tsai lens distortion model.
void PinholeModel2::read_file(std::string const& filename) {
  this->read(filename);
}

void PinholeModel2::read(std::string const& filename) {

  fs::path filename_path( filename );
  if ( filename_path.extension() == ".pinhole" ) {
#if defined(VW_HAVE_PKG_PROTOBUF) && VW_HAVE_PKG_PROTOBUF==1
    std::fstream input( filename.c_str(), std::ios::in | std::ios::binary );
    if ( !input )
      vw_throw( IOErr() << "Pinhole::read_file: Could not open " << filename << "\n" );
    TsaiFile file;
    if ( !file.ParseFromIstream( &input ) )
      vw_throw( IOErr() << "Pinhole::read_file: Protocol buffer failed to parse \"" << filename << "\"\n" );
    input.close();

    // Making sure protobuf seems correct
    VW_ASSERT( file.focal_length_size() == 2,
               IOErr() << "Pinhole::read_file: Unexpected amount of focal lengths." );
    VW_ASSERT( file.center_point_size() == 2,
               IOErr() << "Pinhole::read_file: Unexpected amount of center points." );
    VW_ASSERT( file.u_direction_size() == 3,
               IOErr() << "Pinhole::read_file: Unexpected size of u vector." );
    VW_ASSERT( file.v_direction_size() == 3,
               IOErr() << "Pinhole::read_file: Unexpected size of v vector." );
    VW_ASSERT( file.w_direction_size() == 3,
               IOErr() << "Pinhole::read_file: Unexpected size of w vector." );
    VW_ASSERT( file.camera_center_size() == 3,
               IOErr() << "Pinhole::read_file: Unexpected size of camera vector." );
    VW_ASSERT( file.camera_rotation_size() == 9,
               IOErr() << "Pinhole::read_file: Unexpected size of rotation matrix." );

    typedef VectorProxy<double,3> Vector3P;
    m_u_direction = Vector3P(file.mutable_u_direction()->mutable_data());
    m_v_direction = Vector3P(file.mutable_v_direction()->mutable_data());
    m_w_direction = Vector3P(file.mutable_w_direction()->mutable_data());
    m_camera_center = Vector3P(file.mutable_camera_center()->mutable_data());
    m_fu = file.focal_length(0);
    m_fv = file.focal_length(1);
    m_cu = file.center_point(0);
    m_cv = file.center_point(1);
    m_rotation = MatrixProxy<double,3,3>(file.mutable_camera_rotation()->mutable_data());
    m_pixel_pitch = file.pixel_pitch();

    this->rebuild_camera_matrix();

    if ( file.distortion_name() == "NULL" ) {
      VW_ASSERT( file.distortion_vector_size() == 0,
                 IOErr() << "Pinhole::read_file: Unexpected distortion vector." );
      m_distortion.reset( new NullLensDistortion());
    } else if ( file.distortion_name() == "TSAI" ) {
      VW_ASSERT( file.distortion_vector_size() == 4,
                 IOErr() << "Pinhole::read_file: Unexpected distortion vector." );
      m_distortion.reset( new TsaiLensDistortion(VectorProxy<double,4>(file.mutable_distortion_vector()->mutable_data())));
    } else if ( file.distortion_name() == "BROWNCONRADY" ) {
      VW_ASSERT( file.distortion_vector_size() == 8,
                 IOErr() << "Pinhole::read_file: Unexpected distortion vector." );
      m_distortion.reset( new BrownConradyDistortion(VectorProxy<double,8>(file.mutable_distortion_vector()->mutable_data())));
    } else if ( file.distortion_name() == "AdjustableTSAI" ) {
      VW_ASSERT( file.distortion_vector_size() > 3,
                 IOErr() << "Pinhole::read_file: Unexpected distortion vector." );
      m_distortion.reset( new AdjustableTsaiLensDistortion(VectorProxy<double>(file.distortion_vector_size(),file.mutable_distortion_vector()->mutable_data())));
    }
#else
    // If you hit this point, you need to install Google Protobuffers to
    // be in order to write.
    vw_throw( IOErr() << "Pinhole::write_file: Camera IO not supported without Google Protobuffers" );
#endif
  } else if ( filename_path.extension() == ".tsai" ) {
    this->read_old_file( filename );
  } else {
    vw_throw( IOErr() << "Unknown PinholeModel2 filename extension \""
              << filename_path.extension() << "\"" );
  }
}


// Write parameters of an exiting PinholeModel2 into a .tsai file for later use.
void PinholeModel2::write_file(std::string const& filename) const {
  write(filename);
}
void PinholeModel2::write(std::string const& filename) const {
#if defined(VW_HAVE_PKG_PROTOBUF) && VW_HAVE_PKG_PROTOBUF==1
  std::string output_file =
    fs::path(filename).replace_extension(".pinhole").string();

  TsaiFile file;
  file.add_focal_length( m_fu );
  file.add_focal_length( m_fv );
  file.add_center_point( m_cu );
  file.add_center_point( m_cv );
  file.set_pixel_pitch( m_pixel_pitch );

  std::copy(m_u_direction.begin(), m_u_direction.end(),
            RepeatedFieldBackInserter(file.mutable_u_direction()));
  std::copy(m_v_direction.begin(), m_v_direction.end(),
            RepeatedFieldBackInserter(file.mutable_v_direction()));
  std::copy(m_w_direction.begin(), m_w_direction.end(),
            RepeatedFieldBackInserter(file.mutable_w_direction()));
  std::copy(m_camera_center.begin(), m_camera_center.end(),
            RepeatedFieldBackInserter(file.mutable_camera_center()));

  std::copy(m_rotation.begin(), m_rotation.end(),
            RepeatedFieldBackInserter(file.mutable_camera_rotation()));

  file.set_distortion_name( m_distortion->name() );
  Vector<double> distort_vec = m_distortion->distortion_parameters();
  std::copy(distort_vec.begin(),distort_vec.end(),
            RepeatedFieldBackInserter(file.mutable_distortion_vector()));

  std::ofstream output(output_file.c_str());
  if( !output.is_open() )
    vw_throw( IOErr() << "PinholeModel2::write_file: Could not open file\n" );
  file.SerializeToOstream( &output );
  output.close();
#else
  // If you hit this point, you need to install Google Protobuffers to
  // be in order to write.
  vw_throw( IOErr() << "Pinhole::write_file: Camera IO not supported without Google Protobuffers" );
#endif
}

Vector2 PinholeModel2::point_to_pixel(Vector3 const& point) const {

  //  Multiply the pixel location by the camera matrix.
  double denominator = m_camera_matrix(2,0)*point(0) + m_camera_matrix(2,1)*point(1) +
    m_camera_matrix(2,2)*point(2) + m_camera_matrix(2,3);
  Vector2 pixel = Vector2( (m_camera_matrix(0,0)*point(0) + m_camera_matrix(0,1)*point(1) +
                            m_camera_matrix(0,2)*point(2) + m_camera_matrix(0,3)) / denominator,
                           (m_camera_matrix(1,0)*point(0) + m_camera_matrix(1,1)*point(1) +
                            m_camera_matrix(1,2)*point(2) + m_camera_matrix(1,3)) / denominator);

  //  Apply the lens distortion model
  //return m_distortion->distorted_coordinates(*this, pixel)/m_pixel_pitch;
  return pixel;
}

bool PinholeModel2::projection_valid(Vector3 const& point) const {
  // z coordinate after extrinsic transformation
  double z = m_extrinsics(2, 0)*point(0) + m_extrinsics(2, 1)*point(1) +
    m_extrinsics(2, 2)*point(2) + m_extrinsics(2,3);
  return z > 0;
}

Vector3 PinholeModel2::pixel_to_vector (Vector2 const& pix) const {
  // Apply the inverse lens distortion model
  Vector2 undistorted_pix = pix; //m_distortion->undistorted_coordinates(*this, pix*m_pixel_pitch);

  // Compute the direction of the ray emanating from the camera center.
  Vector3 p(0,0,1);
  subvector(p,0,2) = undistorted_pix;
  return normalize( m_inv_camera_transform * p);
}

Vector3 PinholeModel2::camera_center(Vector2 const& /*pix*/ ) const {
  return m_camera_center;
};

void PinholeModel2::set_camera_center(Vector3 const& position) {
  m_camera_center = position; rebuild_camera_matrix();
}

Quaternion<double> PinholeModel2::camera_pose(Vector2 const& /*pix*/ ) const {
  return Quaternion<double>(m_rotation);
}

void PinholeModel2::set_camera_pose(Quaternion<double> const& pose) {
  m_rotation = pose.rotation_matrix(); rebuild_camera_matrix();
}

void PinholeModel2::set_camera_pose(Matrix<double,3,3> const& pose) {
  m_rotation = pose; rebuild_camera_matrix();
}

void PinholeModel2::coordinate_frame(Vector3 &u_vec, Vector3 &v_vec, Vector3 &w_vec) const {
  u_vec = m_u_direction;
  v_vec = m_v_direction;
  w_vec = m_w_direction;
}

void PinholeModel2::set_coordinate_frame(Vector3 u_vec, Vector3 v_vec, Vector3 w_vec) {
  m_u_direction = u_vec;
  m_v_direction = v_vec;
  m_w_direction = w_vec;

  rebuild_camera_matrix();
}

Vector3 PinholeModel2::coordinate_frame_u_direction() const { return m_u_direction; }
Vector3 PinholeModel2::coordinate_frame_v_direction() const { return m_v_direction; }
Vector3 PinholeModel2::coordinate_frame_w_direction() const { return m_w_direction; }

const LensDistortion* PinholeModel2::lens_distortion() const { return m_distortion.get(); };
void PinholeModel2::set_lens_distortion(LensDistortion const& distortion) {
  m_distortion = distortion.copy();
}

void PinholeModel2::intrinsic_parameters(double& f_u, double& f_v,
                                        double& c_u, double& c_v) const {
  f_u = m_fu;  f_v = m_fv;  c_u = m_cu;  c_v = m_cv;
}

void PinholeModel2::set_intrinsic_parameters(double f_u, double f_v,
                                            double c_u, double c_v) {
  m_fu = f_u;  m_fv = f_v;  m_cu = c_u;  m_cv = c_v;
  rebuild_camera_matrix();
}

Vector2 PinholeModel2::focal_length() const { return Vector2(m_fu,m_fv); }
void PinholeModel2::set_focal_length(Vector2 const& f, bool rebuild ) {
  m_fu = f[0]; m_fv = f[1];
  if (rebuild) rebuild_camera_matrix();
}
Vector2 PinholeModel2::point_offset() const { return Vector2(m_cu,m_cv); }
void PinholeModel2::set_point_offset(Vector2 const& c, bool rebuild ) {
  m_cu = c[0]; m_cv = c[1];
  if (rebuild) rebuild_camera_matrix();
}
double PinholeModel2::pixel_pitch() const { return m_pixel_pitch; }
void PinholeModel2::set_pixel_pitch( double pitch ) { m_pixel_pitch = pitch; }


void PinholeModel2::set_camera_matrix( Matrix<double,3,4> const& p ) {
#if defined(VW_HAVE_PKG_LAPACK) && VW_HAVE_PKG_LAPACK==1
  // Solving for camera center
  Matrix<double> cam_nullsp = nullspace(p);
  Vector<double> cam_center = select_col(cam_nullsp,0);
  cam_center /= cam_center[3];
  m_camera_center = subvector(cam_center,0,3);

  // Solving for intrinsics with RQ decomposition
  Matrix<double> M = submatrix(p,0,0,3,3);
  Matrix<double> R,Q;
  rqd( M, R, Q );
  Matrix<double> sign_fix(3,3);
  sign_fix.set_identity();
  if ( R(0,0) < 0 )
    sign_fix(0,0) = -1;
  if ( R(1,1) < 0 )
    sign_fix(1,1) = -1;
  if ( R(2,2) < 0 )
    sign_fix(2,2) = -1;
  R = R*sign_fix;
  Q = sign_fix*Q;
  R /= R(2,2);

  // Pulling out intrinsic and last extrinsic
  Matrix<double,3,3> uvwRotation;
  select_row(uvwRotation,0) = m_u_direction;
  select_row(uvwRotation,1) = m_v_direction;
  select_row(uvwRotation,2) = m_w_direction;
  m_rotation = inverse(uvwRotation*Q);
  m_fu = R(0,0);
  m_fv = R(1,1);
  m_cu = R(0,2);
  m_cv = R(1,2);

  if ( fabs(R(0,1)) >= 1.2 )
    vw_out(WarningMessage,"camera") << "Significant skew not modelled by pinhole camera\n";

  // Rebuild
  rebuild_camera_matrix();
#else
  vw_throw( NoImplErr() << "PinholeModel2::set_Camera_Matrix is unavailable without LAPACK" );
#endif
}

Matrix<double,3,4> PinholeModel2::camera_matrix() const {
  return m_camera_matrix;
}

void PinholeModel2::rebuild_camera_matrix() {

  /// The intrinsic portion of the camera matrix is stored as
  ///
  ///    [  fx   0   cx  ]
  /// K= [  0    fy  cy  ]
  ///    [  0    0   1   ]
  ///
  /// with fx, fy the focal length of the system (in horizontal and
  /// vertical pixels), and (cx, cy) the pixel coordinates of the
  /// central pixel (the principal point on the image plane).

  m_intrinsics(0,0) = m_fu;
  m_intrinsics(0,1) = 0;
  m_intrinsics(0,2) = m_cu;
  m_intrinsics(1,0) = 0;
  m_intrinsics(1,1) = m_fv;
  m_intrinsics(1,2) = m_cv;
  m_intrinsics(2,0) = 0;
  m_intrinsics(2,1) = 0;
  m_intrinsics(2,2) = 1;

  // The extrinsics are normally built as the matrix:  [ R | -R*C ].
  // To allow for user-specified coordinate frames, the
  // extrinsics are now build to include the u,v,w rotation
  //
  //               | u_0  u_1  u_2  |
  //     Extr. =   | v_0  v_1  v_2  | * [ R | -R*C]
  //               | w_0  w_1  w_2  |
  //
  // The vectors u,v, and w must be orthonormal.

  /*   check for orthonormality of u,v,w              */
  VW_LINE_ASSERT( dot_prod(m_u_direction, m_v_direction) == 0 );
  VW_LINE_ASSERT( dot_prod(m_u_direction, m_w_direction) == 0 );
  VW_LINE_ASSERT( dot_prod(m_v_direction, m_w_direction) == 0 );
  VW_LINE_ASSERT( fabs( norm_2(m_u_direction) - 1 ) < 0.001 );
  VW_LINE_ASSERT( fabs( norm_2(m_v_direction) - 1 ) < 0.001 );
  VW_LINE_ASSERT( fabs( norm_2(m_w_direction) - 1 ) < 0.001 );

  Matrix<double,3,3> uvwRotation;

  select_row(uvwRotation,0) = m_u_direction;
  select_row(uvwRotation,1) = m_v_direction;
  select_row(uvwRotation,2) = m_w_direction;

  Matrix<double,3,3> rotation_inverse = transpose(m_rotation);
  submatrix(m_extrinsics,0,0,3,3) = uvwRotation * rotation_inverse;
  select_col(m_extrinsics,3) = uvwRotation * -rotation_inverse * m_camera_center;

  m_camera_matrix = m_intrinsics * m_extrinsics;
  m_inv_camera_transform = inverse(uvwRotation*rotation_inverse) * inverse(m_intrinsics);
}


// scale_camera
//  Used to modify camera in the event to user resizes the image
  PinholeModel2 scale_camera(PinholeModel2 const& camera_model,
                                         float scale) {
  Vector2 focal = camera_model.focal_length();
  Vector2 offset = camera_model.point_offset();
  focal *= scale;
  offset *= scale;
  boost::shared_ptr<LensDistortion> lens = camera_model.lens_distortion()->copy();
  lens->scale( scale );
  return PinholeModel2( camera_model.camera_center(),
                        camera_model.camera_pose().rotation_matrix(),
                        focal[0], focal[1], offset[0], offset[1],
                        camera_model.coordinate_frame_u_direction(),
                        camera_model.coordinate_frame_v_direction(),
                        camera_model.coordinate_frame_w_direction(),
                        *lens
                        );
}

  //  std::ostream& operator<<(std::ostream& str, PinholeModel2 const& model);

}}

#endif  //__CAMERAMODEL_CAHV_H__


using namespace vw;
using namespace vw::camera;
using namespace vw::cartography;

int main( int argc, char**argv ){

  GeoReference G;
  std::string srs =  "+proj=stere +lat_0=90 +lat_ts=70 +Lon_0=-45 +k=1 +x_0=0 +ellipse=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0";
  bool have_user_datum = false;
  Datum user_datum;
  asp::set_srs_string(srs,
                      have_user_datum, user_datum, G);

  std::cout << "georef is " << G << std::endl;

  std::ifstream is(argv[1]);
  std::ofstream os(argv[2]);
  os.precision(18);
  double scale = atof(argv[3]);

  std::cout << "--scale is " << scale << std::endl;

  int id;
  double x, y, z, x1, y1, x2, y2;
  std::string file1, file2;
  while (is >> id >> x >> y >> z >> file1 >> x1 >> y1 >> file2 >> x2 >> y2){
    std::cout << "id x y z file1 x1 y1 file2 x2 y2 " << x << ' ' << y << ' ' << z << ' '
              << file1 << ' ' << x1 << ' ' << y1 << ' ' << file2 << ' ' << x2 << ' ' << y2 << std::endl;


    Vector3 P(x, y, z);
    Vector2 ll = G.point_to_lonlat(Vector2(x, y));
    std::cout << "lonlat is " << ll << std::endl;

    os << id  << ' ' << ll[1] << ' ' << ll[0] << ' ' << z << ' ' << 1 << ' ' << 1 << ' ' << 1 << ' ' << file1 << ' ' << x1*scale << ' ' << y1*scale << ' ' << 1 << ' ' << 1 << ' ' << file2 << ' ' << x2*scale << ' ' << y2*scale << ' ' << 1 << ' ' << 1 << std::endl;

    //0  77.2460527777778 -80.2234027777778 0 1 1 1 DZB1212-500082L002001_bb_20pct.tif 4940 5090 1 1 DZB1212-500082L003001_bb_20pct.tif 1047 5173 1 1

  }
}
