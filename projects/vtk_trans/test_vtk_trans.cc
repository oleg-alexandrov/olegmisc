// A test for VTK's thin plate transform in 2D. Based on code examples
// in the VTK repo.

/*=========================================================================

  Program:   Visualization Toolkit

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#include <vtkMath.h>
#include <vtkNew.h>
#include <vtkPoints.h>
#include <vtkThinPlateSplineTransform.h>

#include <iostream>

// Find the max error between two arrays of points. Note that
// cannot use const here as GetPoint() is not const.
double max_error(vtkNew<vtkPoints> & a, vtkNew<vtkPoints> & b) {

  double max_err = 0.0;
  int npoints = a->GetNumberOfPoints();

  for (int i = 0; i < npoints; i++) {
    double testDstPoint[3] = {0.0};
    a->GetPoint(i, testDstPoint);
    double transSrcPoint[3] = {0.0};
    b->GetPoint(i, transSrcPoint);
    double d = vtkMath::Distance2BetweenPoints(testDstPoint, transSrcPoint);
    max_err = std::max(max_err, d);
  }
  return max_err;
}

void TestThinPlateSplineTransform() {

  int npoints = 20;

  const double P[20][3] = {
    { -0.8316301300814422, -0.06992580859519772, -1.6034524068257419 },
    { -2.151893827785692, 0.38244721645095636, -0.9275967632551845 },
    { 0.8147291118075928, -0.7016483698682392, 0.15003863332602096 },
    { 0.918239421266975, 0.5515514723709805, -1.0230600499321258 },
    { -0.4977939747967184, 1.5000786176083494, 0.892455159403953 },
    { 2.137759080794324, -0.7876029858279091, 0.23676951564894347 },
    { 0.07659657475437548, 0.37528421293358666, 1.061745743663681 },
    { -0.7908820649026604, 1.4270955106455065, 2.2665387247459576 },
    { -0.5663930529602919, 1.9402635876094498, 1.1531767242062774 },
    { 0.22529528853908187, -1.5938090446587108, -0.7004997748768814 },
    { 0.6165064084492409, -0.2761336076050157, -0.7930056820043028 },
    { -1.6122391974605947, -1.4200010952872733, 1.0567292903013055 },
    { 0.17993263043615856, -0.9038514957133562, -2.1611068227229695 },
    { -1.4186794357559613, 0.85026116269838, -1.7600646313947719 },
    { 0.9690209792801024, 0.7018737798529897, 0.3923799957082836 },
    { -0.6586203767750309, -2.1468680342265904, 0.762954972139701 },
    { 1.2872860659137344, 0.8557080868402649, 0.3905931440107816 },
    { -0.18996464681200217, 0.8315184491297033, -1.0227889589485941 },
    { 1.0636210067525393, -0.24736478911115908, -0.7581101375259237 },
    { -0.09448165336394657, -1.1381967760924927, -0.7171168342666931 },
  };

  // Create the two point sets
  vtkNew<vtkPoints> srcPoints, dstPoints;

  for (int i = 0; i < npoints; i++) {

    // Input
    double src[3] = {P[i][0], P[i][1], 0};

    // Output, model a nonlinear function
    double dst[3] = {src[0] + 0.1 * src[1] *src[0] * src[0],
                     src[1] + 0.05 * src[0] * src[1],
                     0};
    
    srcPoints->InsertNextPoint(src);
    dstPoints->InsertNextPoint(dst);
  }

  // Set up the transform
  vtkNew<vtkThinPlateSplineTransform> trans;
  //trans->SetBasisToR(); // 3D
  trans->SetBasisToR2LogR(); // 2D
  trans->SetRegularizeBulkTransform(true); // this needs some care
  trans->SetSourceLandmarks(srcPoints);
  trans->SetTargetLandmarks(dstPoints);
  trans->Update();

  // Test the forward transform on the points used to create it
  vtkNew<vtkPoints> transSrcPoints;
  trans->TransformPoints(srcPoints, transSrcPoints);
  std::cout << "Error using the forward transform: "
            << max_error(dstPoints, transSrcPoints) << "\n";
  
  // Test the forward transform on a new set of points for which we
  
  // Compute the inverse transform. This changes the direction
  // in which TransformPoints() is applied.
  trans->Inverse();

  // Test how reliably the inverse brings one back
  vtkNew<vtkPoints> srcPoints2;
  trans->TransformPoints(transSrcPoints, srcPoints2);
  std::cout << "Error after forward then inverse transform: "
            << max_error(srcPoints, srcPoints2) << "\n";

  return;
}

int main() {
  TestThinPlateSplineTransform();

  return 0;
}

