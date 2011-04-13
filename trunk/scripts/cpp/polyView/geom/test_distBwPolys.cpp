#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <limits>
#include "dPoly.h"
#include "polyUtils.h"
#include "geomUtils.h"

using namespace std;
using namespace utils;

int main(int argc, char** argv){

  if (argc <= 2){
    cout << "Usage: " << argv[0] << " poly1.xg poly2.xg" << endl;
    exit(1);
  }
  
  char * polyFile1 = argv[1];
  char * polyFile2 = argv[2];

  bool isPointCloud = false;
  dPoly poly1; if ( !poly1.readPoly(polyFile1, isPointCloud) ) exit(1);
  dPoly poly2; if ( !poly2.readPoly(polyFile2, isPointCloud) ) exit(1);

  vector<segDist> distVec1, distVec2;

  time_t Start_t, End_t;
  double time_task1, time_task2;

  Start_t = time(NULL);    //record time that task 1 begins
  findDistanceBwPolys(poly1, poly2, // inputs
                      distVec1      // outputs
                      );
  End_t = time(NULL);    //record time that task 1 ends
  time_task1 = difftime(End_t, Start_t);    //compute elapsed time of task 1
  cout << "Time 1 is " << time_task1 << endl;

  Start_t = time(NULL);    //record time that task 1 begins
  findDistanceBwPolysBruteForce(poly1, poly2, // inputs
                                distVec2      // outputs
                                );
  End_t = time(NULL);    //record time that task 1 ends
  time_task2 = difftime(End_t, Start_t);    //compute elapsed time of task 1
  cout << "Time 2 is " << time_task2 << endl;

  cout.precision(20);
  
  for (int t = 0; t < (int)distVec1.size(); t++){

    if (distVec1[t] != distVec2[t]){
      cerr << "Unequal segments" << endl;
      cout << distVec1[t] << endl;
      cout << distVec2[t] << endl;
      cout << endl;
      //exit(1);
    }
    
  }

#if 0
  char out[] = "distances.xg";
  cout << "Writing " << out << endl;
  ofstream of(out);
  of << "color = red" << endl;
  for (int t = 0; t < (int)distVec1.size(); t++){
    const segDist & S = distVec1[t];
    of << S.begx << ' ' << S.begy << endl;
    of << S.endx << ' ' << S.endy << endl;
    of << "NEXT" << endl;
  }
  of.close();
#endif
  
  return 0;
  
}
