#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <limits>
#include <ctime>
#include "boxTree.h"
#include "dPoly.h"

using namespace std;
using namespace utils;

double rand_ab(double a, double b){
  assert(a <= b);
  return a + rand()%max(int(b - a), 1);
}

struct Box{
  double xl, yl, xh, yh;
  Box(): xl(0.0), yl(0.0), xh(0.0), yh(0.0){}
  Box(double xl_in, double yl_in, double xh_in, double yh_in):
    xl(xl_in), yl(yl_in), xh(xh_in), yh(yh_in){}
};

void saveBoxes(std::vector<Box> & Boxes, std::string file, std::string color){
  cout << "Writing " << file << endl;
  ofstream of(file.c_str());
  of << "color = " << color << endl;
  for (int s = 0; s < (int)Boxes.size(); s++){
    const Box & B = Boxes[s]; // alias
    of << B.xl << ' ' << B.yl << endl;
    of << B.xh << ' ' << B.yl << endl;
    of << B.xh << ' ' << B.yh << endl;
    of << B.xl << ' ' << B.yh << endl;
    of << B.xl << ' ' << B.yl << endl;
    of << "NEXT" << endl;
  }
  return;
}


int main(int argc, char** argv){

 srand(time(NULL));

 int numBoxes   = 1000;
 int L          = 100, w = 10; // To determine region size and box width 
 int numRepeats = 1;           // How many times to repeat each experiment
 int numRuns    = 10000;       // How many experiments
 bool doTiming  = false; 

 if (doTiming){
   numRuns = 1;
   numRepeats = 100000;
 }
 
 vector<Box> Boxes;

 for (int q = 0; q < numRuns; q++){
   
    Boxes.clear();

   for (int s = 0; s < numBoxes; s++){
     double xl = rand_ab(-L, L), xh = xl + rand_ab(0, w);
     double yl = rand_ab(-L, L), yh = yl + rand_ab(0, w);
     Boxes.push_back(Box(xl, yl, xh, yh));
   }
 
   boxTree<Box> T;
   // Boxes will be reordered but otherwise unchanged inside of this function.
   // Do not modify this vector afterward.
   T.formTree(Boxes); 

   double L2 = 2*L;
   double xl = rand_ab(-L2, L2), xh = xl + rand_ab(0, L2);
   double yl = rand_ab(-L2, L2), yh = yl + rand_ab(0, L2);

   time_t Start_t, End_t;
   int time_task;
   
   vector<Box> outBoxes;
   if(doTiming) Start_t = time(NULL);    //record time that task 1 begins
   for (int v = 0; v < numRepeats; v++){
     T.getBoxesInBox(xl, yl, xh, yh, outBoxes);
   }
   if(doTiming){
     End_t = time(NULL);  
     time_task = difftime(End_t, Start_t);
     cout << "Tree search time: " << time_task << endl;
   }

   sort(outBoxes.begin(), outBoxes.end(), lexLessThan<Box>);
 
   vector<Box> outBoxes2; // Must be different than Boxes
   outBoxes2.resize(numBoxes);
   
   if(doTiming) Start_t = time(NULL);
   for (int v = 0; v < numRepeats; v++){
     outBoxes2.clear();
     for (int s = 0; s < (int)Boxes.size(); s++){
       const Box & B = Boxes[s];
       if (boxesIntersect(B.xl, B.yl, B.xh, B.yh, xl, yl, xh, yh)){
         outBoxes2.push_back(B);
       }
     }
   }
   if(doTiming){
     End_t = time(NULL);
     time_task = difftime(End_t, Start_t);
     cout << "Brute force search time: " << time_task << endl;
   }

   sort(outBoxes2.begin(), outBoxes2.end(), lexLessThan<Box>);

   bool haveProblem = false;
 
   int len = outBoxes.size();
   if ( len != (int)outBoxes2.size() ){
     haveProblem = true;
   }else{
     for (int s = 0; s < len; s++){
       if ( outBoxes[s] != outBoxes2[s] ){
         haveProblem = true;
       }
     }
   }

   if (haveProblem){
     cerr << "Have a problem!" << endl;

     cout << "In:   " << Boxes.size()     << endl;
     cout << "Out:  " << outBoxes.size()  << endl;
     cout << "Out2: " << outBoxes2.size() << endl;
     
     vector<Box> W;
     W.push_back(Box(xl, yl, xh, yh));
   
     saveBoxes( Boxes,    "all.xg",       "blue"   );
     saveBoxes( W,        "window.xg",    "green"  );
     saveBoxes( outBoxes, "outBoxes.xg",  "white"  );
     saveBoxes( outBoxes, "outBoxes2.xg", "yellow" );
     exit(1);
   }

   if (q%1000 == 0){
     cout << "In:   " << Boxes.size()     << endl;
     cout << "Out:  " << outBoxes.size()  << endl;
     cout << "Out2: " << outBoxes2.size() << endl;
   }
   
 }

 return 0;
}
