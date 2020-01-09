// This writes an uncompress binary PCD
#include <iostream>
#include <fstream>
#include <vw/Core.h>
#include <vw/Image.h>
#include <vw/FileIO.h>
#include <vw/Cartography.h>
#include <asp/Core/Common.h>
#include <asp/Core/Macros.h>

using namespace std;
using namespace vw;

struct Options : public asp::BaseOptions {};

// Allows FileIO to correctly read/write these pixel types
namespace vw {
  typedef Vector<float64,6> Vector6;
  template<> struct PixelFormatID<Vector3>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector3f>  { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector4>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_4_CHANNEL; };
  template<> struct PixelFormatID<Vector6>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
}

void find_hist(std::vector<double> const& vals, int n, std::vector<double> & h){
  h.clear(); h.resize(n);
  double mn = *min_element(vals.begin(), vals.end());
  double mx = *max_element(vals.begin(), vals.end());
  if (mx == mn) mx++;
  for (int i = 0; i < (int)vals.size(); i++){
    int v = round((n-1)*(vals[i]-mn)/(mx-mn));
    h[v]++;
  }
}

void find_hist(std::vector<double> const& vals, int n,
               double mx, std::vector<double> & h){
  h.clear(); h.resize(n);
//   double mn = *min_element(vals.begin(), vals.end());
//   double mx = *max_element(vals.begin(), vals.end());
//   if (mx == mn) mx++;
  double mn = 0.0;
  for (int i = 0; i < (int)vals.size(); i++){
    if (vals[i] < mn) continue;
    int v = round((n-1)*(vals[i]-mn)/(mx-mn));
    h[v]++;
  }
}

void find_hist2(std::vector<double> const& vals, std::vector<double> & h){
  int n = 100;
  h.clear(); h.resize(n);
  for (int i = 0; i < n; i++) h[i] = 0.0;
  
  for (int i = 0; i < (int)vals.size(); i++){
    int v = round(vals[i]);
    v = std::min(v, n-1);
    h[v]++;
  }
}

int main( int argc, char**argv ){

  if (argc < 3){
    std::cerr << "Usage: " << argv[0] << " input-PC.tif output.txt"
              << std::endl;
    exit(0);
  }
  
  std::string input = argv[1], output = argv[2];
  if ( input.empty()  || output.empty() ) {
    std::cerr << "Please provide an input and output file." << std::endl;
    return 1;
  }
  std::cout << "Values: " << input << ' ' << output << std::endl;

  ImageViewRef<Vector4> pc = asp::read_cloud<4>(input);
  vector<double> vals;
  
  for (int row = 0; row < pc.rows(); row++){
    for (int col = 0; col < pc.cols(); col++){
      Vector4 P = pc(col, row);
      if (subvector(P, 0, 3) == Vector3()) continue;
      double err = P[3];
      vals.push_back(err);
    }
  }

  double pct = 0.25;
  double factor = 3.0;

  double mx2 = 43.6406;
  std::vector<double> hist(1024, 0);
  for (int i = 0; i < (int)vals.size(); i++){
    double x = vals[i];
    if (x==0)
      continue;
    x = std::min(mx2, x);
    int len = hist.size();
    int k = round((len-1)*std::min(x, mx2)/mx2);
    hist[k]++;
  }

  std::cout << "--writing: hist2.txt" << std::endl;
  std::ofstream fs("hist2.txt");
  for (int s = 0; s < (int)hist.size(); s++){
    fs << hist[s] << std::endl;
  }
  fs.close();

  double Q;
  {
    int pos = 0, sum = 0;
    int total = 0;
    for (int s = 0; s < (int)hist.size(); s++) total += hist[s];
    for (int s = 0; s < (int)hist.size(); s++){
      sum += hist[s];
      if (sum >= (1.0-pct)*total ){
        pos = s;
        break;
      }
    }

    std::cout << "total is " << total << std::endl;
    std::cout << "cut at " << pos << std::endl;
    double Q1 = pos*mx2/hist.size();
    std::cout << "cut 1 val is " << Q1 << std::endl;

    Q = factor*Q1;
    int index = int(round(hist.size()*Q/mx2));
    sum = 0;
    for (int s = 0; s <= index; s++){
      sum += hist[s];
    }
    
    std::cout << "index1 is " << sum << std::endl;
  }
  
  
  std::cout << "cutoff is: " << Q << std::endl;
  
  std::sort(vals.begin(), vals.end());
  int len = vals.size();
//   int cut = int(round((1.0-pct)*len))-1;
//   double Q0 = vals[cut];
//   std::cout << "cut2 val is " << Q0 << std::endl;
  
//   double Q = factor*vals[cut];
//   std::cout << "threshold2: " << Q << std::endl;
  
//   double mx = vals[0];
//   int count = 0;
//   for (int i = 0; i < len; i++){
//     double v = vals[i];
//     if (v < Q){
//       mx = std::max(mx, v);
//       count = i;
//     }
//   }
//   count++;
//   std::cout << "cut, max, keep: " << mx << ' ' << vals[len-1]
//             << ' ' << double(count)/len << std::endl;

//   std::cout << "index2 is " << count << std::endl;
  
//   std::cout << "cut and len " << cut << ' ' << len << ' ' << double(cut)/len
//             << std::endl;
  
//   std::cout << "count: " << count << ' ' << len << ' ' << double(count)/len
//             << std::endl;
//   std::cout << "vals: " << mx << ' ' << Q << ' ' << vals[len-1] << std::endl;
  

//   std::cout << "cuts are " << Q0 << ' ' << Q << std::endl;
//   int count0 = 0, count1 = 0;
//   for (int i = 0; i < len; i++){
//     double v = vals[i];
//     if (v < Q0) count0 = std::max(i, count0);
//     if (v < Q)  count1 = std::max(i, count1);
//   }
//   std::cout << "count0 " << count0 << ' ' << double(count0)/len << std::endl;
//   std::cout << "count1 " << count1 << ' ' << double(count1)/len << std::endl;
  
  int n = 1000;
  vector<double> h;
  find_hist(vals, n, 2*Q, h);
  
  vector<double> vals2, h2;
  for (int i = 0; i < (int)vals.size(); i++){
    if (vals[i] < Q){
      vals2.push_back(vals[i]);
    }else{
      vals2.push_back(-1);
    }
  }
  find_hist(vals2, n, 2*Q, h2);

  //find_hist2(vals, h);

  std::cout << "Will write: " << output << std::endl;
  ofstream outfile( output.c_str() );
  outfile.precision(18);
  for (int i = 0; i < (int)h.size(); i++){
    outfile << h[i] << " ";
  }
  outfile << std::endl;
  
  int sum = 0;
  for (int i = 0; i < (int)h2.size(); i++){
//     sum += h[i];
//     if (sum <= count)
    outfile << h2[i] << " ";
//     else
//       outfile << 0 << " ";
  }
  outfile << std::endl;
  outfile.close();
  
  return 0;
}
