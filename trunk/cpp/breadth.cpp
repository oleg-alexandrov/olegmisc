#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <deque>
#include <cassert>
#include <cstring>
using namespace std;

int main() {

  int N = 11;
  vector< vector<int> > G;
  G.clear();
  G.resize(N);
  G[0].push_back(1); G[0].push_back(4);
  G[1].push_back(0); G[1].push_back(2); G[1].push_back(3); G[1].push_back(7);
  G[2].push_back(1); G[2].push_back(7);
  G[3].push_back(1);
  G[4].push_back(0); G[4].push_back(5);
  G[5].push_back(4); G[5].push_back(6); G[5].push_back(8); G[5].push_back(10);
  G[6].push_back(5);
  G[7].push_back(1); G[7].push_back(2); G[7].push_back(9);

  vector<int> Visited;
  Visited.resize(N);
  for (int s = 0; s < (int)Visited.size(); s++) Visited[s] = 0;

  deque<int> toVisit;
  toVisit.clear();

  toVisit.push_back(0);

  while (1){

    if (toVisit.empty()) break;

    int v = toVisit.front(); toVisit.pop_front();
    
    if (Visited[v]) continue;

    Visited[v] = 1;
    cout << "Visiting " << v << endl;
    
    for (int t = 0; t < (int)G[v].size(); t++){
      int w = G[v][t];
      if (Visited[w]) continue;
      toVisit.push_back(w);
    }
    
  }
  
  return 0;
}
