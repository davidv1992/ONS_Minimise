#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <random>
#include <utility>

using namespace std;

int main(int argc, char **argv) {
	for (int i=1; i<argc; i++) {
		ifstream in(argv[i]);
		string ignore;
		
		int nStates, nAlph;
		
		// Read header
		in >> ignore >> nAlph >> nStates;
		
		// Skip alphabet description
		in >> ignore;
		for (int i=0; i<nAlph; i++) {
			in >> ignore >> ignore;
		}
		
		// Skip state description
		in >> ignore;
		for (int i=0; i<nStates; i++) {
			in >> ignore >> ignore >> ignore;
		}
		
		// Construct graph out of delta.
		vector<vector<bool>> conn(nStates, vector<bool>(nStates, false));
		for (int i=0; i<nStates; i++) conn[i][i] = true;
		in >> ignore;
		while (true) {
			int a, b;
			in >> a >> ignore >> ignore >> ignore >> b >> ignore;
			if (!in) break;
			conn[a][b] = conn[b][a] = true;
		}
		
		// Calculate
		for (int k=0; k<nStates; k++) {
			for (int i=0; i<nStates; i++) {
				for (int j=0; j<nStates; j++) {
					conn[i][j] = conn[i][j] || (conn[i][k] && conn[k][j]);
				}
			}
		}
		
		// Verify
		bool isOk = true;
		for (int i=0; i<nStates; i++) {
			for (int j=0; j<nStates; j++) {
				isOk = isOk && conn[i][j];
			}
		}
		
		if (isOk)
			cout << "Connected" << endl;
		else
			cout << "Disconnected" << endl;
	}
}
