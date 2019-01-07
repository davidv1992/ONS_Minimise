#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <random>

using namespace std;

vector<string> relops = {"<", "=", ">"};
vector<string> logops = {"and", "or"};
string invop = "not";

struct rel {
	string var1;
	string var2;
	string relop;
	
	void print(ostream &o) {
		o << "(" << var1 << " " << relop << " " << var2 << ")";
	}
};

struct formula {
	bool isRel;
	bool inverted;
	rel reldata;
	formula *left, *right;
	string logop;
	
	formula() {
		left = right = NULL;
	}
	
	~formula() {
		if (left) {
			delete left;
		}
		if (right) {
			delete right;
		}
	}
	
	void print(ostream &o) {
		if (inverted) {
			o << "(" << invop << " ";
		}
		if (isRel) {
			reldata.print(o);
		} else {
			o << "(";
			left->print(o);
			o << " " << logop << " ";
			right->print(o);
			o << ")";
		}
		if (inverted) {
			o << ")";
		}
	}
};

string varName(int stateVars, int inputVars, int i) {
	ostringstream res;
	if (i < stateVars) {
		res << "x" << i;
	} else {
		res << "y" << i-stateVars;
	}
	return res.str(); 
}

formula *genFormula(int stateVars, int inputVars, int maxIters) {
	// Generation parameters
	const double relprob = 0.5;
	const double notprob = 0.5;
	
	// Setup randomness
	random_device rd;
	default_random_engine generator(rd());
	bernoulli_distribution reldist(relprob);
	bernoulli_distribution notdist(notprob);
	uniform_int_distribution<int> logopsdist(0, logops.size()-1);
	uniform_int_distribution<int> relopsdist(0, relops.size()-1);
	uniform_int_distribution<int> var(0, stateVars+inputVars-1);
	
	// Setup work queue
	vector<formula*> work;
	formula *result = new formula();
	work.push_back(result);
	
	int iters = 0;
	while (iters < maxIters && !work.empty()) {
		uniform_int_distribution<int> expSel(0, work.size()-1);
		int sel = expSel(generator);
		swap(work[sel], work.back());
		formula *cur = work.back();
		work.pop_back();
		
		if (reldist(generator)) {
			cur->isRel = true;
			cur->inverted = notdist(generator);
			cur->reldata.relop = relops[relopsdist(generator)];
			
			int v1 = var(generator);
			int v2 = var(generator);
			cur->reldata.var1 = varName(stateVars, inputVars, v1);
			cur->reldata.var2 = varName(stateVars, inputVars, v2);
		} else {
			cur->isRel = false;
			cur->inverted = notdist(generator);
			cur->left = new formula();
			cur->right = new formula();
			cur->logop = logops[logopsdist(generator)];
			work.push_back(cur->left);
			work.push_back(cur->right);
		}
		
		iters++;
	}
	
	while (!work.empty()) {
		formula *cur = work.back();
		work.pop_back();
		
		cur->isRel = true;
		cur->inverted = notdist(generator);
		cur->reldata.relop = relops[relopsdist(generator)];
		
		int v1 = var(generator);
		int v2 = var(generator);
		cur->reldata.var1 = varName(stateVars, inputVars, v1);
		cur->reldata.var2 = varName(stateVars, inputVars, v2);
	}
	
	return result;
}

int main(int argc, char *argv[]) {
	if (argc < 3) return 1;
	const int nOrbits = atoi(argv[1]);
	const int maxVars = atoi(argv[2]);
	const int maxClause = atoi(argv[3]);
	
	const double acceptProb = 0.5;
	
	random_device rd;
	default_random_engine generator(rd());
	bernoulli_distribution acceptDist(acceptProb);
	uniform_int_distribution<int> orbitDist(0, maxVars);
	
	cout << "FAutomaton 1 " << nOrbits << endl;
	cout << "Alphabet" << endl;
	cout << "0 1" << endl;
	cout << "States" << endl;
	vector<int> stateSize(nOrbits);
	for (int i=0; i<nOrbits; i++) {
		stateSize[i] = orbitDist(generator);
		cout << i << " " << stateSize[i] << " " << acceptDist(generator) << endl;
	}
	cout << "Delta" << endl;
	for (int i=0; i<nOrbits; i++) {
		vector<int> cands;
		for (int j=0; j<nOrbits; j++) {
			if (stateSize[j] <= stateSize[i]+1) {
				cands.push_back(j);
			}
		}
		uniform_int_distribution<int> orbitSel(0, cands.size()-1);
		uniform_int_distribution<int> varSel(0, stateSize[i]);
		
		formula *sel = genFormula(stateSize[i], 1, maxClause);
		
		int A = cands[orbitSel(generator)];
		cout << i << " 0 ";
		sel->print(cout);
		cout << " -> " << A;
		for (int j=0; j<stateSize[A]; j++) {
			cout << " " << varName(stateSize[i], 1, varSel(generator));
		}
		cout << endl;
		
		sel->inverted = !sel->inverted;
		int B = cands[orbitSel(generator)];
		cout << i << " 0 ";
		sel->print(cout);
		cout << " -> " << B;
		for (int j=0; j<stateSize[B]; j++) {
			cout << " " << varName(stateSize[i], 1, varSel(generator));
		}
		cout << endl;
		
		delete sel;
	}
	
	return 0;
}
