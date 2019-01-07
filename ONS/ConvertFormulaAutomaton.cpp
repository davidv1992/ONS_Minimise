#include <iostream>
#include <sstream>
#include <vector>
#include <map>
#include <cassert>
#include <cctype>

#include <ONS/rational.h>
#include <ONS/nomset.h>
#include <ONS/orbit.h>
#include <ONS/eqimap.h>

using namespace std;
using namespace ONS;

class formula {
public:
	virtual bool eval(map<string, rational> vals)=0;
	virtual void print(ostream &o)=0;
	virtual ~formula(){}
};

class eqform : public formula {
public:
	string v1, v2;
	virtual bool eval(map<string, rational> vals) {
		assert(vals.count(v1));
		assert(vals.count(v2));
		return vals[v1] == vals[v2];
	}
	virtual void print(ostream &o) {
		o << "(" << v1 << " = " << v2 << ")";
	}
};

class ltform : public formula {
public:
	string v1, v2;
	virtual bool eval(map<string, rational> vals) {
		assert(vals.count(v1));
		assert(vals.count(v2));
		return vals[v1] < vals[v2];
	}
	virtual void print(ostream &o) {
		o << "(" << v1 << " < " << v2 << ")";
	}
};

class notform : public formula {
public:
	formula *inner;
	virtual bool eval(map<string, rational> vals) {
		return !inner->eval(vals);
	}
	virtual void print(ostream &o) {
		o << "(not ";
		inner->print(o);
		o << ")";
	}
	virtual ~notform() {
		delete inner;
	}
};

class orform : public formula {
public:
	formula *left, *right;
	virtual bool eval(map<string, rational> vals) {
		return left->eval(vals) || right->eval(vals);
	}
	virtual void print(ostream &o) {
		o << "(";
		left->print(o);
		o << " or ";
		right->print(o);
		o << ")";
	}
	virtual ~orform() {
		delete left;
		delete right;
	}
};

class andform : public formula {
public:
	formula *left, *right;
	virtual bool eval(map<string, rational> vals) {
		return left->eval(vals) && right->eval(vals);
	}
	virtual void print(ostream &o) {
		o << "(";
		left->print(o);
		o << " and ";
		right->print(o);
		o << ")";
	}
	virtual ~andform() {
		delete left;
		delete right;
	}
};

void skipSpace(istream &in) {
	while (isspace(in.peek())) in.get();
}

void match(istream &in, char s) {
	assert(in.get() == s);
}

formula *parseFormula(istream &in) {
	formula *result;
	
	skipSpace(in);
	match(in, '(');
	skipSpace(in);
	
	if (in.peek() == '(') {
		// either and or or, parse that
		formula *left = parseFormula(in);
		skipSpace(in);
		
		// read operator
		string op = "";
		while (isalpha(in.peek())) {
			op += in.get();
		}
		
		// parse right hand side
		formula *right = parseFormula(in);
		
		// create result
		if (op == "and") {
			andform *preres = new andform();
			preres->left = left;
			preres->right = right;
			result = preres;
		} else if (op == "or") {
			orform *preres = new orform();
			preres->left = left;
			preres->right = right;
			result = preres;
		} else {
			assert(false);
		}
	} else if (isalpha(in.peek())) {
		// variable name or not, get name first
		string name = "";
		while (isalnum(in.peek())) {
			name += in.get();
		}
		
		// check for not
		if (name == "not") {
			notform *preres = new notform();
			preres->inner = parseFormula(in);
			result = preres;
		} else {
			// read operator
			skipSpace(in);
			char op = in.get();
			
			// and second name
			skipSpace(in);
			string name2 = "";
			while (isalnum(in.peek())) {
				name2 += in.get();
			}
			
			if (op == '=') {
				eqform *preres = new eqform();
				preres->v1 = name;
				preres->v2 = name2;
				result = preres;
			} else if (op == '<') {
				ltform *preres = new ltform();
				preres->v1 = name;
				preres->v2 = name2;
				result = preres;
			} else if (op == '>') {
				ltform *preres = new ltform();
				preres->v1 = name2;
				preres->v2 = name;
				result = preres;
			} else {
				assert(false);
			}
		}
	} else {
		assert(false);
	}
	
	match(in, ')');
	return result;
}

template<typename Q, typename A>
class automaton {
public:
	nomset<A> alphabet;
	nomset<Q> states;
	eqimap<pair<Q,A>,Q> delta;
	nomset<Q> finalStates;
	Q initialState;
	
	bool accepts(std::vector<A> word) {
		Q currentState = initialState;
		for (auto a : word) {
			currentState = delta({currentState, a});
		}
		return finalStates.contains(currentState);
	}
};

struct transition {
	formula *cond;
	int outidx;
	vector<string> vars;
};

vector<nomset<vector<rational>>> Qn;

nomset<vector<rational>> constructQn(int n) {
	if (Qn.size() == 0) {
		Qn.push_back(nomset<vector<rational>>(vector<rational>(0)));
	}
	
	while (n >= Qn.size()) {
		nomset<pair<rational, vector<rational>>> pre = nomset_product(nomset_rationals(), Qn.back());
		Qn.push_back(nomset_map<pair<rational, vector<rational>>, vector<rational>>(pre, [](pair<rational, vector<rational>> in) {
			in.second.push_back(in.first);
			return in.second;
		}));
	}
	
	return Qn[n];
}

automaton<pair<int, vector<rational>>, pair<int, vector<rational>>> readFormulaAutomaton(istream &in) {
	automaton<pair<int, vector<rational>>, pair<int, vector<rational>>> result;

	string ignore;
	int nAlphabet, nStates;
	map<int,int> alphSizes, stateSizes;
	
	// Read header
	in >> ignore >> nAlphabet >> nStates;
	
	// Read and construct alphabet
	in >> ignore;
	for (int i=0; i<nAlphabet; i++) {
		int index, size;
		in >> index >> size;
		result.alphabet = nomset_union(
			result.alphabet,
			nomset_map<vector<rational>, pair<int, vector<rational>>>(constructQn(size), [index](vector<rational> in) {
				return make_pair(index, in);
			}));
		alphSizes[index] = size;
	}
	
	// Read and construct state space
	in >> ignore;
	for (int i=0; i<nStates; i++) {
		int index, size;
		bool accepts;
		in >> index >> size >> accepts;
		auto curState = nomset_map<vector<rational>, pair<int, vector<rational>>>(
			constructQn(size), 
			[index](vector<rational> in) {
				return make_pair(index, in);
			});
		result.states = nomset_union(result.states, curState);
		if (accepts)
			result.finalStates = nomset_union(result.finalStates, curState);
		stateSizes[index] = size;
	}
	
	// Read transitions
	map<pair<int, int>, vector<transition>> transMap;
	in >> ignore;
	while (true) {
		int stateIndex, alphIndex;
		in >> stateIndex >> alphIndex;
		if (!in)
			break;
		
		transition trans;
		trans.cond = parseFormula(in);
		
		in >> ignore;
		in >> trans.outidx;
		for (int i=0; i<stateSizes[trans.outidx]; i++) {
			string name;
			in >> name;
			trans.vars.push_back(name);
		}
		
		transMap[make_pair(stateIndex, alphIndex)].push_back(trans);
	}
	
	// Construct delta
	auto deltaRange = nomset_product(result.states, result.alphabet);
	result.delta = eqimap<pair<pair<int, vector<rational>>, pair<int, vector<rational>>>, pair<int, vector<rational>>>(
		deltaRange,
		[&transMap](pair<pair<int, vector<rational>>, pair<int, vector<rational>>> in) {
			// Construct variable list
			map<string, rational> vars;
			for (int i=0; i<(int)in.first.second.size(); i++) {
				ostringstream vn;
				vn << "x" << i;
				vars[vn.str()] = in.first.second[i];
			}
			for (int i=0; i<(int)in.second.second.size(); i++) {
				ostringstream vn;
				vn << "y" << i;
				vars[vn.str()] = in.second.second[i];
			}
			
			for (auto &trans : transMap[make_pair(in.first.first, in.second.first)]) {
				// Try transition
				bool isOk = trans.cond->eval(vars);
				if (!isOk)
					continue;
				
				// Calc result
				pair<int, vector<rational>> res;
				res.first = trans.outidx;
				for (auto &vn: trans.vars) {
					res.second.push_back(vars[vn]);
				}
				return res;
			}

			assert(false);
			pair<int, vector<rational>> fail;
			return fail;
		});
	return result;
}

int main() {
	auto aut = readFormulaAutomaton(cin);
	
	int alphCount = 0, stateCount = 0;
	map<orbit<pair<int, vector<rational>>>, int> alphIdx;
	map<orbit<pair<int, vector<rational>>>, int> stateIdx;
	
	cout << "Automaton " << aut.alphabet.orbits.size() << " " << aut.states.orbits.size() << endl;
	cout << "Alphabet" << endl;
	for (auto o : aut.alphabet) {
		alphIdx[o] = alphCount;
		cout << alphCount << " " << o.supportSize() << endl;
		alphCount++;
	}
	cout << "States" << endl;
	for (auto o : aut.states) {
		stateIdx[o] = stateCount;
		cout << stateCount << " " << o.supportSize() << " " << aut.finalStates.contains(o) << endl;
		stateCount++;
	}
	cout << "Delta" << endl;
	for (auto o : aut.delta) {
		auto el = o.first.getElement();
		string prodString = "";
		orbit<pair<int, vector<rational>>> alphO(el.second);
		assert(alphIdx.count(alphO));
		int alphI = alphIdx[alphO];
		orbit<pair<int, vector<rational>>> stateO(el.first);
		assert(stateIdx.count(stateO));
		int stateI = stateIdx[stateO];
		
		vector<rational> Aseq = stateO.getSeqFromElement(el.first);
		vector<rational> Bseq = alphO.getSeqFromElement(el.second);
		auto Ait = Aseq.begin();
		auto Bit = Bseq.begin();
		while (Ait != Aseq.end() && Bit != Bseq.end()) {
			if (*Ait == *Bit) {
				prodString += 'C';
				Ait++;
				Bit++;
			} else if (*Ait < *Bit) {
				prodString += 'A';
				Ait++;
			} else {
				prodString += 'B';
				Bit++;
			}
		}
		while (Ait != Aseq.end()) {
			prodString += 'A';
			Ait++;
		}
		while (Bit != Bseq.end()) {
			prodString += 'B';
			Bit++;
		}
		assert(stateIdx.count(o.second.first));
		cout << stateI << " " << alphI << " " << prodString << " -> " << stateIdx[o.second.first] << " ";
		for (auto b : o.second.second) cout << b;
		cout << endl;
	}
	return 0;
}
