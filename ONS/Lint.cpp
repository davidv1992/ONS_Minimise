#include <iostream>
#include <sstream>
#include <utility>

#include <ONS/orbit.h>
#include <ONS/eqimap.h>
#include <ONS/nomset.h>
#include <ONS/rational.h>

using namespace std;
using namespace ONS;

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

automaton<pair<int, vector<rational>>, rational> LintAutomaton() {
	automaton<pair<int, vector<rational>>, rational> res;
	
	res.alphabet = nomset_rationals();
	
	res.states.insert({0,{}});
	res.states.insert({1,{rational(1)}});
	res.states.insert({2,{rational(1),rational(2)}});
	res.states.insert({3,{rational(1),rational(2)}});
	res.states.insert({4,{}});
	
	res.finalStates = nomset_filter(res.states, [](pair<int, vector<rational>> el) {
		return el.first == 2;
	});
	
	auto deltaDom = nomset_product(res.states, res.alphabet);
	res.delta = eqimap<pair<pair<int, vector<rational>>, rational>, pair<int, vector<rational>>>(deltaDom,
		[](pair<pair<int, vector<rational>>, rational> in) {
			if (in.first.first == 0) {
				return make_pair<int, vector<rational>>(1,{in.second});
			} else if (in.first.first == 1) {
				if (in.first.second[0] >= in.second)
					return make_pair<int,vector<rational>>(4,{});
				else
					return make_pair<int,vector<rational>>(2,{in.first.second[0],in.second});
			} else if (in.first.first == 2) {
				if (in.first.second[0] < in.second && in.first.second[1] > in.second) {
					return make_pair<int, vector<rational>>(3,{in.second,in.first.second[1]});
				} else {
					return make_pair<int,vector<rational>>(4,{});
				}
			} else if (in.first.first == 3) {
				if (in.first.second[0] < in.second && in.first.second[1] > in.second) {
					return make_pair<int,vector<rational>>(2,{in.first.second[0], in.second});
				} else {
					return make_pair<int,vector<rational>>(4,{});
				}
			} else {
				return make_pair<int, vector<rational>>(4,{});
			}
		});
	
	return res;
}

int main() {
	auto aut = LintAutomaton();
	
	int alphCount = 0, stateCount = 0;
	map<orbit<rational>, int> alphIdx;
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
		orbit<rational> alphO(el.second);
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
		//cerr << o.second.first.getElement() << endl;
		assert(stateIdx.count(o.second.first));
		cout << stateI << " " << alphI << " " << prodString << " -> " << stateIdx[o.second.first] << " ";
		for (auto b : o.second.second) cout << b;
		cout << endl;
	}
	return 0;
}
