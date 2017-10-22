#include <vector>
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

automaton<variant<pair<string, singleton>, pair<string, vector<rational>>>, rational> runningAutomaton (int len) {
	automaton<variant<pair<string, singleton>, pair<string, vector<rational>>>, rational> res;
	res.alphabet = nomset_rationals();

	res.states.insert(orbit<variant<pair<string, singleton>, pair<string, vector<rational>>>>(make_pair(string("accept"), singleton())));
	res.states.insert(orbit<variant<pair<string, singleton>, pair<string, vector<rational>>>>(make_pair(string("reject"), singleton())));
	res.finalStates.insert(orbit<variant<pair<string, singleton>, pair<string, vector<rational>>>>(make_pair(string("accept"), singleton())));
	
	nomset<vector<rational>> vec;
	vec.insert(orbit<vector<rational>>(vector<rational>(0)));
	for (auto o : vec) {
		vector<rational> el = o.getElement();
		res.states.insert(orbit<variant<pair<string, singleton>, pair<string, vector<rational>>>>(make_pair(string("store"), el)));
		res.initialState = make_pair(string("store"), el);
	}
	for (int i=1; i<=len; i++) {
		nomset<pair<vector<rational>,rational>> prod = nomset_product(vec, nomset_rationals());
		vec = nomset_map<pair<vector<rational>,rational>, vector<rational>>(prod, [](pair<vector<rational>, rational> el) {el.first.push_back(el.second); return el.first;});
		for (auto o : vec) {
			vector<rational> el = o.getElement();
			res.states.insert(orbit<variant<pair<string, singleton>, pair<string, vector<rational>>>>(make_pair(string("check"), el)));
			if (i != len)
				res.states.insert(orbit<variant<pair<string, singleton>, pair<string, vector<rational>>>>(make_pair(string("store"), el)));
		}
	}
	
	auto deltaDom = nomset_product(res.states, res.alphabet);
	res.delta = eqimap<pair<variant<pair<string, singleton>, pair<string, vector<rational>>>, rational>, variant<pair<string, singleton>, pair<string, vector<rational>>>>(deltaDom,
		[&](pair<variant<pair<string, singleton>, pair<string, vector<rational>>>, rational> in) {
			if (in.first.index() == 0) {
				return variant<pair<string, singleton>, pair<string, vector<rational>>>(make_pair(string("reject"), singleton()));
			}
			
			auto el = in.first.get<1>();
			if (el.first == "store") {
				el.second.push_back(in.second);
				if (el.second.size() == len)
					return variant<pair<string, singleton>, pair<string, vector<rational>>>(make_pair(string("check"), el.second));
				return variant<pair<string, singleton>, pair<string, vector<rational>>>(el);
			} else {
				if (el.second.back() != in.second)
					return variant<pair<string, singleton>, pair<string, vector<rational>>>(make_pair(string("reject"), singleton()));
				
				el.second.pop_back();
				if (el.second.size() == 0)
					return variant<pair<string, singleton>, pair<string, vector<rational>>>(make_pair(string("accept"), singleton()));
				return variant<pair<string, singleton>, pair<string, vector<rational>>>(el);
			}
		});
	
	return res;
}

int main(int argc, char **argv) {
	assert(argc == 2);
	auto aut = runningAutomaton(atoi(argv[1]));
	
	int alphCount = 0, stateCount = 0;
	map<orbit<rational>, int> alphIdx;
	map<orbit<variant<pair<string, singleton>, pair<string, vector<rational>>>>, int> stateIdx;
	
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
		int alphI = alphIdx[alphO];
		orbit<variant<pair<string, singleton>, pair<string, vector<rational>>>> stateO(el.first);
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
		
		cout << stateI << " " << alphI << " " << prodString << " -> " << stateIdx[o.second.first] << " ";
		for (auto b : o.second.second) cout << b;
		cout << endl;
	}
	return 0;
}
