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

automaton<variant<singleton, rational, pair<rational,rational>>, rational> LmaxAutomaton() {
	automaton<variant<singleton, rational, pair<rational,rational>>, rational> res;
	
	res.alphabet = nomset_rationals();
	
	res.states = nomset_singleton();
	res.states = nomset_union<variant<singleton, rational, pair<rational,rational>>>(res.states, nomset_rationals());
	res.states = nomset_union<variant<singleton, rational, pair<rational,rational>>>(res.states, nomset_product(nomset_rationals(), nomset_rationals()));
	
	res.finalStates = nomset_filter(res.states, [](variant<singleton,rational,pair<rational,rational>> el) {
		if (el.index() != 2)
			return false;
		
		pair<rational,rational> r = el.get<2>();
		return r.first == r.second;
	});
	
	auto deltaDom = nomset_product(res.states, res.alphabet);
	res.delta = eqimap<pair<variant<singleton, rational, pair<rational,rational>>, rational>, variant<singleton, rational, pair<rational,rational>>>(deltaDom,
		[](pair<variant<singleton, rational, pair<rational,rational>>, rational> in) {
			if (in.first.index() == 0) {
				return variant<singleton,rational,pair<rational,rational>>(in.second);
			} else if (in.first.index() == 1) {
				return variant<singleton,rational,pair<rational,rational>>(make_pair(in.first.get<1>(), in.second));
			} else {
				return variant<singleton,rational,pair<rational,rational>>(make_pair(max(in.first.get<2>().first, in.first.get<2>().second), in.second));
			}
		});
	
	return res;
}

int main() {
	auto aut = LmaxAutomaton();
	
	int alphCount = 0, stateCount = 0;
	map<orbit<rational>, int> alphIdx;
	map<orbit<variant<singleton, rational, pair<rational,rational>>>, int> stateIdx;
	
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
		orbit<variant<singleton, rational, pair<rational,rational>>> stateO(el.first);
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
