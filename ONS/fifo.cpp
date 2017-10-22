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

automaton<variant<singleton, pair<vector<rational>,vector<rational>>>, pair<string, rational>> fifoAutomaton(int depth) {
	automaton<variant<singleton, pair<vector<rational>,vector<rational>>>, pair<string, rational>> res;
	res.alphabet = nomset_union(
		nomset_map<rational, pair<string, rational>>(nomset_rationals(), [](rational a) {return make_pair("push", a);}),
		nomset_map<rational, pair<string, rational>>(nomset_rationals(), [](rational a) {return make_pair("pop", a);}));
	
	res.states = nomset_singleton();
	
	nomset<vector<rational>> vec;
	vec.insert(orbit<vector<rational>>(vector<rational>(0)));
	nomset<pair<vector<rational>,vector<rational>>> pvec;
	pvec.insert(orbit<pair<vector<rational>,vector<rational>>>(make_pair(vector<rational>(0),vector<rational>(0))));
	
	res.states = nomset_union(res.states, nomset<variant<singleton, pair<vector<rational>,vector<rational>>>>(pvec));
	res.finalStates = pvec;
	res.initialState = make_pair(vector<rational>(0), vector<rational>(0));
	for (int i=1; i<=depth; i++) {
		nomset<pair<vector<rational>,rational>> prod = nomset_product(vec, nomset_rationals());
		vec = nomset_map<pair<vector<rational>,rational>, vector<rational>>(prod, [](pair<vector<rational>, rational> el) {el.first.push_back(el.second); return el.first;});
		for (auto o : vec) {
			vector<rational> el = o.getElement();
			vector<rational> first;
			vector<rational> second = el;
			for (int j=0; j<=i; j++) {
				res.states.insert(orbit<variant<singleton, pair<vector<rational>,vector<rational>>>>(make_pair(first, second)));
				res.finalStates.insert(orbit<variant<singleton, pair<vector<rational>,vector<rational>>>>(make_pair(first, second)));
				if (j != i) {
					first.push_back(second.back());
					second.pop_back();
				}
			}
		}
	}
	
	auto deltaDom = nomset_product(res.states, res.alphabet);
	res.delta = eqimap<pair<variant<singleton, pair<vector<rational>,vector<rational>>>, pair<string, rational>>,variant<singleton, pair<vector<rational>,vector<rational>>>>(deltaDom,
		[&](pair<variant<singleton, pair<vector<rational>,vector<rational>>>, pair<string, rational>> el) {
			if (el.first.index() == 0) {
				// Error -> Error
				return variant<singleton, pair<vector<rational>,vector<rational>>>(singleton());
			}
			
			pair<vector<rational>,vector<rational>> fifo = el.first.get<1>();
			
			if (el.second.first == "push") {
				if (fifo.first.size() + fifo.second.size() == depth) {
					return variant<singleton, pair<vector<rational>,vector<rational>>>(singleton());
				}
				
				fifo.first.push_back(el.second.second);
				return variant<singleton, pair<vector<rational>,vector<rational>>>(fifo);
			} else {
				if (fifo.first.size() == 0 && fifo.second.size() == 0) {
					return variant<singleton, pair<vector<rational>,vector<rational>>>(singleton());
				}
				
				if (fifo.second.size() == 0) {
					while (fifo.first.size() != 0) {
						fifo.second.push_back(fifo.first.back());
						fifo.first.pop_back();
					}
				}
				
				if (el.second.second != fifo.second.back()) {
					return variant<singleton, pair<vector<rational>,vector<rational>>>(singleton());
				}
				
				fifo.second.pop_back();
				return variant<singleton, pair<vector<rational>,vector<rational>>>(fifo);
			}
		});
	
	return res;
}

int main(int argc, char **argv) {
	assert(argc == 2);
	auto aut = fifoAutomaton(atoi(argv[1]));
	
	int alphCount = 0, stateCount = 0;
	map<orbit<pair<string, rational>>, int> alphIdx;
	map<orbit<variant<singleton, pair<vector<rational>,vector<rational>>>>, int> stateIdx;
	
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
		orbit<pair<string, rational>> alphO(el.second);
		assert(alphIdx.count(alphO));
		int alphI = alphIdx[alphO];
		orbit<variant<singleton, pair<vector<rational>,vector<rational>>>> stateO(el.first);
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
