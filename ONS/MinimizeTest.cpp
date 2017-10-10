#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <random>
#include <utility>

#include <OrbitSets/orbit.h>
#include <OrbitSets/eqimap.h>
#include <OrbitSets/nomset.h>
#include <OrbitSets/rational.h>

using namespace std;
using namespace OrbitSets;

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

template<typename Q, typename A>
nomset<pair<Q,Q>> automaton_equiv(automaton<Q,A> aut) {
	nomset<Q> StatesSubFinal = nomset_filter(aut.states, [&](Q el) { return !aut.finalStates.contains(el); });
	
	nomset<pair<Q,Q>> result = nomset_union(nomset_product(aut.finalStates, aut.finalStates), nomset_product(StatesSubFinal, StatesSubFinal));
	
	while (true) {
		nomset<pair<Q,Q>> next;
		
		for (auto o : result.orbits) {
			nomset<pair<Q,Q>> curOrbit;
			curOrbit.orbits.insert(o);
			
			nomset<pair<pair<Q,Q>, A>> testInput = nomset_product(curOrbit, aut.alphabet);
			nomset<pair<Q,Q>> testOut = nomset_map<pair<pair<Q,Q>,A>, pair<Q,Q>>(testInput, [&](pair<pair<Q,Q>,A> el) {
				auto q1 = aut.delta(pair<Q,A>(el.first.first, el.second));
				auto q2 = aut.delta(pair<Q,A>(el.first.second, el.second));
				auto res = pair<Q,Q>(q1, q2);
				return res;
			});
			bool testResult = result.contains(testOut);
			if (testResult) {
				next.orbits.insert(next.orbits.end(), o);
			}
		}

		if (next == result)
			break;
		result = next;
	}
	
	return result;
}

template<typename Q, typename A>
automaton<pair<string, abstract>,A> automaton_minimize_a(automaton<Q,A> aut) {
	auto equiv = automaton_equiv(aut);
	automaton<pair<string, abstract>, A> res;
	res.alphabet = aut.alphabet;
	
	eqimap<Q, pair<string, abstract>> partition;
	int curLabel = 1;
	for (auto o : aut.states.orbits) {
		nomset<Q> curOrbit;
		curOrbit.orbits.insert(o);
		bool handled = false;
		for (auto s : aut.states.orbits) {
			if (!(s < o)) break;
			nomset<Q> refOrbit;
			refOrbit.orbits.insert(s);
			nomset<pair<Q,Q>> refProduct = nomset_product(refOrbit, curOrbit);
			for (auto p : refProduct.orbits) {
				if (equiv.contains(p)) {
					pair<Q,Q> el = p.getElement();
					std::vector<rational> seqRef, seqCur;
					seqRef = s.getSeqFromElement(el.first);
					seqCur = o.getSeqFromElement(el.second);
					std::vector<bool> refMask = partition.mapData[s].second;
					std::vector<bool> curMask(o.supportSize(), false);
					
					size_t i_A =0, i_B = 0;
					while (i_A < seqRef.size() && i_B < seqCur.size()) {
						if (seqRef[i_A] == seqCur[i_B]) {
							if (refMask[i_A])
								curMask[i_B] = true;
							i_A++;
							i_B++;
						} else if (seqRef[i_A] < seqCur[i_B]) {
							i_A++;
						} else {
							i_B++;
						}
					}
					partition.mapData[o] = {partition.mapData[s].first, curMask};
					
					handled = true;
					break;
				}
			}
			if (handled) break;
		}
		
		if (handled) continue;
		
		nomset<pair<Q,Q>> curProduct = nomset_product(curOrbit, curOrbit);
		std::vector<bool> mask(o.supportSize(), true);
		for (auto p : curProduct.orbits) {
			if (equiv.contains(p)) {
				pair<Q,Q> el = p.getElement();
				std::vector<rational> seqA, seqB;
				seqA = o.getSeqFromElement(el.first);
				seqB = o.getSeqFromElement(el.second);
				
				for (size_t i=0; i<seqA.size(); i++) {
					if (seqA[i] != seqB[i])
						mask[i] = false;
				}
			}
		}
		
		int tgtSupSize = 0;
		for (auto b : mask) {
			if (b) tgtSupSize++;
		}
		
		ostringstream labelStream;
		labelStream << "O" << curLabel;
		curLabel++;
		orbit<pair<string, abstract>> newOrbit = orbit<pair<string, abstract>>(labelStream.str(), orbit<abstract>(tgtSupSize));
		res.states.orbits.insert(newOrbit);
		partition.mapData[o] = {newOrbit, mask};
	}
	
	res.initialState = partition(aut.initialState);
	res.finalStates = nomset_map<Q, pair<string,abstract>,eqimap<Q,pair<string,abstract>>>(aut.finalStates, partition);
	
	auto deltaSpace = nomset_product(aut.states, aut.alphabet);
	for (auto o : deltaSpace.orbits) {
		auto el = o.getElement();
		pair<pair<string,abstract>,A> nel;
		nel.second = el.second;
		nel.first = partition(el.first);
		orbit<pair<pair<string,abstract>,A>> to(nel);
		if (res.delta.mapData.count(to) != 0) continue;
		
		pair<string,abstract> out = partition(aut.delta(el));
		res.delta.add(nel, out);
	}
	
	return res;
}

template<typename Q, typename A> automaton<pair<int,abstract>,A> automaton_minimize_b(automaton<Q,A> aut) {
	nomset<pair<int,abstract>> part_domain;
	eqimap<Q,pair<int,abstract>> partition;
	
	part_domain.orbits.insert(orbit<pair<int,abstract>>(1, orbit<abstract>(0)));
	part_domain.orbits.insert(orbit<pair<int,abstract>>(2, orbit<abstract>(0)));
	partition = eqimap<Q,pair<int,abstract>>(aut.states, [&](Q el) {
		if (aut.finalStates.contains(el)) {
			return pair<int, abstract>(1, abstract());
		} else {
			return pair<int, abstract>(2, abstract());
		}
	});
	
	bool cont = true;
	while (cont) {
		nomset<pair<int,abstract>> next_domain;
		eqimap<Q,pair<int,abstract>> next_partition;
		int next_dom_count = 1;
		
		cont = false;
		for (auto o : aut.states.orbits) {
			bool done = false;
			nomset<Q> curOrbit;
			curOrbit.orbits.insert(o);
			
			for (auto s : aut.states.orbits) {
				if (!(s < o)) break;
				nomset<Q> refOrbit;
				refOrbit.orbits.insert(s);
				nomset<pair<Q,Q>> refProduct = nomset_product(curOrbit, refOrbit);
				for (auto p : refProduct.orbits) {
					pair<Q,Q> el = p.getElement();
					pair<int, abstract> e1 = partition(el.first);
					pair<int, abstract> e2 = partition(el.second);
					if (e1 != e2) continue;
					
					bool isOk = true;
					nomset<pair<Q,Q>> pOrbit;
					pOrbit.orbits.insert(p);
					nomset<pair<pair<Q,Q>,A>> alphProduct = nomset_product(pOrbit, aut.alphabet);
					for (auto ao : alphProduct.orbits) {
						pair<pair<Q,Q>,A> ael = ao.getElement();
						pair<int, abstract> e1 = partition(aut.delta(pair<Q,A>(ael.first.first, ael.second)));
						pair<int, abstract> e2 = partition(aut.delta(pair<Q,A>(ael.first.second, ael.second)));
						if (e1 != e2) {
							isOk = false;
							cont = true;
							break;
						}
					}
					
					if (!isOk) continue;
					auto md = next_partition.mapData.find(s);
					std::vector<bool> &refMask = md->second.second;
					std::vector<bool> curMask(o.supportSize(), false);
					std::vector<rational> seqRef, seqCur;
					seqCur = o.getSeqFromElement(el.first);
					seqRef = s.getSeqFromElement(el.second);
					size_t i_A = 0, i_B = 0;
					while (i_A < seqCur.size() && i_B < seqRef.size()) {
						if (seqCur[i_A] == seqRef[i_B]) {
							curMask[i_A] = refMask[i_B];
							i_A++;
							i_B++;
						} else if (seqCur[i_A] < seqRef[i_B]) {
							i_A++;
						} else {
							i_B++;
						}
					}
					next_partition.mapData[o] = {md->second.first, curMask};
					done = true;
				}
				
				if (done) break;
			}
			
			if (done) continue;
			
			nomset<pair<Q,Q>> curProduct = nomset_product(curOrbit, curOrbit);
			std::vector<bool> mask(o.supportSize(), true);
			for (auto p : curProduct.orbits) {
				auto el = p.getElement();
				if (partition(el.first) != partition(el.second)) continue;
				
				bool isOk = true;
				nomset<pair<Q,Q>> pOrbit;
				pOrbit.orbits.insert(p);
				nomset<pair<pair<Q,Q>,A>> alphProduct = nomset_product(pOrbit, aut.alphabet);
				for (auto ao : alphProduct.orbits) {
					auto ael = ao.getElement();
					if (partition(aut.delta({ael.first.first, ael.second})) != partition(aut.delta({ael.first.second,ael.second}))) {
						isOk = false;
						cont = true;
						break;
					}
				}
				if (!isOk) continue;
				
				auto Aseq = o.getSeqFromElement(el.first);
				auto Bseq = o.getSeqFromElement(el.second);
				
				for (size_t i = 0; i<Aseq.size(); i++) {
					if (Aseq[i] != Bseq[i])
						mask[i] = false;
				}
			}
			
			unsigned tgtSuppSize = 0;
			for (auto b : mask) {
				if (b) tgtSuppSize++;
			}
			
			orbit<pair<int, abstract>> newOrbit(next_dom_count, orbit<abstract>(tgtSuppSize));
			next_dom_count++;
			next_domain.orbits.insert(newOrbit);
			next_partition.mapData[o] = {newOrbit, mask};
		}
		
		part_domain = next_domain;
		partition = next_partition;
	}
	
	automaton<pair<int,abstract>,A> res;
	res.states = part_domain;
	res.initialState = partition(aut.initialState);
	res.finalStates = nomset_map<Q, pair<int,abstract>,eqimap<Q,pair<int,abstract>>>(aut.finalStates, partition);
	
	auto deltaSpace = nomset_product(aut.states, aut.alphabet);
	for (auto o : deltaSpace.orbits) {
		auto el = o.getElement();
		pair<pair<int,abstract>,A> nel;
		nel.second = el.second;
		nel.first = partition(el.first);
		orbit<pair<pair<int,abstract>,A>> to(nel);
		if (res.delta.mapData.count(to) != 0) continue;
		
		pair<int,abstract> out = partition(aut.delta(el));
		res.delta.add(nel, out);
	}
	
	return res;
}

int main(int argc, char **argv) {
	for (int i=1; i<argc; i++) {
		ifstream in(argv[i]);
		string ignore;
		
		automaton<pair<int, abstract>, pair<int, abstract>> aut;
		
		int nAlphabet, nStates;
		map<int, orbit<pair<int, abstract>>> alphabetMap, stateMap;
		
		// Read header
		in >> ignore >> nAlphabet >> nStates;
		
		// Read alphabet
		in >> ignore;
		for (int i=0; i<nAlphabet; i++) {
			int idx, supportSize;
			in >> idx >> supportSize;
			orbit<pair<int,abstract>> curOrbit(idx, orbit<abstract>(supportSize));
			aut.alphabet.orbits.insert(curOrbit);
			alphabetMap[idx] = curOrbit;
		}
		
		// Read states
		in >> ignore;
		for (int i=0; i<nStates; i++) {
			int idx, supportSize;
			bool isFinal;
			in >> idx >> supportSize >> isFinal;
			orbit<pair<int,abstract>> curOrbit(idx, orbit<abstract>(supportSize));
			aut.states.orbits.insert(curOrbit);
			stateMap[idx] = curOrbit;
			if (isFinal)
				aut.finalStates.orbits.insert(curOrbit);
			
			// Deal with the fact that the framework needs an initial state.
			if (i == 0) {
				aut.initialState = curOrbit.getElement();
			}
		}
		
		// Read delta
		in >> ignore;
		auto deltaDom = nomset_product(aut.states, aut.alphabet);
		int domSize = deltaDom.orbits.size();
		for (int i=0; i<domSize; i++) {
			int state, alph;
			string ProdMap;
			int targ;
			vector<bool> mask;
			in >> state >> alph >> ProdMap >> ignore >> targ;
			mask.resize(ProdMap.size(), false);
			for (size_t i=0; i<ProdMap.size(); i++) {
				char c;
				in >> c;
				if (c == '1')
					mask[i] = true;
			}
			
			pair<pair<int, abstract>, pair<int, abstract>> el;
			el.first.first = state;
			el.second.first = alph;
			for (size_t i=0; i<ProdMap.size(); i++) {
				if (ProdMap[i] != 'B')
					el.first.second.data.insert(rational(i));
				if (ProdMap[i] != 'A')
					el.second.second.data.insert(rational(i));
			}
			
			orbit<pair<pair<int,abstract>,pair<int,abstract>>> prodOrbit(el);
			aut.delta.mapData[prodOrbit].first = stateMap[targ];
			aut.delta.mapData[prodOrbit].second = mask;
		}
		
		auto min = automaton_minimize_b(aut);
		cout << aut.states.orbits.size() << " " << min.states.orbits.size() << endl;
	}
}
