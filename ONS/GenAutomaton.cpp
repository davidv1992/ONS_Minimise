#include <iostream>
#include <iomanip>
#include <vector>
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

vector<vector<long long>> combTable(1,vector<long long>(1,1));

long long noverk(int n, int k) {
	assert(n >= 0 && k >= 0 && k <= n);
	if (n >= (int)combTable.size()) {
		for (int i=combTable.size(); i<=n; i++) {
			combTable.push_back(vector<long long>(i+1));
			combTable[i][0] = 1;
			combTable[i][i] = 1;
			for (int j=1; j<i; j++) {
				combTable[i][j] = combTable[i-1][j-1] + combTable[i-1][j];
			}
		}
	}
	return combTable[n][k];
}

template<typename T>
vector<bool> randKofN(int k, int n, T generator) {
	vector<bool> res(n, false);
	
	uniform_int_distribution<long long> selDist(0, noverk(n,k)-1);
	long long idx = selDist(generator);
	for (int i=0; i<n; i++) {
		if ((n-i) == k) {
			assert(idx == 0);
			k--;
			res[i] = true;
		} else if (noverk(n-i-1, k) <= idx) {
			idx-=noverk(n-i-1,k);
			k--;
			res[i] = true;
		}
	}
	
	return res;
}

int main() {
	// Parameters
	const int minOrbits = 15, maxOrbits = 15;
	const int maxVar = 3;
	
	// Initialize random distributions
	std::random_device rd;
	default_random_engine generator(rd());
	uniform_int_distribution<int> numOrbits(minOrbits, maxOrbits);
	uniform_int_distribution<int> numVars(0, maxVar);
	uniform_int_distribution<int> tossup(0,1);
	
	// Determine the number of orbits we generate, and provide distribution to randomly select orbits
	int nOrbits = numOrbits(generator);
	uniform_int_distribution<int> rOrbit(0, nOrbits-1);
	
	// Setup the basics for the generation
	automaton <pair<int, abstract>, pair<int, abstract>> aut;
	aut.alphabet.orbits.insert(orbit<pair<int,abstract>>(0,orbit<abstract>(1)));
	
	// Generate the orbits
	vector<orbit<pair<int, abstract>>> orbList;
	for (int i=0; i<nOrbits; i++) {
		int curVars = numVars(generator);
		orbit<pair<int, abstract>> curOrbit(i, orbit<abstract>(curVars));
		aut.states.orbits.insert(curOrbit);
		if (tossup(generator))
			aut.finalStates.orbits.insert(curOrbit);
		if (i == 0)
			aut.initialState = curOrbit.getElement();
		orbList.push_back(curOrbit);
	}
	
	// Generate delta
	nomset<pair<pair<int,abstract>,pair<int,abstract>>> deltaDom = nomset_product(aut.states, aut.alphabet);
	for (auto o : deltaDom.orbits) {
		// select orbit
		int sel = rOrbit(generator);
		while (orbList[sel].supportSize() > o.supportSize())
			sel = rOrbit(generator);
		
		// generate mask
		vector<bool> mask = randKofN(orbList[sel].supportSize(), o.supportSize(), generator);
		
		// and add to map
		aut.delta.mapData[o] = make_pair(orbList[sel], mask);
	}
	
	// Output automaton
	cout << "Automaton " << aut.alphabet.orbits.size() << " " << aut.states.orbits.size() << endl;
	cout << "Alphabet" << endl;
	for (auto o : aut.alphabet.orbits) {
		cout << o.getElement().first << " " << o.supportSize() << endl;
	}
	cout << "States" << endl;
	for (auto o : aut.states.orbits) {
		cout << o.getElement().first << " " << o.supportSize() << " " << aut.finalStates.contains(o) << endl;
	}
	cout << "Delta" << endl;
	for (auto o : aut.delta.mapData) {
		auto el = o.first.getElement();
		string prodString = "";
		auto Ait = el.first.second.data.begin();
		auto Bit = el.second.second.data.begin();
		while (Ait != el.first.second.data.end() && Bit != el.second.second.data.end()) {
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
		while (Ait != el.first.second.data.end()) {
			prodString += 'A';
			Ait++;
		}
		while (Bit != el.second.second.data.end()) {
			prodString += 'B';
			Bit++;
		}
		
		cout << el.first.first << " " << el.second.first << " " << prodString << " -> " << o.second.first.getElement().first << " ";
		for (auto b : o.second.second) cout << b;
		cout << endl;
	}
}
