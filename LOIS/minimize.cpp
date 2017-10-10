#include <iostream>
#include <fstream>
#include <map>

#include <loisextra.h>
#include <lois-weak.h>
#include <lois-automaton.h>

using namespace std;
using namespace lois;

template<class State, class Symbol> void minimalize1(lsetof<State> Q, lsetof<State> F, lsetof<transition<State,Symbol>> delta, lsetof<Symbol> alph) {
  cout << "Q = " << Q << endl;
  cout << "F = " << F << endl;
  cout << "delta = " << delta << endl;
  
  lsetof<State> NF = Q &~ F;
  cout << "NF = " << NF << endl << endl;

  lsetof<lpair<State,State>> eq = (F * F) | (NF * NF);
  
  int it = 0;
  
  lbool cont = true;
  
  While(cont) {
    cout << "eq = " << eq << endl << endl;
    
    lsetof<lpair<State,State>> neq;
    
    for(State q1: Q) for(State q2: Q) If(memberof(make_lpair(q1,q2), eq)) {
      lbool b = true;
      for(Symbol& x: alph) for(auto& t1: delta) for(auto& t2: delta)
        If(t1.src == q1 && t1.symbol == x && t2.src == q2 && t2.symbol == x) {
          If(!memberof(make_lpair(t1.tgt, t2.tgt), eq)) {
            b = ffalse;
            }
          }
      If(b) neq += make_lpair(q1, q2);
      }
    
    If(eq == neq) cont = ffalse;
    eq = neq; it++;
    }

  cout << "number of iterations: " << it << endl << endl;
  
  lsetof<lsetof<State>> classes;
  
  for(State& a: Q) {
    lsetof<State> t;
    for(State& b: Q) If(memberof(make_lpair(a,b), eq)) 
      t += b;
    classes += t;
    }
  
  cout << "unoptimized classes: " << classes << endl;
  classes = optimize(classes);

  cout << "classes: " << classes << endl;  
  }

int main(int argc, char **argv) {
	initLois();
	sym.useUnicode();
	//solver = solverSMT();
	//solver = solverIncremental("z3 -smt2 -in -t:30000");
	//solver = solverSPASS("SPASS -TimeLimit=240 query.spass > query.out");
	//solver = solverIncremental("./cvc4-2017-* --lang smt --incremental");
	
	Domain dA("A");  
	lsetof<term> A = dA.getSet();
	
	for (int i=1; i<argc; i++) {
		ifstream in (argv[1]);
		string ignore;
		
		// Read header
		int nStates, nAlph;
		in >> ignore >> nAlph >> nStates;
		
		// Read alphabet
		map<int, int> alphMap;
		lsetof<lpair<int, lsetof<term>>> alph;
		in >> ignore;
		for (int i=0; i<nAlph; i++) {
			int idx, supportSize;
			in >> idx >> supportSize;
			
			lsetof<lvector<term>> curBase = newSet(lvector<term>({}));
			for (int i=0; i<supportSize; i++) {
				lsetof<lvector<term>> next;
				for (term a : A) for (lvector<term> b : curBase) {
					lvector<term> nb = b;
					if (i != 0) {
						If (a > b[i-1]) {
							nb.push_back(a);
							next += nb;
						}
					} else {
						nb.push_back(a);
						next += nb;
					}
				}
				curBase = next;
			}
			
			lsetof<lpair<int, lsetof<term>>> curOrb;
			for (lvector<term> a : curBase) {
				lsetof<term> cur;
				for (term b : a) {
					cur += b;
				}
				curOrb += make_lpair(idx, cur);
			}
			alphMap[idx] = supportSize;
			alph = alph | curOrb;
		}
		
		// Read states
		map<int, int> stateMap;
		lsetof<lpair<int, lsetof<term>>> states;
		lsetof<lpair<int, lsetof<term>>> F;
		in >> ignore;
		for (int i=0; i<nStates; i++) {
			int idx, supportSize;
			bool isFinal;
			in >> idx >> supportSize >> isFinal;
			
			lsetof<lvector<term>> curBase = newSet(lvector<term>({}));
			for (int i=0; i<supportSize; i++) {
				lsetof<lvector<term>> next;
				for (term a : A) for (lvector<term> b : curBase) {
					lvector<term> nb = b;
					if (i != 0) {
						If (a > b[i-1]) {
							nb.push_back(a);
							next += nb;
						}
					} else {
						nb.push_back(a);
						next += nb;
					}
				}
				curBase = next;
			}
			
			lsetof<lpair<int, lsetof<term>>> curOrb;
			for (lvector<term> a : curBase) {
				lsetof<term> cur;
				for (term b : a) {
					cur += b;
				}
				curOrb += make_lpair(idx, cur);
			}
			stateMap[idx] = supportSize;
			states = states | curOrb;
			if (isFinal)
				F = F | curOrb;
		}
		
		// Read delta
		lsetof<transition<lpair<int, lsetof<term>>, lpair<int, lsetof<term>>>> delta;
		in >> ignore;
		while (true) {
			int state, alph, targ;
			string ProdMap;
			vector<bool> mask;
			
			in >> state >> alph >> ProdMap >> ignore >> targ;
			if (!in) break;
			mask.resize(ProdMap.size(),false);
			for (size_t i=0; i<ProdMap.size(); i++) {
				char c;
				in >> c;
				if (c == '1')
					mask[i] = true;
			}
			
			lsetof<lvector<term>> curBase = newSet(lvector<term>({}));
			for (size_t i=0; i<ProdMap.size(); i++) {
				lsetof<lvector<term>> next;
				for (term a : A) for (lvector<term> b : curBase) {
					lvector<term> nb = b;
					if (i != 0) {
						If (a > b[i-1]) {
							nb.push_back(a);
							next += nb;
						}
					} else {
						nb.push_back(a);
						next += nb;
					}
				}
				curBase = next;
			}
			
			lsetof<lvector<lsetof<term>>> target;
			
			for (lvector<term> a : curBase) {
				lvector<lsetof<term>> el({lsetof<term>({}), lsetof<term>({}), lsetof<term>({})});
				
				for (size_t i=0; i<ProdMap.size(); i++) {
					if (ProdMap[i] != 'B')
						el[0] += a[i];
					if (ProdMap[i] != 'A')
						el[1] += a[i];
					if (mask[i])
						el[2] += a[i];
				}
				
				target += el;
			}
			
			for (lvector<lsetof<term>> t : target) {
				If (t.size() == 3) {
					delta += make_transition(make_lpair(state,t[0]), make_lpair(alph,t[1]), make_lpair(targ,t[2]));
				}
			}
		}
		
		minimalize1(states, F, delta, alph);
	}
}
