#include <iostream>
#include <cassert>
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

template<class State, class Symbol> void minimalize2(lsetof<State> Q, lsetof<State> F, lsetof<transition<State,Symbol>> delta, lsetof<Symbol> alph) {
  cout << "Q = " << Q << endl;
  cout << "F = " << F << endl;
  cout << "delta = " << delta << endl;
  
  lsetof<lsetof<State>> classes;
  classes += F;
  classes += Q &~ F;
  
  int it = 0;
  
  lbool cont = true;
  
  While(cont) {
    lsetof<lsetof<State>> nclasses;
    cout << "classes = " << classes << endl << endl;
    cont = ffalse;
    
    for(auto& EC: classes) {
      for(State& q1: EC) {
        lsetof<State> EC1;
        for(State& q2: EC) {
          lbool b = true;
          for(auto& t1: delta) for(auto& t2: delta) 
            If(t1.src == q1 && t2.src == q2 && t1.symbol == t2.symbol)
            If(EXISTS(c, classes, memberof(t1.tgt, c) && !memberof(t2.tgt, c)))
              b &= ffalse;
          Ife(b) EC1 += q2;
          else cont = ftrue;
          }
        nclasses += EC1;
        }
      }

    classes = optimize(nclasses); it++;
    }

  cout << "number of iterations: " << it << endl << endl;
  
  cout << "classes: " << classes << endl;  
  }

int runningTest(int depth) {
	Domain dA("A");  
	lsetof<term> A = dA.getSet();
	
	lsetof<term> alphabet = A;
	
	lsetof<lpair<int, lvector<term>>> states, F;
	states += make_lpair(3, lvector<term>({}));
	states += make_lpair(4, lvector<term>({}));
	F += make_lpair(3, lvector<term>({}));
	lsetof<lvector<term>> vecs = newSet(lvector<term>({}));
	for (int i=0; i<depth; i++) {
		for (lvector<term> p : vecs) {
			states += make_lpair(0, p);
		}
		lsetof<lvector<term>> nvecs;
		for (term a : A) for (lvector<term> b : vecs) {
			lvector<term> nb = b;
			nb.push_back(a);
			nvecs += nb;
		}
		vecs = nvecs;
		for (lvector<term> p : vecs) {
			states += make_lpair(1, p);
		}
	}
	
	lsetof<transition<lpair<int, lvector<term>>, term>> delta;
	
	for (lpair<int, lvector<term>> s : states) for (term a : alphabet) {
		Ife (s.first == 3 || s.first == 4) {
			delta += make_transition(s, a, make_lpair(4, lvector<term>({})));
		} else { Ife (s.first == 0) {
			lvector<term> nextState = s.second;
			nextState.push_back(a);
			Ife (nextState.size() == depth) {
				delta += make_transition(s, a, make_lpair(1,nextState));
			} else {
				delta += make_transition(s, a, make_lpair(0, nextState));
			}	
		} else {
			Ife (s.second[0] != a) {
				delta += make_transition(s, a, make_lpair(4, lvector<term>({})));
			} else {
				lvector<term> nextState;
				for (int i=1; i<s.second.size(); i++) {
					nextState.push_back(s.second[i]);
				}
				Ife (nextState.size() == 0) {
					delta += make_transition(s, a, make_lpair(3, lvector<term>({})));
				} else {
					delta += make_transition(s, a, make_lpair(1, nextState));
				}
			}
		}}
	}
	
	minimalize1(states, F, delta, alphabet);
}

int main(int argc, char **argv) {
	initLois();
	sym.useUnicode();
	assert(argc == 2);
	int depth = atoi(argv[1]);
	
	for (int i=0; i<=depth; i++)
		runningTest(i);
	
	return 0;
}
