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

void fifoTest(int depth) {
	Domain dA("A");  
	lsetof<term> A = dA.getSet();
	
	lsetof<lpair<int, term>> alphabet;
	for (term a : A) {
		alphabet += make_lpair(0, a);
		alphabet += make_lpair(1, a);
	}
	
	lsetof<lpair<int, lpair<lvector<term>, lvector<term>>>> states, F;
	states += make_lpair(0, make_lpair(lvector<term>({}),lvector<term>({})));
	states += make_lpair(1, make_lpair(lvector<term>({}),lvector<term>({})));
	F += make_lpair(0, make_lpair(lvector<term>({}),lvector<term>({})));
	lsetof<lvector<term>> vecs = newSet(lvector<term>({}));
	for (int i=1; i<=depth; i++) {
		lsetof<lvector<term>> nvecs;
		for (term a : A) for (lvector<term> b : vecs) {
			lvector<term> nb = b;
			nb.push_back(a);
			nvecs += nb;
		}
		vecs = nvecs;
		for (lvector<term> o : vecs) {
			for (int i=0; i<=o.size(); i++) {
				lvector<term> a,b;
				for (int j=0; j<i; j++) {
					a.push_back(o[j]);
				}
				for (int j=i; j<o.size(); j++) {
					b.push_back(o[j]);
				}
				states += make_lpair(0, make_lpair(a,b));
				F += make_lpair(0, make_lpair(a,b));
			}
		}
	}
	
	lsetof<transition<lpair<int, lpair<lvector<term>,lvector<term>>>, lpair<int,term>>> delta;
	for (lpair<int, lpair<lvector<term>, lvector<term>>> s : states) for (lpair<int, term> a : alphabet) {
		Ife (s.first == 1) {
			delta += make_transition(s,a,s);
		} else { Ife (a.first == 0) {
			Ife (s.second.first.size() + s.second.second.size() == depth) {
				delta += make_transition(s,a,make_lpair(1,make_lpair(lvector<term>({}), lvector<term>({}))));
			} else {
				lpair<int, lpair<lvector<term>,lvector<term>>> ns = s;
				ns.second.first.push_back(a.second);
				delta += make_transition(s,a,ns);
			}
		} else {
			lpair<int, lpair<lvector<term>,lvector<term>>> ns = s;
			
			If (ns.second.second.size() == 0) {
				for (int i=ns.second.first.size()-1; i>=0; i--) {
					ns.second.second.push_back(ns.second.first[i]);
				}
				ns.second.first = lvector<term>({});
			}
			
			Ife (ns.second.second.size() == 0) {
				delta += make_transition(s,a,make_lpair(1,make_lpair(lvector<term>({}), lvector<term>({}))));
			} else { Ife(ns.second.second[ns.second.second.size()-1] != a.second) {
				delta += make_transition(s,a,make_lpair(1,make_lpair(lvector<term>({}), lvector<term>({}))));
			} else {
				lvector<term> newsec;
				for (int i=0; i<ns.second.second.size()-1; i++) {
					newsec.push_back(ns.second.second[i]);
				}
				ns.second.second = newsec;
				delta += make_transition(s,a,ns);
			}}
		}}
	}
	
	minimalize1(states, F, delta, alphabet);
}

int main(int argc, char **argv) {
	assert(argc == 2);
	int depth = atoi(argv[1]);
	
	initLois();
	sym.useUnicode();
	
	fifoTest(depth);

	return 0;
}
