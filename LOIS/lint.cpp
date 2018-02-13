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

void lintTest() {
	Domain dA("A");
	lsetof<term> A = dA.getSet();
	
	lsetof<term> alphabet = A;
	
	lsetof<lpair<int, lvector<term>>> states, F;
	states += make_lpair(0, lvector<term>({}));
	states += make_lpair(4, lvector<term>({}));
	for (term a : A) {
		states += make_lpair(1, lvector<term>({a}));
	}
	for (term a : A) for (term b: A) {
		states += make_lpair(2, lvector<term>({a,b}));
		states += make_lpair(3, lvector<term>({a,b}));
		F += make_lpair(2, lvector<term>({a,b}));
	}
	
	lsetof<transition<lpair<int,lvector<term>>, term>> delta;
	for (lpair<int, lvector<term>> s: states) for (term a : alphabet) {
		Ife (s.first == 0) {
			delta += make_transition(s, a, make_lpair(1, lvector<term>({a})));
		} else { Ife(s.first == 1) {
			Ife (s.second[0] <= a) {
				delta += make_transition(s, a, make_lpair(4, lvector<term>({})));
			} else {
				delta += make_transition(s, a, make_lpair(2, lvector<term>({s.second[0],a})));
			}
		} else { Ife(s.first == 2) {
			Ife (s.second[0] < a && s.second[1] > a) {
				delta += make_transition(s, a, make_lpair(3, lvector<term>({a,s.second[1]})));
			} else {
				delta += make_transition(s, a, make_lpair(4, lvector<term>({})));
			}
		} else { Ife(s.first == 3) {
			Ife (s.second[0] < a && s.second[1] > a) {
				delta += make_transition(s, a, make_lpair(2, lvector<term>({s.second[0], a})));
			} else {
				delta += make_transition(s, a, make_lpair(4, lvector<term>({})));
			}
		} else {
			delta += make_transition(s, a, s);
		}}}}
	}
	
	minimalize1(states, F, delta, alphabet);
}

void lmaxTest() {
	Domain dA("A");  
	lsetof<term> A = dA.getSet();
	
	lsetof<term> alphabet = A;
	
	lsetof<lvector<term>> states, F;
	states += lvector<term>({});
	for (term a : A) {
		states += lvector<term>({a});
		F += lvector<term>({a,a});
	}
	for (term a : A) for (term b : A) {
		states += lvector<term>({a,b});
	}
	
	lsetof<transition<lvector<term>, term>> delta;
	for (lvector<term> s: states) for (term a : alphabet) {
		Ife (s.size() < 2) {
			lvector<term> ns = s;
			ns.push_back(a);
			delta += make_transition(s, a, ns);
		} else {
			Ife (s[0] > s[1]) {
				lvector<term> ns;
				ns.push_back(s[0]);
				ns.push_back(a);
				delta += make_transition(s, a, ns);
			} else {
				lvector<term> ns;
				ns.push_back(s[1]);
				ns.push_back(a);
				delta += make_transition(s, a, ns);
			}
		}
	}
	
	minimalize1(states, F, delta, alphabet);
}

int main() {
	initLois();
	sym.useUnicode();
	
	lmaxTest();
	
	return 0;
}
