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
