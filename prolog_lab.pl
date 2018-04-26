% Allen Simpson
% CSCI 305 Prolog Lab 2

mother(M,C) :- 
    parent(M,C),
    female(M).

father(F,C) :-
    parent(F,C),
    male(F).

spouse(H,W):-
    married(H,W).
spouse(W,H):-
    married(H,W).

child(C,P) :-
    parent(P,C).

son(S,P) :- 
    child(S,P),
    male(S).

daughter(D,P) :- 
    child(D,P),
    female(D).

hasSameMother(A,B) :-
    mother(M,A),
    mother(M,B).

hasSameFather(A,B) :-
    father(F,A),
    father(F,B).

%Only looking at Full Siblings
sibling(A,B) :- 
    hasSameMother(A,B),
    hasSameFather(A,B),
    not(A=B).

%Looking at Halfsiblings
halfSibling(A,B) :-
    (
        hasSameFather(A,B);
        hasSameMother(A,B)
    ),
    not(sibling(A,B)),
    not(A=B).


brother(Br,A) :-
    (
        sibling(Br,A);
        halfSibling(Br,A)
    ),
    male(Br).

sister(Sr,A) :-
    (
        sibling(Sr,A);
        halfSibling(Sr,A)
    ),
    female(Sr).

uncle(U,A) :-
    parent(P,A),
    brother(U,P).
uncle(U,A) :-
    parent(P,A),
    sibling(S,P),
    spouse(U,S),
    male(U).

aunt(Au,B) :-
    parent(P,B),
    sister(Au,P).
aunt(Au,B) :-
    parent(P,B),
    sibling(S,P),
    spouse(Au,S),
    female(Au).

grandparent(Gp,A) :-
    parent(Gp,P),
    parent(P,A).     

grandmother(Gm,A) :-
    grandparent(Gm,A),
    female(Gm).

grandfather(Gf,A) :-
    grandparent(Gf,A),
    male(Gf).

grandchild(Gc,A) :-
    grandparent(A,Gc).

ancestor(An,B):-
    parent(An,B).
ancestor(An,B):-
    parent(P,B),
    ancestor(An,P).

descendant(D,A):-
    ancestor(A,D).

%people are older based soley on birthyear
older(A,B):-
    born(A,Abirth),
    born(B,Bbirth),
    Abirth<Bbirth.
%people are older based soley on birthyear
younger(A,B):-
    born(A,Abirth),
    born(B,Bbirth),
    Abirth>Bbirth.

regentWhenBorn(Per,Reg) :-
    reigned(Reg,RegStart,RegEnd),
    born(Per,PerBirth),
    PerBirth>RegStart,
    PerBirth<RegEnd.

cousin(Cus,Per) :-
    parent(S,Cus),
    parent(P,Per),
    sibling(P,S).
