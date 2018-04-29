% Allen Simpson
% CSCI 305 Prolog Lab 2


%Parent Rules
mother(M,C) :- 
    parent(M,C),
    female(M).

father(F,C) :-
    parent(F,C),
    male(F).

%Marrige Rules
spouse(H,W):-
    married(H,W).
spouse(W,H):-
    married(H,W).

%Child Rules
child(C,P) :-
    parent(P,C).

son(S,P) :- 
    child(S,P),
    male(S).

daughter(D,P) :- 
    child(D,P),
    female(D).
%This is to simplify the sibling(A,B) rule, and allow for the halfsibling(A,B) rule to be reasonable.
hasSameMother(A,B) :-
    mother(M,A),
    mother(M,B).
%This is to simplify the sibling(A,B) rule, and allow for the halfsibling(A,B) rule to be reasonable.
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

%Extended Family rules
uncle(U,A) :-       %Uncle is brother of parent
    parent(P,A),
    brother(U,P).
uncle(U,A) :-       %Uncle is male spouse of a parent's sibling
    parent(P,A),
    sibling(S,P),
    spouse(U,S),
    male(U).

aunt(Au,B) :-       %Aunt is a sister of parent
    parent(P,B),
    sister(Au,P).
aunt(Au,B) :-       %Aunt is female spouse of a parent's sibling
    parent(P,B),
    sibling(S,P),
    spouse(Au,S),
    female(Au).

%Grandparent rules
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

%Family Tree Rules:
ancestor(An,B):-
    parent(An,B).
ancestor(An,B):-
    parent(P,B),
    ancestor(An,P).

descendant(D,A):- %reused ancestor code rather than redefining, simplifes code
    ancestor(A,D).

%Age Related Rules!
%finding age
age(Person,Age):-
    born(Person,Born),
    died(Person,Died),
    !,
    Age is Died-Born.
age(Person,Age):-
        born(Person,Born),
        Age is 2018-Born.

%Peoples' ages based on deathyear-birthyear || 2018-birthyear
older(A,B):-
    age(A,Aage),
    age(B,Bage),
    Aage>Bage.

younger(A,B):-
    age(A,Aage),
    age(B,Bage),
    Aage<Bage.

regentWhenBorn(Per,Reg) :-
    reigned(Reg,RegStart,RegEnd),
    born(Per,PerBirth),
    PerBirth>RegStart,
    PerBirth<RegEnd.

cousin(Cus,Per) :-
    parent(S,Cus),
    parent(P,Per),
    sibling(P,S).
