% Name: Jaehyung You
% Filename: youja.pl
% Description: Prolog for Homework 6
% 2019 - 03 - 15

% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X,_), female(X).
isFather(Y) :- parent(Y,_), male(Y).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z,X), parent(Z,Y), not(X = Y).

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y) :- female(X), sibling(X,Y).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y).
siblingInLaw(X,Y) :- married(X,Z), sibling(Z,Y).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
% Reference : https://stackoverflow.com/questions/23590823/uncle-or-aunt-without-sibling-fact 
aunt(X,Y) :- female(X), sibling(X,Z), parent(Z,Y).
aunt(X,Y) :- female(X), married(X,Z), sibling(Z,W), parent(W,Y).
uncle(X,Y) :- male(X), sibling(X,Z), parent(Z,Y).
uncle(X,Y) :- male(X), married(X,Z), sibling(Z,W), parent(W,Y).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(Z,X), sibling(Z,W), parent(W,Y).
cousin(X,Y) :- parent(Z,X), married(X,Z), sibling(Z,W), parent(W,Y).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).

% Extra credit: Define the predicate `related/2`.


%%
% Part 2. Language implementation (see course web page)
%%

cmdlit(C) :- number(C).
cmdlit(C) :- string(C).
cmdlit(t). % For boolean values
cmdlit(f). % For boolean values

cmd(C,S1,S2) :-  cmdlit(C), S2 = [C|S1].

cmd(add, [X,Y|Z],S2) :-  W is (X + Y), S2 = [W|Z].

cmd(lte, [X,Y|Z], S2) :-  X =< Y, S2 = [t|Z], !.
cmd(lte, [X,Y|Z], S2) :-  X > Y, S2 = [f|Z], !.

cmd(if(C,_), [t|S1], S2) :- prog(C,S1,S2).
cmd(if(_,C), [f|S1], S2) :- prog(C,S1,S2).

prog([], S1, S1).
prog([C|CL], S1, S2) :- cmd(C, S1, X), prog(CL, X, S2), !.
