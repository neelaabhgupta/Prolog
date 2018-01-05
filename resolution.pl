resolution(InputFile):-
	load_dyn(InputFile),
	resolve.

append([], L, L).
append([X|L],M, [X|N]) :-
	append(L,M,N).

length(List, Length) :-
        ( var(Length) ->
          length(List, 0, Length)
        ;
          Length >= 0,
          length1(List, Length) ).

length([], Length, Length).
length([_|L], N, Length) :-
        N1 is N+1,
        length(L, N1, Length).

predicates_no(Z):- findall(X, myClause(X,Y), Z).
p_no(X):- predicates_no(Z), length(Z,L), Y is L+1, append(Z,[Y],X).
predicates(Z):- findall(Y, myClause(X,Y), Z).
temp_p(X):- predicates(Z), rev(Z,X).
t_predicatess([neg(X),H|T]):- temp_p([H|T]), myQuery(_,X).
t_predicatess([X,H|T]):- temp_p([H|T]), myQuery(_,neg(X)).

predicatess(X):- t_predicatess(Z), rev(Z,X).

bracket_open(or(A , B), Z) :-
  bracket_open(A, X),
  bracket_open(B, Y),
  append(X, Y, Z).

bracket_open(neg(A), [neg(A)]).
bracket_open(A,[A]).

simplify(A, Z) :-
   atom(A),
   Z = [[A]].

simplify(neg(A), Z) :-
   atom(A),
   Z = [[neg(A)]].

simplify(or(A , B), Z) :-
  bracket_open(A, A1),
  bracket_open(B, B1),
  append(A1, B1, C),
  Z = [C].

transform([],[]).
transform([H|T],Z):-
	simplify(H,X),
	transform(T,Z2),
	append(Z2,X,Z).

rev([],[]).
rev([H|T], X) :-
	rev(T,X2),
	append(X2, [H], X).

del(X,[],[]).
del(X,[X|Tail],Tail).
    
    del(X,[Y|Tail],[Y|Tail1]):-
        del(X,Tail,Tail1).

main(Y):-
	predicatess(X),
	transform(X,Z),
	rev(Z,Y).

member(X, [X|_]).
member(X, [_|Ys]) :-
	member(X,Ys).

make_set([],[]).
make_set([X|Y],Y2) :- member(X,Y), !,
	make_set(Y,Y2).
make_set([X|Y],[X|Y2]) :- make_set(Y,Y2).

my_last(X,[X]).
my_last(X,[_|L]) :- my_last(X,L).

comparing(X,Y,R):- comparing(X,Y,R,Y).
comparing([],_,[],_).
comparing([H|T], Y, R, []):- R = [H|Ts], comparing(T,Y,Ts,Y).
comparing([H1|T1],Y,R,[H2|T2]):- 
	(H1 == neg(H2) -> 
		comparing(T1, Y, R, Y)
	;	(neg(H1) == H2 ->	
			comparing(T1,Y,R,Y)
		;	comparing([H1|T1], Y, R, T2)
		)
	).

remove_brackets([X],X).

resolve:-
	p_no(PN),
	length(PN,L),
	Ls is L+1,
	main(Y),
	res_helper(Y,Z),
	my_last(X,Z),
	(X == [] -> 
		write('resolution(success)'),
		write('\n'),
		print_res(Z,PN,Ls)
	;	write('resolution fail'),
		write('\n')
	).

print_res([[]],[H1,H2|T],L):-
	write('resolution('),
	write(H1),
	write(', '),
	write(H2),
	write(', '),
	write('empty'),
	write(', '),
	write(L),
	write(').').
print_res([H|T],[H1,H2|T1],L):-
	brackets_put(H,X),
	my_last(Y,X),
	remove_brackets(Y,Ys),
	write('resolution('),
	write(H1),
	write(', '),
	write(H2),
	write(', '),
	write(Ys),
	write(', '),
	write(L),
	write(').'),
	append([L],T1,T2),
	Ls is L+1,
	write('\n'),
	print_res(T,T2,Ls).


brackets_put([],[]).
brackets_put([H],X):- X = [[H]].
brackets_put([H1,H2|T],[T1|Xs]):- Y = [or(H1, H2)], append(Y,T,T1), brackets_put(T1, Xs).

res_helper([[]],[]).
res_helper([H],[H]).
res_helper([H1,H2|T],[Rs|Z]):- 
	comparing(H1,H2,R1),
	comparing(H2,H1,R2),
	append(R1,R2,R),
	make_set(R,Rs),
	append([Rs],T,T1),
	res_helper(T1,Z).
