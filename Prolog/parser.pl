:- use_module(library(dcg/basics)).

%
%Gramatica do Parser
board(Z) --> line(X), {Z = [X]}.
board(Z) --> line(X), board(Y), {Z = [X | Y]}.
line(L) --> sqr(X), [10], {L = [X]}.
line(L) --> sqr(X), line(Y), {L = [X | Y]}.
sqr(S) --> [X|T], {square([X|T],S)}.

robot(R) --> player(P), integer(N), {R = [P,N]}.
resource(R) --> "R", integer(N), {R = ["R",N]}.

player(P) --> "A", {P = "A"}.
player(P) --> "B", {P = "B"}.

num(N) --> integer(N).

parse_board(L, V) :- board(V, L, []).
parse_line(L, V) :- line(V, L, []).

square(L,S) :- isEmpty(L), S = ".".
square(L,S) :- isWall(L), S = "X". 
square([X|T],S) :- isRobot([X|T]), number_codes(N,T), S = [X,N].
square([X|T],S) :- isResource([X|T]), number_codes(N,T), S = [X,N].
                   
isNumber([H|T]) :- code_type([H],digit), isNumber(T).
isNumber([H|[]]) :- code_type([H],digit).
isNumber([]) :- false.

isEmpty(L) :- not( dif(L,"..")).

isWall(L) :- not( dif(L,"XX")).

isRobot([X|T]) :- isPlayer(X), isNumber(T).

%65 e 66 sao codigos dos caracteres 'A' e 'B'
isPlayer(C) :- C == 65.
isPlayer(C) :- C == 66.
                  
% 82 equivale ao caracter 'R'
isResource([X|T]) :- X == 82, isNumber(T).
