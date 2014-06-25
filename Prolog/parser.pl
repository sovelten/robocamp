%Gramatica do Parser
board(Z) --> line(X), {Z = [X]}.
board(Z) --> line(X), board(Y), {Z = [X | Y]}.

line(L) --> sqr(X), [10], {L = [X]}.
line(L) --> sqr(X), line(Y), {L = [X | Y]}.

sqr(S) --> resource(S).
sqr(S) --> robot(S).
sqr(S) --> wall(S).
sqr(S) --> empty(S).

wall(W) --> "XX", {W = "X"}.
empty(E) --> "..", {E = "."}.
robot(R) --> player(P), digit(N), {R = [P,N]}.
resource(R) --> "R", digit(N), {R = ["R",N]}.

digit(D) --> [D], {code_type(D, digit)}.

digits([H|T]) --> digit(H), !, digits(T).
digits([]) --> [].

num(N) --> digits(L), {number_codes(N,L)}.

player(P) --> "A", {P = "A"}.
player(P) --> "B", {P = "B"}.

%Predicados para parsing do tabuleiro todo ou
% linha a linha
parse_board(L, V) :- board(V, L, []).
parse_line(L, V) :- line(V, L, []).

main :- write("hello").
