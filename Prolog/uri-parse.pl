% Nicoletta_Davide_858101
% Dituri_Daniele_873401
% Rocca_Tommaso_

uri_parse(URIString, uri(Scheme, _Userinfo, _Host,
                         _Port, _Path, _Query, _Fragment)) :-
    string_chars(URIString, X1),
    parse_scheme(X1, Scheme, X2).

parse_scheme(X, Scheme, X2) :-
    cut_list(X, ':', X2),
    separa1(X, Y, X2),
    atom_string(Y, Scheme), !.

cut_list([X | Xs], X, Xs).
cut_list([X | Xs], Y, Ys) :-
    cut_list(Xs, Y, Ys).

separa1(List, P1, P2) :-
    length(List, X),
    length(P2, Y),
    Z is X - Y,
    separa2(List, P1, Z).

separa2([_ | _], _, 1).
separa2([X | Xs], [X | Ys], N) :-
    M is N - 1,
    separa2(Xs, Ys, M).
