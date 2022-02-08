% Nicoletta_Davide_858101
% Dituri_Daniele_873401
% Rocca_Tommaso_

uri_parse(URIString, uri(Scheme, _Userinfo, _Host,
                         _Port, _Path, _Query, _Fragment)) :-
    string_chars(URIString, X1),
    parse_scheme(X1, _, SchemeL),
    atom_string(SchemeL, Scheme),
    cut_string1(X1, SchemeL, X2).

parse_scheme([X | Xs], List, Scheme) :-
    X \= ':',
    append(List, [X], T),
    parse_scheme(Xs, T, Scheme), !.

parse_scheme([X | _Xs], List, Scheme) :-
    X = ':',
    Scheme = List.

cut_string1(List1, List2, List3) :-
    length(List2, X),
    cut_string2(List1, X, List3).

cut_string2([_X | Xs], Integer, Result) :-
    Integer \= 0,
    Int is Integer - 1,
    cut_string2(Xs, Int, Result).

cut_string2([_X | Xs], Integer, Result) :-
    Integer = 0,
    Result = Xs.
