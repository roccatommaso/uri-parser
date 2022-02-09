% Nicoletta_Davide_858101
% Dituri_Daniele_873401
% Rocca_Tommaso_

uri_parse(URIString, uri(Scheme, Userinfo, Host,
                         Port, _Path, _Query, _Fragment)) :-
    string_chars(URIString, X1),
    parse_scheme(X1, _, SchemeL),
    atom_string(SchemeL, Scheme),
    cut_string1(X1, SchemeL, X2),
    if_authority(X2, Userinfo, Host, Port, X3).

% Scheme ':' Other

parse_scheme([X | Xs], List, Scheme) :-
    X \= ':',
    append(List, [X], T),
    parse_scheme(Xs, T, Scheme), !.
parse_scheme([X | _Xs], List, Scheme) :-
    X = ':',
    Scheme = List.

if_authority([X | Xs], User, Host, Port, Other) :-
    X \= '/',
    User = _,
    Host = _,
    Port = 80,
    Other = Xs.
if_authority([X, Y | Xs], User, Host, Port, Other) :-
    X = '/',
    Y \= '/',
    User = _,
    Host = _,
    Port = 80,
    Other = Xs.
if_authority([X, Y | Xs], User, Host, Port, Other) :-
    X = '/',
    Y = '/',
    if_user(Xs, Var),
    parse_user(Xs, Var, UserL, _),
    atom_string(UserL, User),
    cut_string1(Xs, UserL, Y1),
    parse_host(Y1, Host, Y2),
    if_port(Y2, Port, Y3),
    Other = Y3.

if_user([], 0).
if_user([X | _Xs], Var) :-
    X = '@',
    Var = 1.
if_user([X | Xs], Var) :-
    X \= '@',
    if_user(Xs, Var).

parse_user(_, 0, User, _) :-
    User = _.
parse_user([X | Xs], 1, User, List) :-
    X \= '@',
    append(List, [X], T),
    parse_user(Xs, 1, User, T).
parse_user([X | _Xs], 1, User, List) :-
    X = '@',
    User = List.

cut_string1(List1, List2, List3) :-
    length(List2, X),
    cut_string2(List1, X, List3).

cut_string2([_X | Xs], Integer, Other) :-
    Integer \= 0,
    Int is Integer - 1,
    cut_string2(Xs, Int, Other).
cut_string2([_X | Xs], Integer, Other) :-
    Integer = 0,
    Other = Xs.

% foo://example.com:8042/over/there?name=ferret#nose
