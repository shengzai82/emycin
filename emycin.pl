base_de_regras([
[ [idade, jovem], [especie, humana], [sexo, masculino], -> , 1.0, [chamado, menino] ],
[ [idade, jovem], [especie, humana], [sexo, feminino], -> , 1.0, [chamado, menina] ],
[ [idade, adulto], [especie, humana], [sexo, masculino], -> , 1.0, [chamado, homem] ],
[ [idade, adulto], [especie, humana], [sexo, feminino], -> , 1.0, [chamado, mulher] ],
[ [idade, jovem], [especie, cao], ->, 1.0, [chamado, filhote] ],
[ [idade, adulto], [especie, cao], [sexo, feminino], ->, 1.0, [chamado, cadela] ],
[ [idade, adulto], [especie, cao], ->, 0.5, [chamado, cao] ],
[ [pernas, 2], ->, 1.0, [especie, humana] ],
[ [pernas, 4], ->, -1.0, [especie, humana] ],
[ [pernas, 4], ->, 0.5, [especie, cao] ],
[ [altura, baixa], ->, 0.5, [especie, cao] ],
[ [altura, alta], ->, 0.5, [especie, humana] ]
]).

limiar_de_exatidao(0.2).

:- dynamic monitoramento/0.

monitora(Str) :-
    monitoramento,
    write(Str), !.
monitora(_) :- !.

monitoraln(Str) :-
    monitoramento,
    writeln(Str), !.
monitoraln(_) :- !.

calcula_grau(A, B, R) :-
    A > 0,
    B > 0,
    R is A + B - (A * B), !.

calcula_grau(A, B, R) :-
    A < 0,
    B < 0,
    R is A + B + (A * B), !.

calcula_grau(A, B, R) :-
    R is (A + B) / (1 - min(abs(A), abs(B))), !.


grau_suficiente(G) :-
    limiar_de_exatidao(L),
    G >= L, !.


insere_valor([], Attr, Valor, [[Attr, Valor]]) :- !.

insere_valor([[Valor | [G]] | T], Valor, Grau, [[Valor, NG] | T]) :-
    calcula_grau(G, Grau, NG), !.

insere_valor([H | T], Valor, Grau, [H | NT]) :-
    insere_valor(T, Valor, Grau, NT), !.

fatos_put([], Attr, Valor, Grau, [[Attr, [Valor, Grau]]]) :- !.

fatos_put([[Attr | LV] | T ], Attr, Valor, Grau, [[Attr | NLV] | T]) :-
    insere_valor(LV, Valor, Grau, NLV), !.

fatos_put([H | T], Attr, Valor, Grau, [H | NT]) :-
    fatos_put(T, Attr, Valor, Grau, NT), !.

fatos_put(Attr, Valor, Grau) :-
    base_de_fatos(BF),
    fatos_put(BF, Attr, Valor, Grau, NBF),
    retractall(base_de_fatos(_)),
    assertz(base_de_fatos(NBF)),
    monitora('A exatidao de '),
    monitora(Attr),
    monitora(' = '),
    monitora(Valor),
    monitora(' agora eh '),
    monitora(Grau),
    monitoraln('.'),
    !.

fatos_get([[Attr | L] | _], Attr, L) :- !.
fatos_get([_ | T], Attr, L) :-
    fatos_get(T, Attr, L), !.

fatos_get(Attr, L) :-
    base_de_fatos(BF),
    fatos_get(BF, Attr, L), !.


parse_regra([-> | [G, C]], [], G, C) :- !.
parse_regra([ H | T ], [ H | A ], G, C) :-
    parse_regra(T, A, G, C), !.

get_grau([[V | [G]] | _], V, G) :- !.
get_grau([_|T], V, G) :-
    get_grau(T, V, G), !.


resolve_antecedentes([], 0) :- !.
resolve_antecedentes([[Attr, Value] | T], G) :-
    busca(Attr, L),
    get_grau(L, Value, G1),
    resolve_antecedentes(T, G2),
    calcula_grau(G1, G2, G), !.



tenta_regra(Attr, A, G1, V) :-
    resolve_antecedentes(A, G2),
    grau_suficiente(G2),
    calcula_grau(G1, G2, G),
    fatos_put(Attr, V, G),
    !.

tenta_regra(Attr, _, _, _) :-
    monitora('Abandonando esta regra para '),
    monitora(Attr),
    monitoraln('.'),
    !, fail.


busca_r([R|T], Attr, L) :-
    parse_regra(R, A, G1, [Attr | [V]]),
    monitora('Tentando a regra '),
    monitora(R),
    monitoraln('.'),
    tenta_regra(Attr, A, G1, V),
    busca_r(T, Attr, L), !.

busca_r([_|T], Attr, L) :-
    busca_r(T, Attr, L), !.

busca_r([], Attr, L) :-
    fatos_get(Attr, L), !.

busca_r([], Attr, L) :-
    writeln(''),
    write('Me informe sobre '),
    write(Attr),
    write(' ? '),
    read(V),
    write('Qual eh o seu grau de exatidao (um numero entre -1 e 1) ? '),
    read(G),
    writeln(''),
    fatos_put(Attr, V, G),
    fatos_get(Attr, L), !.


busca(Attr, L) :-
    monitora('Descobrindo sobre '),
    monitora(Attr),
    monitoraln('.'),
    base_de_regras(BR),
    busca_r(BR, Attr, L), !.



diagnostico(Attr) :-
    retractall(base_de_fatos(_)),
    assertz(base_de_fatos([])),
    busca(Attr, H),
    write('As hipoteses sao as seguintes: '),
    writeln(H).

