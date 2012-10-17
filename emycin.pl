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



% calcula_grau(A, B, R)
%
% Calcula um novo grau de exatidão (R) combinando o grau (A) da base de fatos
% com o grau (B) resultado da regra.
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



% grau_suficiente(G)
%
% Aceita se o grau de exatidão (G) é superior ao limiar.
%
grau_suficiente(G) :-
    limiar_de_exatidao(L),
    G >= L, !.



% insere_valor_r(LV, Valor, Grau, NLV)
%
% Insere um novo valor na lista de valores e retorna uma nova lista contendo o
% valor
insere_valor_r([], Valor, Grau, [[Valor, Grau]], Grau) :- !.

insere_valor_r([[Valor | [G]] | T], Valor, Grau, [[Valor, NG] | T], NG) :-
    calcula_grau(G, Grau, NG), !.

insere_valor_r([H | T], Valor, Grau, [H | NT], NG) :-
    insere_valor_r(T, Valor, Grau, NT, NG), !.



% insere_base_fatos(Attr, Valor, Gray)
%
% Insere na base de fatos um atributo (Attr) com seu respectivo Valor e Grau.
insere_base_fatos(Attr, Valor, Grau) :-
    base_de_fatos(BF),
    insere_base_fatos_r(BF, Attr, Valor, Grau, NBF, NG),
    retractall(base_de_fatos(_)),
    assertz(base_de_fatos(NBF)),
    monitora('A exatidao de '),
    monitora(Attr),
    monitora(' = '),
    monitora(Valor),
    monitora(' agora eh '),
    monitora(NG),
    monitoraln('.'),
    !.

% Fim da base de fatos, então insere novo atributo.
insere_base_fatos_r([], Attr, Valor, Grau, [[Attr, [Valor, Grau]]], Grau) :- !.

% Atributo encontrado na base de fatos, insere novo valor
insere_base_fatos_r([[Attr | LV] | T ], Attr, Valor, Grau, [[Attr | NLV] | T], NG) :-
    insere_valor_r(LV, Valor, Grau, NLV, NG), !.

% Varre recursivamente a base de fatos
insere_base_fatos_r([H | T], Attr, Valor, Grau, [H | NT], NG) :-
    insere_base_fatos_r(T, Attr, Valor, Grau, NT, NG), !.



% hipoteses(Attr, Hip)
%
% Busca valores na base de fatos para um dado atributo (Attr) e retorna como
% lista de hipóteses (Hip).
hipoteses(Attr, Hip) :-
    base_de_fatos(BF),
    hipoteses_r(BF, Attr, Hip), !.



% hipoteses_r(BF, Attr, Hip)
%
% Varre recursivamente a base de fatos (BF) até encontrar o atributo dado
% (Attr) e retorna a lista de valores como hipóteses.
hipoteses_r([[Attr | Hip] | _], Attr, Hip) :- !.
hipoteses_r([_ | T], Attr, Hip) :-
    hipoteses_r(T, Attr, Hip), !.



% parse_regra(R, A, G, C)
%
% Analisa uma regra (R) e retornar a lista de antecedentes (A), o grau da
% consequente (G) e a dupla [Atributo, Valor] (C).
parse_regra([-> | [G, C]], [], G, C) :- !.
parse_regra([ H | T ], [ H | A ], G, C) :-
    parse_regra(T, A, G, C), !.



% grau_hipotese_r(H, V, G)
%
% Varre lista de hipoteses (V) e encontra o grau (G) referente a um dado
% valor (V).
grau_hipotese_r([[V | [G]] | _], V, G) :- !.
grau_hipotese_r([_|T], V, G) :-
    grau_hipotese_r(T, V, G), !.



% resolve_antecedentes(R, Ant, G)
%
% Resolve antecedentes de uma regra e retorna em G o menor grau entre elas.
resolve_antecedentes_r(_, [], 1) :- !.
resolve_antecedentes_r(R, [[Attr, Valor] | T], G) :-
    busca_antecedente(R, Attr, Hip),
    grau_hipotese_r(Hip, Valor, G1),
    resolve_antecedentes_r(R, T, G2),
    G is min(G1, G2), !.



% busca_antecedente(R, Attr, Hip)
%
% Tenta busca um atributo de um antecedente de regra, se não encontrar pergunta
% ao usuário.
busca_antecedente(_, Attr, Hip) :-
    busca(Attr, Hip), !.

busca_antecedente(R, Attr, Hip) :-
    pergunta(R, Attr, RV),
    write('Qual eh o seu grau de exatidao (um numero entre -1 e 1) ? '),
    read(RG),
    writeln(''),
    insere_base_fatos(Attr, RV, RG),
    hipoteses(Attr, Hip), !.

pergunta(R, Attr, Valor) :-
    writeln(''),
    write('Me informe sobre '),
    write(Attr),
    write(' ? '),
    read(Valor),
    responde_porque(R, Valor).

responde_porque(R, porque) :-
    writeln('Estou tentando usar a regra:'),
    writeln(R), !, fail.
responde_porque(_, _).


% aplica_regra(R, Ant, Grau, Attr, Valor)
%
% Aplica uma regra (resolve antecedentes e insere consequente na base de fatos)
aplica_regra(R, Ant, Grau, Attr, Valor) :-
    resolve_antecedentes_r(R, Ant, GrauAnt),
    grau_suficiente(GrauAnt),
    G is GrauAnt * Grau,
    insere_base_fatos(Attr, Valor, G),
    !.

% Se falhou na aplicação, abandona a regra
aplica_regra(_, _, _, Attr, _) :-
    monitora('Abandonando esta regra para '),
    monitora(Attr),
    monitoraln('.'),
    !, fail.



% busca_r(BR, Attr, Hip)
%
% Busca e aplica regras com consequente = atributo recursivamente.
% Retorna a lista de hipóteses (Hip).
busca_r([Regra|Resto], Attr, Hip) :-
    parse_regra(Regra, Ant, Grau, [Attr | [Valor]]),
    monitora('Tentando a regra '),
    monitora(Regra),
    monitoraln('.'),
    aplica_regra(Regra, Ant, Grau, Attr, Valor),
    busca_r(Resto, Attr, Hip), !.

% Falhou parse_regra ou aplica_regra, ignora
busca_r([_|T], Attr, Hip) :-
    busca_r(T, Attr, Hip), !.

% Fim da base de regras
% Retorna valores da base de fatos como hipotese
busca_r([], Attr, Hip) :-
    hipoteses(Attr, Hip),
    !.



% busca(Attr, Hip)
%
% Resolve um atributo buscando na base de regras.
% Retorna a lista de hipóteses (Hip).
busca(Attr, Hip) :-
    monitora('Descobrindo sobre '),
    monitora(Attr),
    monitoraln('.'),
    base_de_regras(BR),
    busca_r(BR, Attr, Hip), !.



diagnostico(Attr) :-
    retractall(base_de_fatos(_)),
    assertz(base_de_fatos([])),
    busca(Attr, H),
    write('As hipoteses sao as seguintes: '),
    writeln(H), !.

diagnostico(_) :-
    write('As hipoteses sao as seguintes: '),
    writeln([]).
