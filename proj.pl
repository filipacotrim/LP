%Filipa Santos Baptista Alves Cotrim 95572

:-[codigo_comum].

%------------------------------------------------------------------------------------
% 3.1.1
% obtem_letras_palavras(Lst_Pals,Letras) em que Lst_Pals eh uma lista de palavras e 
% Letras eh a lista ordenada cujos elementos sao listas com as letras de cada palavra
%de Lst_Pals.
%------------------------------------------------------------------------------------

obtem_letras_palavras(Lst_Pals,Letras) :- 
    obtem_letras_palavras(Lst_Pals,Letras_Temp,[]),
    sort(Letras_Temp,Letras).

obtem_letras_palavras([],Ac,Ac).

obtem_letras_palavras([P|R],Letras,Ac) :-
    %atom_chars coloca os caracteres da palavra P na lista Chars
    atom_chars(P,Chars),
    %vamos juntando ao acumulador os caracteres de cada palavra 
    append(Ac,[Chars],Junto),
    obtem_letras_palavras(R,Letras,Junto).

%------------------------------------------------------------------------------------
%3.1.2
%espaco_fila(Fila,Esp) em que Fila eh uma fila(quer seja linha ou coluna) de uma 
%grelha e Esp eh um espaco de Fila.
%------------------------------------------------------------------------------------

%nesta funcao tem-se em conta se uma posicao eh posicao disponivel (variavel 
% anonima) ou se eh uma posicao indisponivel (cardinal).

espaco_fila(Fila,Esp) :- espaco_fila(Fila,Esp,[]).

espaco_fila([],Esp,Esp).

%ao encontrar uma posicao disponivel adiciona-a sempre ao acumulador para quando
%encontrar um '#' o acumulador ser avaliado no seu comprimento (Comp >= 3).
espaco_fila([P|R],Esp,Ac) :-
    P \== #,
    R \== [],
    append(Ac,[P],Junto),
    espaco_fila(R,Esp,Junto).

%no entanto, se o resto R da Fila for vazio, ou seja, nao eh encontrado um '#'
%o comprimento do acumulador tem de ser avaliado ah mesma.
espaco_fila([P|R],Esp,Ac) :-
    P \== #,
    R == [],
    append(Ac,[P],Junto),
    length(Junto,Comp),
    Comp >= 3,
    espaco_fila([],Esp,Junto).

espaco_fila([P|_],Esp,Ac) :-
    P == #,
    length(Ac,Comp),
    Comp >= 3,
    espaco_fila([],Esp,Ac).

%se a fila nao acabar depois de encontrar um '#', isto eh, se o resto R nao for 
%vazio coloco o acumulador a [] para um novo espaco ser avaliado.
espaco_fila([P|R],Esp,_) :-
    R \== [],
    P == #,
    espaco_fila(R,Esp,[]).

%------------------------------------------------------------------------------------
%3.1.3
%espacos_fila(Fila,Espacos) onde Espacos eh a lista de todos os espacos de Fila,
% da esquerda para a direita.
%------------------------------------------------------------------------------------

espacos_fila(Fila,Espacos) :- 
    bagof(Espacos,espaco_fila(Fila,Espacos),Espacos).

espacos_fila(_,[]).

%------------------------------------------------------------------------------------
%3.1.4
%espacos_puzzle(Grelha,Espacos) em que Grelha eh uma grelha e Espacos eh a lista de 
%espacos de Grelha.
%------------------------------------------------------------------------------------

espacos_puzzle(Grelha,Espacos) :-
    espacos_matriz(Grelha,Esp_Matriz),
    espacos_transp(Grelha,Esp_Transp),
    append(Esp_Matriz,Esp_Transp,Espacos),
    !.

espacos_transp(Grelha,Esp_Transp) :-
    mat_transposta(Grelha,Transp),
    espacos_matriz(Transp,Esp_Transp,[]).

%funcao auxiliar que acumula todos os espacos de uma grelha.
espacos_matriz(Grelha,Esp) :-
    espacos_matriz(Grelha,Esp,[]).

espacos_matriz([],Esp,Esp).

espacos_matriz([P|R],Esp,Ac) :-
    espacos_fila(P,Esp_Temp),
    append(Ac,Esp_Temp,Ac_Temp),
    espacos_matriz(R,Esp,Ac_Temp).

%------------------------------------------------------------------------------------
%3.1.5
%espacos_com_posicoes_comuns(Espacos,Esp,Esps_com) em que Espacos eh uma lista de 
%espacos, Esp eh um espaco e Esps_com eh a lista de espacos com variaveis em comum 
%com Esp, exceptuando o proprio Esp. O conjunto de espacos Esps_com deve estar 
%de acordo com a ordem de Esps.
%------------------------------------------------------------------------------------

pertence(El,[P|_]) :- El == P.

pertence(El,[P|R]) :-
    El \== P,
    pertence(El,R).  

%funcao auxiliar comuns recebe um espaco X de Espacos e avalia se alguma posicao de 
%Esp pertence a X, se sim devolve X, caso contrario, devolve lista vazia.
comuns(X,[P|_],_,X) :-
    pertence(P,X),
    !.

comuns(_,[],Ac,Ac).

comuns(X,[P|R],Ac,Res) :-
    \+pertence(P,X),
    comuns(X,R,Ac,Res).


%controi iterativamente Esps_com juntando apenas aqueles com 
%posicoes comuns.
espacos_com_posicoes_comuns(Espacos,Esp,Esps_com) :-
    espacos_com_posicoes_comuns(Espacos,Esp,Esps_com,[]).

espacos_com_posicoes_comuns([],_,Ac,Ac).

espacos_com_posicoes_comuns([P|R],Esp,Esps_com,Ac) :-
    P \== Esp,
    comuns(P,Esp,[],Espaco),
    Espaco \== [],
    append(Ac,[Espaco],Junto),
    espacos_com_posicoes_comuns(R,Esp,Esps_com,Junto).

espacos_com_posicoes_comuns([P|R],Esp,Esps_com,Ac) :-
    P \== Esp,
    comuns(P,Esp,[],Espaco),
    Espaco == [],
    espacos_com_posicoes_comuns(R,Esp,Esps_com,Ac).

%se o espaco P for igual a Esp nao deve ser incluido.
espacos_com_posicoes_comuns([P|R],Esp,Esps_com,Ac) :-
    P == Esp,
    espacos_com_posicoes_comuns(R,Esp,Esps_com,Ac).

%------------------------------------------------------------------------------------
%3.1.6
%palavra_possivel_esp(Pal,Esp,Espacos,Letras) em que Pal eh uma lista de letras de
%uma palavra, Esp eh um espaco, Espacos eh uma lista de espacos e Letras eh uma lista
%de letras de palavras. Assim, Pal eh uma palavra possivel para o espaco Esp.
%------------------------------------------------------------------------------------

%funcao auxiliar que verifica se uma palavra Pal unifica com um espaco Esp.
respeita_letras([],[]).

respeita_letras([P|R],[X|Y]) :-
    unifiable(P,X,_),     
    respeita_letras(R,Y).

respeita_letras([P|R],[P|Y]) :-
    P \= X,
    %se n forem unificaveis entao quer dizer que o espaco esta preenchido por
    %uma letra pelo que as letras tem de ser iguais.
    P == X,   
    respeita_letras(R,Y).


%funcao auxiliar que avalia as duas condicoes necessarias para que uma palavra
%unifique com um espaco
ambas(Pal,Esp) :-
    respeita_letras(Pal,Esp),
    same_length(Pal,Esp).


%funcao auxiliar que verifica se todos os espacos de Espacos
%sao unificaveis com alguma palavra de Letras.
unificaveis(Espacos,Letras) :- unificaveis(Espacos,Letras,0).

unificaveis([],_,Flag) :- Flag == 1.

unificaveis([P|R],Letras,Flag) :-
    Flag == 0,
    verifica_espaco(P,Letras),
    unificaveis(R,Letras,1).

unificaveis([P|R],Letras,Flag) :-
    Flag == 1,
    verifica_espaco(P,Letras),
    unificaveis(R,Letras,1).

unificaveis([P|_],Letras,_) :-
    \+verifica_espaco(P,Letras),
    unificaveis([],_,0).

verifica_espaco([],_) :- !.

verifica_espaco(Esp,[X|_]) :-
    ambas(X,Esp),
    !,
    verifica_espaco([],_).

verifica_espaco(Esp,[X|Y]) :-
    \+ambas(X,Esp),
    verifica_espaco(Esp,Y).


palavra_possivel_esp(Pal,Esp,Espacos,Letras) :-
    member(Pal,Letras),
    ambas(Pal,Esp),
    espacos_com_posicoes_comuns(Espacos,Esp,Esp_com),
    Pal = Esp,
    unificaveis(Esp_com,Letras).


%------------------------------------------------------------------------------------
%3.1.7
%palavras_possiveis_esp(Letras,Espacos,Esp,Pals_Possiveis) em que Letras eh uma lista
%de letras de palavras, Espacos eh uma lista de espacos, Esp eh um espaco e significa
%que Pals_Possiveis eh a lista ordenada de palavras possiveis para o espaco Esp.
%------------------------------------------------------------------------------------

palavras_possiveis_esp(Letras,Espacos,Esp,Pals_Possiveis) :-
    findall(Pal,palavra_possivel_esp(Pal,Esp,Espacos,Letras),Pals_Possiveis).

%------------------------------------------------------------------------------------
%3.1.8
%palavras_possiveis(Letras, Espacos, Pals_Possiveis), em que Letras eh
%uma lista de listas de letras de palavras,Espacos eh uma lista de espacos e 
%significa que Pals_Possiveis eh a lista de palavras possiveis.
%------------------------------------------------------------------------------------

palavras_possiveis(Letras,Espacos,Pals_Possiveis) :- 
    palavras_possiveis(Letras,Espacos,Espacos,Pals_Possiveis,[]).

palavras_possiveis(_,_,[],Junto,Junto).

%controi iterativamente Pals_Possiveis juntando apenas os Espacos
%com palavras possiveis para os mesmos.
palavras_possiveis(Letras,Espacos,[P|R],Pals_Possiveis,Ac) :-
    palavras_possiveis_esp(Letras,Espacos,P,Pals),
    Pals \== [],
    append([P],[Pals],Junto_Temp),
    append(Ac,[Junto_Temp],Junto),
    palavras_possiveis(Letras,Espacos,R,Pals_Possiveis,Junto).

palavras_possiveis(Letras,Espacos,[P|R],Pals_Possiveis,Ac) :-
    palavras_possiveis_esp(Letras,Espacos,P,Pals),
    Pals == [],
    palavras_possiveis(Letras,Espacos,R,Pals_Possiveis,Ac).


%------------------------------------------------------------------------------------
%3.1.9
%letras_comuns(Lst_Pals, Letras_comuns), em que Lst_Pals eh uma lista de
%listas de letras, significa que Letras_comuns eh uma lista de pares (pos, letra),
%significando que todas as listas de Lst_Pals contem a letra na posicao pos.
%------------------------------------------------------------------------------------

%funcao auxiliar que avalia para todas as palavras de Lst_Pals se contem
%a letra Letra no Indice recebido.
letras_nas_restantes([],_,_,Flag) :- Flag == 1.

letras_nas_restantes([X|_],Letra,Indice,_) :-
    nth1(Indice_Temp,X,Letra),
    Indice_Temp \== Indice,
    letras_nas_restantes([],Letra,Indice,0).

letras_nas_restantes([X|Y],Letra,Indice,_) :-
    nth1(Indice_Temp,X,Letra),
    Indice_Temp == Indice,
    letras_nas_restantes(Y,Letra,Indice,1).


letras_comuns(Lst_Pals,Letras_comuns) :-
    nth1(1,Lst_Pals,Pal),
    length(Pal,Comp),
    letras_comuns(Lst_Pals,Lst_Pals,Letras_comuns,1,[],Comp,0).

%caso terminal quando o contador for igual ao comprimento registado
%da primeira palavra pois significa que nao ha mais letras por verificar.
letras_comuns(_,_,Ac,_,Ac,Comp,Comp).

letras_comuns([P|_],Lst_Pals,Letras_comuns,Indice,Ac,Comp,Cont) :-
    nth1(Indice,P,Letra),
    letras_nas_restantes(Lst_Pals,Letra,Indice,0),
    !,
    Indice_Temp is Indice+1,
    append(Ac,[(Indice,Letra)],Junto),
    New_Cont is Cont+1,
    letras_comuns(Lst_Pals,Lst_Pals,Letras_comuns,Indice_Temp,Junto,Comp,New_Cont).

letras_comuns([P|_],Lst_Pals,Letras_comuns,Indice,Ac,Comp,Cont) :-
    nth1(Indice,P,Letra),
    \+letras_nas_restantes(Lst_Pals,Letra,Indice,0),
    Indice_Temp is Indice+1,
    New_Cont is Cont+1,
    letras_comuns(Lst_Pals,Lst_Pals,Letras_comuns,Indice_Temp,Ac,Comp,New_Cont).


%------------------------------------------------------------------------------------
%3.1.10
%atribui_comuns(Pals_Possiveis) em que Pals_Possiveis eh uma lista de palavras
%possiveis, actualiza esta lista atribuindo a cada espaco as letras comuns a todas 
%as palavras possiveis para esse espaco
%------------------------------------------------------------------------------------

atribui_comuns([]).

atribui_comuns([P|R]) :-
    nth1(2,P,Pals),
    nth1(1,P,Esp),
    letras_comuns(Pals,Letras_comuns),
    unifica_espaco(Letras_comuns,Esp,_),
    atribui_comuns(R).

%funcao auxiliar que unifica um espaco com as letras de letras_comuns
%nos indices respetivos.
unifica_espaco([],Esp,Esp) :- !.

unifica_espaco([(Indice,Letra)|R], Esp, Esp_Temp) :-
    nth1(Indice,Esp,Letra),
    unifica_espaco(R,Esp,Esp_Temp).


%------------------------------------------------------------------------------------
%3.1.11
%retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) em que Pals_Possiveis 
%eh uma lista de palavras possiveis, significa que Novas_Pals_Possiveis eh o 
%resultado de tirar palavras impossiveis de Pals_Possiveis.
%------------------------------------------------------------------------------------

%funcao auxiliar que acumula apenas as palavras possiveis associdadas a um espaco.
verifica_palavra(Pals,Esp,Novas_Pals) :- verifica_palavra(Pals,Esp,[],Novas_Pals).

verifica_palavra([],_,Novas_Pals,Novas_Pals).

verifica_palavra([P|R],Esp,Ac,Novas_Pals) :-
    ambas(P,Esp),
    append(Ac,[P],Junto),
    verifica_palavra(R,Esp,Junto,Novas_Pals).

verifica_palavra([P|R],Esp,Ac,Novas_Pals) :-
    \+ambas(P,Esp),
    verifica_palavra(R,Esp,Ac,Novas_Pals).


retira_impossiveis(Pals_Possiveis,Novas_Pals_Possiveis) :-
    retira_impossiveis(Pals_Possiveis,Novas_Pals_Possiveis,[]).

retira_impossiveis([],Ac,Ac).

retira_impossiveis([P|R],Novas_Pals_Possiveis,Ac) :-
    nth1(1,P,Esp),
    nth1(2,P,Pals),
    verifica_palavra(Pals,Esp,Novas_Pals),
    %se Novas_Pals for igual a Pals quer dizer que as palavras de Pals 
    %se mantiveram todas possiveis para Esp.
    Novas_Pals == Pals,
    append(Ac,[P],New),
    retira_impossiveis(R,Novas_Pals_Possiveis,New).

retira_impossiveis([P|R],Novas_Pals_Possiveis,Ac) :-
    nth1(1,P,Esp),
    nth1(2,P,Pals),
    verifica_palavra(Pals,Esp,Novas_Pals),
    %se Novas_Pals eh diferente de Pals entao constroi-se uma nova
    %lista do tipo [Esp,Pals] apenas com as palavras que se permaneceram possiveis.
    Novas_Pals \== Pals,
    append([Esp],[Novas_Pals],Junto),
    append(Ac,[Junto],New),
    retira_impossiveis(R,Novas_Pals_Possiveis,New).


%------------------------------------------------------------------------------------
%3.1.12
%obtem_unicas(Pals_Possiveis, Unicas) em que Pals_Possiveis eh uma lista de palavras 
%possiveis, significa que Unicas eh a lista de palavras unicas de Pals_Possiveis.
%------------------------------------------------------------------------------------


obtem_unicas(Pals_Possiveis,Unicas) :- obtem_unicas(Pals_Possiveis,Unicas,[]).

obtem_unicas([],Ac,Ac).

%so adiciona ao acumulador se tiver comprimento 1.
obtem_unicas([P|R],Unicas,Ac) :-
    nth1(2,P,Pal),
    length(Pal,Comp),
    Comp == 1,
    append(Ac,Pal,Junto),
    obtem_unicas(R,Unicas,Junto).

obtem_unicas([P|R],Unicas,Ac) :-
    nth1(2,P,Pal),
    length(Pal,Comp),
    Comp \== 1,
    obtem_unicas(R,Unicas,Ac).

%------------------------------------------------------------------------------------

%3.1.13
%retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) em que Pals_Possiveis eh
% uma lista de palavras possiveis, significa que Novas_Pals_Possiveis eh o resultado
% de retirar de Pals_Possiveis as palavras unicas.
%------------------------------------------------------------------------------------


diferenca([],_,[]).

diferenca([P|R],L,[P|D]) :-
    \+pertence(P,L),
    !,
    diferenca(R,L,D).
diferenca([P|R],L,D) :-
    pertence(P,L),
    !,
    diferenca(R,L,D).

retira_unicas(Pals_Possiveis,Novas_Pals_Possiveis) :-
    obtem_unicas(Pals_Possiveis,Unicas),
    retira_unicas(Pals_Possiveis,Unicas,Novas_Pals_Possiveis,[]),
    !.

retira_unicas([],_,Novas_Palavras_Possiveis,Novas_Palavras_Possiveis).

retira_unicas([[Esp,Pals]|R],Unicas,Novas_Pals_Possiveis,Ac) :-
    length(Pals,Comp),
    Comp > 1,
    %Dif sera as palavras Pals associadas ao espaco Esp sem as unicas
    diferenca(Pals,Unicas,Dif),
    append(Ac,[[Esp,Dif]],Atualizado),
    retira_unicas(R,Unicas,Novas_Pals_Possiveis,Atualizado).

retira_unicas([[Esp,Pals]|R],Unicas,Novas_Pals_Possiveis,Ac) :-
    length(Pals,Comp),
    %a espacos com apenas uma palavra associada nao sofrem alteracao
    Comp =< 1,
    append(Ac,[[Esp,Pals]],Atualizado),
    retira_unicas(R,Unicas,Novas_Pals_Possiveis,Atualizado).

%------------------------------------------------------------------------------------

%3.1.14
%simplifica(Pals_Possiveis,Novas_Pals_possiveis) em que Pals Possiveis eh uma lista de
%palavras possiveis e significa que Novas_Pals_possiveis eh o resultado de simplificar 
%Pals_Possiveis ate nao haver mais alteracoes.
%------------------------------------------------------------------------------------

simplifica(Pals_Possiveis,Novas_Pals_possiveis) :-
    simplifica(Pals_Possiveis,Pals_Possiveis,Novas_Pals_possiveis,0),
    !.

%Temp vai ser a variavel de referencia de um ciclo anterior, isto e,
%se ao realizar um ciclo do predicado Temp e Novo_Temp forem iguais
%entao nao ha mais alteracoes.
simplifica(Temp,Temp,Temp,Cont) :-
    %num primeiro ciclo eles serao sempre iguais pelo que o contador 
    %tera de ser diferente de zero.
    Cont \== 0.

simplifica(_,Temp,Novas_Pals_possiveis,Cont) :-
    atribui_comuns(Temp),
    retira_impossiveis(Temp,Aux),
    retira_unicas(Aux,Novo_Temp),
    Cont_temp is Cont+1,
    simplifica(Temp,Novo_Temp,Novas_Pals_possiveis,Cont_temp).

%------------------------------------------------------------------------------------
%3.1.15
%inicializa(Puz,Novas_Pals_Possiveis) em que Puz eh um puzzle, significa que 
%Pals_Possiveis eh a lista de palavras possiveis simplificada para Puz.
%------------------------------------------------------------------------------------


inicializa(Puz,Novas_Pals_Possiveis) :-
    nth1(1,Puz,Palavras),
    nth1(2,Puz,Grelha),
    obtem_letras_palavras(Palavras,Letras),
    espacos_puzzle(Grelha,Espacos),
    palavras_possiveis(Letras,Espacos,Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis,Sem_Impossiveis),
    retira_unicas(Sem_Impossiveis,Sem_Unicas),
    simplifica(Sem_Unicas,Novas_Pals_Possiveis).

%------------------------------------------------------------------------------------
%3.2.1
%escolhe_menos_alternativas(Pals_Possiveis,Escolha) em que Escolha eh o elemento
%de Pals_Possiveis com o menor numero de palavras associadas ao seu espaco,
%exceptuando os espacos com apenas uma palavra associada
%------------------------------------------------------------------------------------


%funcao auxiliar que guarda o menor comprimento registado.
comp_min([P|R],Comps) :- comp_min([P|R],Comps,P).

comp_min([],P,P).
    
comp_min([P|R],Comps,Pal) :-
    length(P,Comp),
    length(Pal, Temp),
    Comp < Temp,
    comp_min(R,Comps,P).

comp_min([P|R],Comps,Pal) :-
    length(P,Comp),
    length(Pal, Temp),
    Comp >= Temp,
    comp_min(R,Comps,Pal).

%funcao auxiliar que identifica o primeiro espaco das Pals_Possiveis com 
%o comprimento Min.
obtem_espaco([P|_],Min,P) :-
    pertence(Min,P),
    !.

obtem_espaco([P|R],Min,Escolha):-
    \+pertence(Min,P),
    obtem_espaco(R,Min,Escolha).

%reune todo o conjunto de letras menos as das palavras unicas.
reune_letras(Pals_Possiveis,Letras) :-
    maplist(nth1(2),Pals_Possiveis,Todas),
    exclude(comp(1),Todas,Letras).

comp(_,Pal) :-
    length(Pal,1).

escolhe_menos_alternativas(Pals_Possiveis,Escolha) :-
    reune_letras(Pals_Possiveis,Palavras),
    %Palavras ja exclui as unicas.
    comp_min(Palavras,Min),
    obtem_espaco(Pals_Possiveis,Min,Escolha).

%------------------------------------------------------------------------------------
%3.2.2
%experimenta_pal(Escolha,Pals_Possiveis,Novas_Pals_Possiveis) em que Novas_Pals_Possiveis
%resulta de: 1) escolher uma das palavras associadas a Escolha (Pal), 2) unifica-la com o 
%espaco Esp de Escolha, 3) substituir Escolha pelo novo elemento [Esp,[Pal]].
%------------------------------------------------------------------------------------


experimenta_pal([Esp,Pals],Pals_Possiveis,Novas_Pals_Possiveis) :-
    member(Pal,Pals),
    Esp = Pal,
    percorre_possiveis([Esp,[Pal]],Pals_Possiveis,[],Novas_Pals_Possiveis).

%funcao auxiliar cria as Novas_Pals_Possiveis, acumula todas as Pals_Possiveis
%com atencao ah alteracao daquela cujo Esp se encontra igual ao da Escolha.
percorre_possiveis(_,[],Ac,Ac).

percorre_possiveis([Esp,Novo_Pals],[[Esps,_]|R],Ac,Novas_Pals_Possiveis) :-
    Esp == Esps,
    append(Ac,[[Esp,Novo_Pals]],Junto),
    percorre_possiveis([Esp,Novo_Pals],R,Junto,Novas_Pals_Possiveis).

percorre_possiveis([Esp,Novo_Pals],[[Esps,Pals]|R],Ac,Novas_Pals_Possiveis) :-
    Esp \== Esps,
    append(Ac,[[Esps,Pals]],Junto),
    percorre_possiveis([Esp,Novo_Pals],R,Junto,Novas_Pals_Possiveis).

%-----------------------------------------------------------------------------------
%3.2.3
%resolve_aux(Pals_Possiveis,Novas_Pals_Possiveis) em que Novas_Pals_Possiveis eh o 
%resultado de aplicar o algoritmo desenvolvido pelos predicados anteriores:
% escolhe_menos_alternativas;experimenta_pal e simplifica.
%-----------------------------------------------------------------------------------

resolve_aux(Pals_Possiveis,Novas_Pals_Possiveis) :-
    escolhe_menos_alternativas(Pals_Possiveis,Escolha),
    %operador de corte necessario caso a funcao de false.
    !,
    experimenta_pal(Escolha,Pals_Possiveis,Experimenta_Pal),
    simplifica(Experimenta_Pal,Simplificado),
    resolve_aux(Simplificado,Novas_Pals_Possiveis).

resolve_aux(Pals_Possiveis,Pals_Possiveis).

%-----------------------------------------------------------------------------------
%3.3.1
%resolve(Puz) em que apos a invocacao deste predicado a grelha de Puz
%tem todas as variaveis substituidas por letras que constituem as palavras 
%da lista de palavras de Puz.
%-----------------------------------------------------------------------------------

resolve(Puz) :-
    inicializa(Puz,Pals_Possiveis),
    resolve_aux(Pals_Possiveis,_).
