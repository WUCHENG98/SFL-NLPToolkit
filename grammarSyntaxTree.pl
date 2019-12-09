?- [dictionary].
% ======================================   
% ========= SYNTAX TREE ================
% ====================================== 

% This is a mutual reference tree
% either node(key,[nodes]) note: [nodes] would never be empty
% or node(key,[words]) note: that [] is used to represent a empty string as a leaf 
%                            and [word] is use to present a string as a leaf

% -----------------------------------------


% check whether a tree is a node or a leaf
isnode(node(_,_)).
isleaf(L):- \+ isnode(L).


% ======================================   
% ==============  NOUN =================
% ======================================

% define generalized noun: it is eihter a noungroup or a embeded clause.
noun(N,T):- ngroup(N,T).
noun(N,T):- emdclauseA(N,T).

% define list of nouns
nouns(NS,node(nouns,[H,node(conjunction,[and]),T])):-
    appendAll([N,[and],R],NS),
    noun(N,H),
    noun(R,T).
nouns(NS,node(nouns,[H,node(conjunction,[or]),T])):-
    appendAll([N,[or],R],NS),
    noun(N,H),
    noun(R,T).
nouns(NS,node(nouns,[H,node(punctuation,[","])|T])):-
    appendAll([N,[","],R],NS),
    noun(N,H),
    nouns(R,node(nouns,T)).

% define noun phrase: it is either a noun or a noun phrase
nounp(N,T):- noun(N,T).
nounp(N,T):- nouns(N,T).


% --------------------------------------------

% define noun group: determinat, premodifiers, a headnoun and postmodifiers
ngroup(NG,T):-
    appendAll([Det,LPreM,HN,LPostM],NG),
    det(Det,T1),
    premods(LPreM,T2),
    headn(HN,T3),
    postmods(LPostM,T4),
    T = node(noungroup,[T1,T2,T3,T4]).


% define the determiners
det([the],node(determiner,[the])).
det([a],node(determiner,[a])).
det([this],node(determiner,[this])).
det([that],node(determiner,[that])).
det([these],node(determiner,[these])).
det([those],node(determiner,[those])).
det([],node(determiner,[])).

% define the list of premodifiers
premods([],node(premodifiers,[])).
premods(LPreM,node(premodifiers,LT)):-
    append([T0],LT1,LT),
    append(PM,R,LPreM),
    premod(PM,T0),
    premods(R,node(premodifiers,LT1)).

% define a premodifier
premod([ADJ],node(premodifier,[ADJ])):-
    adjective(ADJ).

premod([ADV,ADJ],node(premodifier,[ADV,ADJ])):-
    adverb(ADV),
    adjective(ADJ).

premod([N],node(premodifier,[N])):- noun(N).

% define a head noun
headn([HN],node(headnoun,[HN])):- noun(HN).


% define the list of postmodifiers
postmods([],node(postmodifiers,[])).
postmods(LPostM,node(postmodifiers,LT)):-
    append([T0],LT1,LT),
    append(PM,R,LPostM),
    postmod(PM,T0),
    postmods(R,node(postmodifiers,LT1)).


% define the postmodifier
postmod([Prep|N],T):-
    prepsition(Prep),
    nounp(N,T0),
    T = node(postmodifier,[node(prepsition,[Prep]),T0]).

postmod([PastP,Prep|N],T):-
    form(_,PastP,pastparticiple),
    prepsition(Prep),
    nounp(N,T0),
    T = node(postmodifier,[node(pastparticiple,[PastP]),node(prepsition,[Prep]),T0]).

postmod(EC,T):-
    emdclauseB(EC,T0),
    T = node(postmodifier,[T0]).

% =====================================
% ========== VERB =====================
% =====================================

% define a verbphrase, composed by auxiliaries and a main verb
verbp(VP,T):-
    appendAll([LAux,[MV]],VP),
    auxs(LAux,T1),
    mainv([MV],T2),
    T = node(verbphrase,[T1,T2]).


auxs([],node(auxiliaries,[])).
auxs(LAux,node(auxiliaries,LT)):-
    append(Aux,R,LAux),
    append([T0],LT0,LT),
    aux(Aux,T0),
    auxs(R,node(auxiliaries,LT0)).

aux(Aux,T0):- auxiliary(Aux), T0 = node(auxiliary,Aux).

auxiliary([going,to]).
auxiliary([be]).
auxiliary([can]).
auxiliary([have]).
auxiliary([will]).
auxiliary([do]).
auxiliary([Aux]):- form(Aux0,Aux,pasttense),auxiliary([Aux0]).
auxiliary([Aux]):- form(Aux0,Aux,pastparticiple),auxiliary([Aux0]).
auxiliary([Aux]):- form(Aux0,Aux,presentparticiple),auxiliary([Aux0]).
auxiliary([Aux]):- form(Aux0,Aux,thirdpersonsing),auxiliary([Aux0]).
auxiliary([Aux,not]):- auxiliary([Aux]).


mainv([MV],node(mainverb,[MV])):- verb(MV).


% =====================================
% ========== PREP =====================
% =====================================
% prepresition phrase: prepresition + noun

prepp([Prep|N],T):-
    prepsition(Prep),
    nounp(N,T0),
    T = node(prepsitionphrase,[node(prepsition,[Prep]),T0]).

prepp(PP,T):-
    append(Prep,N,PP),
    prepsition(Prep),
    nounp(N,T0),
    T = node(prepsitionphrase,[node(prepsition,Prep),T0]).


% =====================================
% ========== ADVERB ===================
% =====================================

% define a adverb phrase

advp([ADV],node(adverbphrase,[ADV])):- adverb(ADV).
advp([very,ADV],node(adverbphrase,[very,ADV])):- adverb(ADV).
advp([quite,ADV],node(adverbphrase,[quite,ADV])):- adverb(ADV).


% =====================================
% ========== ADVERBIALs ===============
% =====================================

% define adverbial phrases

% --- combine prepp and advp ----------

% this is used as a helper

% a adverbial phrase
adverbialp(A,T):-prepp(A,T).
adverbialp(A,T):-advp(A,T).

% list of adverbial phrases
lap([],node(adverbials,[])).
lap(LAP,node(adverbials,[T|LT])):-
    append(AP,R,LAP),
    adverbialp(AP,T),
    lap(R,node(adverbials,LT)).

% =====================================
% ========== PREDICATIVE ==============
% =====================================

% a predicative: adv + adj
pred(P,T):-
    appendAll([LAP,[ADJ]],P),
    lap(LAP,T1),
    adjective(ADJ),
    T = node(predicative,[T1,node(adjective,[ADJ])]).


% a list of preidcatives
preds(PS,node(predicatives,[H,node(conjunction,[and]),T])):-
    appendAll([P,[and],R],PS),
    pred(P,H),
    pred(R,T).
preds(PS,node(predicatives,[H,node(conjunction,[or]),T])):-
    appendAll([P,[or],R],PS),
    pred(P,H),
    pred(R,T).
preds(PS,node(predicatives,[H,node(punctuation,[","])|T])):-
    appendAll([P,[","],R],PS),
    pred(P,H),
    preds(R,node(predicatives,T)).


predp(P,T):- pred(P,T).
predp(P,T):- preds(P,T).



% =====================================
% ========== CLAUSE ===================
% =====================================

% ------   real clause ---------------

% N + V + N
clause0(C,T):-
    appendAll([LAP1,N1,V,N2,LAP2],C),
    lap(LAP1,T1),
    nounp(N1,T2),
    verbp(V,T3),
    nounp(N2,T4),
    lap(LAP2,T5),
    T = node(clause,[T1,T2,T3,T4,T5]).

% N + V
clause0(C,T):-
    appendAll([LAP1,N1,V,LAP2],C),
    lap(LAP1,T1),
    nounp(N1,T2),
    verbp(V,T3),
    lap(LAP2,T4),
    T = node(clause,[T1,T2,T3,T4]).

% N + linkingverb + pred
clause0(C,T):-
    appendAll([LAP1,N1,LVP,PD,LAP2],C),
    lap(LAP1,T1),
    nounp(N1,T2),
    last(LVP,LV),
    linkingverb(LV),
    verbp(LVP,T3),
    predp(PD,T4),
    lap(LAP2,T5),
    T = node(clause,[T1,T2,T3,T4,T5]).

% to do
clause0([to,DO|NA],T):-
    appendAll([N,A],NA),
    class(DO,verb),
    nounp(N,T0),
    lap(A,T1),
    T = node(cluase,[node(infinitive,[to,DO]),T0,T1]).

% doing
clause0([DOING|NA],T):-
    form(_,DOING,presentparticiple),
    appendAll([N,A],NA),
    nounp(N,T0),
    lap(A,T1),
    T = node(cluase,[node(presentparticiple,[DOING]),T0,T1]).


% ------  embeded clause -------------

% there are two types of embded clause
emdclause(C,T):-emdclauseA(C,T).
emdclause(C,T):-emdclauseB(C,T).

% to do clause
emdclauseA([to,DO|NA],T):-
    appendAll([N,A],NA),
    class(DO,verb),
    nounp(N,T0),
    lap(A,T1),
    T = node(embededclause,[node(infinitive,[to,DO]),T0,T1]).

% doing clause
emdclauseA([DOING|NA],T):-
    form(_,DOING,presentparticiple),
    appendAll([N,A],NA),
    nounp(N,T0),
    lap(A,T1),
    T = node(embededclause,[node(presentparticiple,[DOING]),T0,T1]).

% that/which/when/where/why/how/ as noun
emdclauseA([CONJ|C],T):-
    class(CONJ,conjunction),
    clause0(C,T0),
    T = node(embededclause,[node(conjunction,[CONJ]),T0]).

% that/which/when/where/why/how/ as postmodifier
emdclauseB([CONJ|C],T):-
    class(CONJ,conjunction),
    appendAll([LAP1,N1,V,LAP2],C),
    lap(LAP1,T1),
    nounp(N1,T2),
    verbp(V,T3),
    lap(LAP2,T4),
    T = node(embededclause,[node(conjunction,[CONJ]),T1,T2,T3,T4]).

emdclauseB([CONJ|C],T):-
    class(CONJ,conjunction),
    appendAll([LAP1,V,N,LAP2],C),
    lap(LAP1,T1),
    verbp(V,T2),
    nounp(N,T3),
    lap(LAP2,T4),
    T = node(embededclause,[node(conjunction,[CONJ]),T1,T2,T3,T4]).


% =====================================
% ======= CLAUSE COMPLEX ==============
% =====================================

% clause complexe is a list of cluases connected by conjunction
ccomp(CC,T):-
    clause0(CC,T0),
    T = node(clausecomplex,[T0]).

ccomp(CC,node(clausecomplex,[T0,node(conjunction,[CONJ])|T])):-
    appendAll([C,[CONJ],R],CC),
    class(CONJ,conjunction),
    clause0(C,T0),
    ccomp(R,node(clausecomplex,T)).

ccomp(CC,node(clausecomplex,[T0,node(punctuation,[","])|T])):-
    appendAll([C,[","],R],CC),
    clause0(C,T0),
    ccomp(R,node(clausecomplex,T)).


% =====================================
% =========== SEARCHER ================
% =====================================

% ------------------------------
% v1
% given a key, find [leaf] or [node] under the first node = key

searchST(K,node(K0,LorB),V):-
    K = K0,
    V = LorB.


searchST(K,node(K0,LST),V):-
    dif(K,K0),
    searchLST(K,LST,V).

searchLST(K,[H|_],V):-
    searchST(K,H,V).

searchLST(K,[_|T],V):-
    searchLST(K,T,V).


% ------------------------------
% v2
% given a key, find [[leaf]] or [[node]] under the all the nodes to be that key 

searchSTAll(_,S,[]):- \+ isnode(S).


searchSTAll(K,node(K,LorB),[V|T]):-
    V = LorB,
    searchLSTAll(K,LorB,T).


searchSTAll(K,node(K0,LST),LV):-
    dif(K,K0),
    searchLSTAll(K,LST,LV).


searchLSTAll(_,[],[]).

searchLSTAll(K,[H|T],LV):-
    searchSTAll(K,H,L),
    searchLSTAll(K,T,R),
    append(L,R,LV).


% =====================================
% =========== RENDERER ================
% =====================================

% render a tree, we just need to change the format
% different level of syntax tree rendering could be called 

% -----------------------------

renderngroup(node(noungroup,[Det,Pre,HN,Post]),T):-
    renderdet(Det,T1),
    renderpremods(Pre,T2),
    renderheadn(HN,T3),
    renderpostmods(Post,T4),
    T = noungroup(T1,T2,T3,T4).

renderdet(node(determiner,[Det]),determiner(Det)).
renderdet(node(determiner,[]),determiner([])).

renderheadn(node(headnoun,[HN]),headnoun(HN)).

renderpremods(node(premodifiers,[]),premodifiers([])).
renderpremods(node(premodifiers,[H|R]),premodifiers([T|R1])):-
    renderpremods(node(premodifiers,R),premodifiers(R1)),
    renderpremod(H,T).

renderpremod(node(premodifier,[H]),premodifier(H)).
renderpremod(node(premodifier,[HV,HJ]),premodifier(HV,HJ)).

renderpostmods(node(postmodifiers,[]),postmodifiers([])).
renderpostmods(node(postmodifiers,[H|R]),postmodifiers([T|R1])):-
    renderpostmods(node(postmodifiers,R),postmodifiers(R1)),
    renderpostmod(H,T).

renderpostmod(node(postmodifier,[node(prepsition,[P]),T0]),postmodifier(prepsition(P),T)):- rendernounp(T0,T).
renderpostmod(node(postmodifier,[node(pastparticiple,[PastP]),node(prepsition,[P]),T0]),postmodifier(pastparticiple(PastP),prepsition(P),T)):- rendernounp(T0,T).
renderpostmod(node(postmodifier,[EC]),T):- emdclauseA(EC,T).

renderngrouptree(NG,T):- ngroup(NG,T0),renderngroup(T0,T).

% ---------------------------------------

rendernoun(N,T):- renderngroup(N,T).
rendernoun(N,T):- renderemdclauseA(N,T).

% ---------------------------------------

rendernouns(node(nouns,[L,node(conjunction,[and]),R]),nouns([L0,conjunction(and),R0])):-
    rendernoun(L,L0),
    rendernoun(R,R0).

rendernouns(node(nouns,[L,node(conjunction,[or]),R]),nouns([L0,conjunction(or),R0])):-
    rendernoun(L,L0),
    rendernoun(R,R0).

rendernouns(node(nouns,[L,node(punctuation,[","])|R]),nouns([L0,punctuation(",")|R0])):-
    rendernoun(L,L0),
    rendernouns(node(nouns,R),nouns(R0)).

% ---------------------------------------

rendernounp(N,T):- rendernoun(N,T).
rendernounp(N,T):- rendernouns(N,T).

% ---------------------------------------

renderverbp(node(verbphrase,[LAux,MV]),T):-
    renderauxs(LAux,T1),
    rendermainv(MV,T2),
    T = verbphrase(T1,T2).

renderauxs(node(auxiliaries,[]),auxiliaries([])).
renderauxs(node(auxiliaries,[H|R]),auxiliaries([T|R1])):-
    renderauxs(node(auxiliaries,R),auxiliaries(R1)),
    renderaux(H,T).

renderaux(node(auxiliary,[H]),auxiliary(H)).
renderaux(node(auxiliary,[A,B]),auxiliary(A,B)).

rendermainv(node(mainverb,[MV]),mainverb(MV)).

renderverbptree(VP,T):- verbp(VP,T0),renderverbp(T0,T).

% -----------------------------------

renderprepp(node(prepsitionphrase,[node(prepsition,[Prep]),NG]),T):-
    renderngroup(NG,T0),
    T = prepsitionphrase(prepsition(Prep),T0).

renderprepp(node(prepsitionphrase,[node(prepsition,Prep),NG]),T):-
    renderngroup(NG,T0),
    T = prepsitionphrase(prepsition(Prep),T0),
    length(Prep,X),
    X > 1.


renderprepptree(PP,T):-prepp(PP,T0),renderprepp(T0,T).

% -----------------------------------

renderpred(node(predicative,[LAP,node(adjective,[A])]),T):-
    renderlap(LAP,T1),
    T = predicative(T1,adjective(A)).

renderpreds(node(predicatives,[L,node(conjunction,[and]),R]),predivatives([L0,conjunction(and),R0])):-
    renderpred(L,L0),
    renderpred(R,R0).

renderpreds(node(predicatives,[L,node(conjunction,[or]),R]),predicatives([L0,conjunction(or),R0])):-
    renderpred(L,L0),
    renderpred(R,R0).

renderpreds(node(predicatives,[L,node(punctuation,[","])|R]),predicatives([L0,punctuation(",")|R0])):-
    renderpred(L,L0),
    renderpreds(node(nouns,R),nouns(R0)).


renderpredp(P,T):- renderpred(P,T).
renderpredp(P,T):- renderpreds(P,T).

renderpredptree(P,T):- predp(P,T0), renderpredp(T0,T).


% -----------------------------------

renderadvp(node(adverbphrase,[ADV]),adverbphrase(ADV)).
renderadvp(node(adverbphrase,[very,ADV]),adverbphrase(very,ADV)).
renderadvp(node(adverbphrase,[quite,ADV]),adverbphrase(quite,ADV)).

renderadvptree(ADVP,T):- advp(ADVP,T0),renderadvp(T0,T).

% ----------------------------------

renderlap(node(adverbials,[]),adverbials([])).

renderlap(node(adverbials,[H|R]),adverbials([A|LA])):-
    renderadvp(H,A),
    renderlap(node(adverbials,R),adverbials(LA)).

renderlap(node(adverbials,[H|R]),adverbials([A|LA])):-
    renderprepp(H,A),
    renderlap(node(adverbials,R),adverbials(LA)).

renderlaptree(LAP,T):- lap(LAP,T0),renderlap(T0,T).

% -----------------------------------

renderclause(node(clause,[LAP1,N1,V,N2,LAP2]),clause(T1,T2,T3,T4,T5)):-
    renderlap(LAP1,T1),
    rendernounp(N1,T2),
    renderverbp(V,T3),
    rendernounp(N2,T4),
    renderlap(LAP2,T5).


renderclause(node(clause,[LAP1,N1,V,LAP2]),clause(T1,T2,T3,T4)):-
    renderlap(LAP1,T1),
    rendernounp(N1,T2),
    renderverbp(V,T3),
    renderlap(LAP2,T4).

renderclause(node(clause,[LAP1,N1,LV,PD,LAP2]),clause(T1,T2,T3,T4,T5)):-
    renderlap(LAP1,T1),
    rendernounp(N1,T2),
    renderverbp(LV,T3),
    renderpredp(PD,T4),
    renderlap(LAP2,T5).

renderclause(node(cluase,[node(infinitive,[to,DO]),N,A]),T):-
    rendernounp(N,T1),
    renderlap(A,T0),
    T = clause(infinitive(to,DO),T1,T0).

renderclause(node(cluase,[node(presentparticiple,[DOING]),N,A]),T):-
    rendernounp(N,T1),
    renderlap(A,T0),
    T = clause(presentparticiple(DOING),T1,T0).


renderclausetree(C,T):- clause0(C,T0),renderclause(T0,T).



% -----------------------------------

renderemdclauseA(node(embededclause,[node(infinitive,[to,DO]),N,A]),T):-
    rendernounp(N,T1),
    renderlap(A,T0),
    T = embededclause(infinitive(to,DO),T1,T0).

renderemdclauseA(node(embededclause,[node(presentparticiple,[DOING]),N,A]),T):-
    rendernounp(N,T1),
    renderlap(A,T0),
    T = embededclause(presentparticiple(DOING),T1,T0).

renderemdclauseA(node(embededclause,[node(conjunction,[CONJ]),C]),T):-
    renderclause(C,T0),
    T = clause(conjunction(CONJ),T0).

    
renderemdclauseB(node(embededclause,[node(conjunction,[CONJ]),LAP1,N,V,LAP2]),T):-
    renderlap(LAP1,T1),
    rendernounp(N,T2),
    renderverbp(V,T3),
    renderlap(LAP2,T4),
    T = embededclause(conjunction(CONJ),T1,T2,T3,T4).

renderemdclauseB(node(embededclause,[node(conjunction,[CONJ]),LAP1,V,N,LAP2]),T):-
    renderlap(LAP1,T1),
    renderverbp(V,T2),
    rendernounp(N,T3),
    renderlap(LAP2,T4),
    T = embededclause(conjunction(CONJ),T1,T2,T3,T4).

renderemdclause(C,T):- renderemdclauseA(C,T).
renderemdclause(C,T):- renderemdclauseB(C,T).

renderemdclausetree(C,T):- emdclause(C,T0), renderemdclause(T0,T).



% -----------------------------

renderccomp(node(clausecomplex,[T0]),clausecomplex([T])):- renderclause(T0,T).

renderccomp(node(clausecomplex,[T0,node(punctuation,[","])|R]),clausecomplex([T1,punctuation(",")|H])):-
    renderclause(T0,T1),
    renderccomp(node(clausecomplex,R),clausecomplex(H)).

renderccomp(node(clausecomplex,[T0,node(conjunction,[CONJ])|R]),clausecomplex([T1,conjunction(CONJ)|H])):-
    renderclause(T0,T1),
    renderccomp(node(clausecomplex,R),clausecomplex(H)).

renderccomptree(CC,T):- ccomp(CC,T0), renderccomp(T0,T).


% ====================================
% ========== DATA and HELPER =========
% ====================================


% define class functions

% noun or pural noun
noun(N):- class(N,noun).
noun(NS):- class(N,noun),form(N,NS,pural).


% adjective with its comparative and superlative
adjective(ADJ):- class(ADJ,adj).
adjective(ADJC):- class(ADJ,adj),form(ADJ,ADJC,comparative).
adjective(ADJS):- class(ADJ,adj),form(ADJ,ADJS,superlative).


% adverb
adverb(ADV):- class(ADV,adv).

% prepsition
prepsition(PP):-class(PP,prep).

% a verb or its past participle/persent participle/third person signjular/linking verb
verb(V):- class(V,verb).
verb(V):- class(V0,verb),form(V0,V,pastparticiple).
verb(V):- class(V0,verb),form(V0,V,presentparticiple).
verb(V):- class(V0,verb),form(V0,V,thirdpersonsing).
verb(V):- linkingverb(V).

% a linking verb or its past participle/persent participle/third person signjular
linkingverb(V):- class(V,linkingverb).
linkingverb(V):- class(V0,linkingverb),form(V0,V,pastparticiple).
linkingverb(V):- class(V0,linkingverb),form(V0,V,presentparticiple).
linkingverb(V):- class(V0,linkingverb),form(V0,V,thirdpersonsing).


% define helper function appendAll to combine different part in the sentence
appendAll([],[]).
appendAll([H|T],X):-
    append(H,Y,X),
    appendAll(T,Y).


