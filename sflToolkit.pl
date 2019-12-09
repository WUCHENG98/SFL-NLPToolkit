?-[grammarSyntaxTree].

%  This is a set of tools used in systemic functional linguistic

% ====================================================
% ======== TOOL 1: Nominal Density Counter ===========
% ====================================================

% counting the grammatical mataphorization of type 6.
countNominalDensity6(C,N):-
    ccomp(C,T),
    searchSTAll(premodifier,T,L),
    length(L,N).


% ====================================================
% ========= TOOL 2: Auto Grammatical Metaphor ========
% ====================================================

% take in a clause expressed like [A,B,C,...],
% use the syntax to decompose the clause into a node(clause,...)
% call clause2ng to return a noun group in the form [D,E,F,...]
clause2ng0(C,NG):-
    clause0(C,T),
    clause2ng(T,NG).

% Take in a node(clause,..) and returns the noun group [D,E,F,...]
clause2ng(node(clause,CL),NG):-
    [LAP1,N1,V,N2,LAP2] = CL,
    transverb(V,HN),
    translap(LAP1,LPre1),
    translap(LAP2,LPre2),
    append(LPre1,LPre2,LPreM),
    transnoun1(N1,LPost1),
    transnoun2(N2,LPost2),
    append(LPost2,LPost1,LPostM),
    appendAll([[the],LPreM,HN,LPostM],NG).

clause2ng(node(clause,CL),NG):-
    [LAP1,N1,V,LAP2] = CL,
    transverb(V,HN),
    translap(LAP1,LPre1),
    translap(LAP2,LPre2),
    append(LPre1,LPre2,LPreM),
    transnoun2(N1,LPost1),
    appendAll([[the],LPreM,HN,LPost1],NG).


% take in a clause-complex expressed like [A,B,C,...],
% use the syntax to decompose the clause into a node(clausecomplex,...)
% call ccomp2cl to return a clause in the form [D,E,F,...]
ccomp2cl0(CC,CL):-
    ccomp(CC,T),
    ccomp2cl(T,CL).

% Take in a node(clausecomplex,..) and returns the clause [D,E,F,...]
ccomp2cl(node(clausecomplex,[T0,node(conjunction,[CONJ])|[R]]),CL):-
    clause2ng(T0,NG1),
    clause2ng(R,NG2),
    class(CONJ,conjnorm),
    trans(CONJ,V,conj2v),
    appendAll([NG1,[V],NG2],CL).
    
ccomp2cl(node(clausecomplex,[T0,node(conjunction,[CONJ])|[R]]),CL):-
    clause2ng(T0,NG1),
    clause2ng(R,NG2),
    class(CONJ,conjrev),
    trans(CONJ,V,conj2v),
    appendAll([NG2,[V],NG1],CL).

% transform a verb in a node to its noun form
transverb(node(verbphrase,[_,node(mainverb,[V])]),[HN]):-
    trans(V,HN,verb2noun).

% transform an adverbial in a node to a premodifier
translap(node(adverbials,[]),[]).
translap(node(adverbials,[ADVP]),ADV):-
    transadv(ADVP,ADV).

% transform an adv in a node to its adj form
transadv(node(adverbphrase,[]),[]).
transadv(node(adverbphrase,[ADV|RV]),[ADJ|RJ]):-
    trans(ADV,ADJ,adv2adj),
    transadv(node(adverbphrase,RV),RJ).

% add "by" to a subject noun group in a node to form a postmodifier
transnoun1(node(noungroup,[_,_,HN,_]),LPost1):-
    transheadnoun(HN,N),
    append([by],N,LPost1).


% add "of" to a subject noun group in a node to form a postmodifier
transnoun2(node(noungroup,[_,_,HN,_]),LPost2):-
    transheadnoun(HN,N),
    append([of],N,LPost2).

% returns the noun from a headnoun node
transheadnoun(node(headnoun,[HN]),[N]):-
    N = HN.

% ====================================================
% ========= TOOL 3: Key Information Extractor ========
% ====================================================
% extract key information in the clause complexes 
infoClauseExtractor(C,TB):-
    ccomp(C,ST),
    searchSTAll(headnoun,ST,N),
    searchSTAll(mainverb,ST,V),
    TB = [[entiy,N],[process,V]].


