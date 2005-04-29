zip([],[],[]).
zip([H|T],[H1|T1],[[H,H1]|T2]):- zip(T,T1,T2).

zip_s(_,[],[],[]).
zip_s(S,[H|T],[H1|T1],[Trm|T2]):-
	Trm =.. [S,H,H1],
	zip_s(S,T,T1,T2).

% when the second is list of lists
zip1([],[],[]).
zip1([H|T],[H1|T1],[[H|H1]|T2]):- zip1(T,T1,T2).

append_l(In,Out):- reverse(In,In1),append_l1(In1,[],Out).
append_l1([],P,P).
append_l1([H|T],Done,Res):-
	append(H,Done,Res1),
	append_l1(T,Res1,Res).

% equivalence classes under binary predicate P
% eqclasses(EquivalencePredicate, InputList, EqClasses)
eqclasses(_,[],[]).
eqclasses(P,[H|T],[E_h|O_t]):-
	eqcl1(P,H,[H|T],E_h,N_h),
	eqclasses(P,N_h,O_t).
	
% eqcl1(EquivalencePredicate, Member, InputList, Equivalent, NonEquivalent)
eqcl1(_,_,[],[],[]).
eqcl1(P,H1,[H2|I],[H2|O],R):-
	apply(P,[H1,H2]) -> eqcl1(P,H1,I,O,R),!.
eqcl1(P,H1,[H2|I],O,[H2|R]):-
	eqcl1(P,H1,I,O,R).

% insert into P-eqclasses (keeping copies)
% eqc_insert(EquivalencePredicate, Member, InputEqClasses, OutputEqClasses)
eqc_insert(_,M,[],[[M]]).
eqc_insert(P,M,[[M1|H]|T],[[M,M1|H]|T]):-
	apply(P,[M,M1]),!.
eqc_insert(P,M,[H|T],[H|T1]):-
	eqc_insert(P,M,T,T1).
       
% stolen from tptp2X.main
%----Runtime version of operators
declare_TPTP_operators:-
    op(100,fx,++),
    op(100,fx,--),
    op(100,fx,'$'),
    op(405,xfx,'='),
    op(405,xfx,'~='),
    op(410,fy,~),
    op(502,xfy,'|'),
    op(502,xfy,'~|'),
    op(503,xfy,&),
    op(503,xfy,~&),
    op(504,xfy,=>),
    op(504,xfy,<=),
    op(505,xfy,<=>),
    op(505,xfy,<~>),
%----! and ? are of higher precedence than : so !X:p(X) is :(!(X),p(X))
%----Otherwise !X:!Y:p(X,Y) cannot be parsed.
    op(400,fx,!),
    op(400,fx,?),
%----Need : stronger than + for equality and otter in tptp2X
%----Need : weaker than quantifiers for !X : ~p
    op(450,xfy,:),
%---- .. used for range in tptp2X. Needs to be stronger than :
    op(400,xfx,'..').

:- declare_TPTP_operators.
% explain tstp parsing
d2l(X,X):- atomic(X);var(X).
d2l([],[]).
d2l(X,Y):- X =.. Z, d2ls(Z,Y).
d2ls([],[]).
d2ls([H|T],[H1|T1]):- d2l(H,H1), d2ls(T,T1).

declare_mptp_predicates:-
 abolish(fof/4),
 abolish(fof/5),
 multifile(fof/4),
 multifile(fof/5).

%% collect nonvar symbols from term
collect_symbols_top(X,L):-
	collect_symbols(X,L1),!,
	flatten(L1,L2),
	sort(L2,L).
collect_symbols(X,[]):- var(X).
collect_symbols(X,[X]):- atomic(X).
collect_symbols(X1,[H1|T2]):-
	X1 =.. [H1|T1],
	maplist(collect_symbols,T1,T2).

union1([],In,In).
union1([H|T],In,Out):-
	union(H,In,R1),
	union1(T,R1,Out).
       
% return the list of quantified variables and conjunction of predicates
sort_transform_qlist([],[],$true).  % needed only for fraenkel
sort_transform_qlist([X:S],[X],S1):-
	sort_transform(sort(X,S),S1).
sort_transform_qlist([(X:S)|T],[X|Qvars1],S1 & Preds1):-
	sort_transform(sort(X,S),S1),
	sort_transform_qlist(T,Qvars1,Preds1).

% to forbid backtracking
sort_transform_top(X,Y):- sort_transform(X,Y), !.
% end of traversal
sort_transform(X,X):- atomic(X); var(X).
% todo: use sort_transform_qlist here for multiple quantifs, consider unsorted
% do sort relativization
sort_transform(! Svars : Y, ! Qvars : (Preds => Y1)):-
	sort_transform_qlist(Svars,Qvars,Preds),
	sort_transform(Y,Y1).
sort_transform(? Svars : Y, ? Qvars : (Preds & Y1)):-
	sort_transform_qlist(Svars,Qvars,Preds),
	sort_transform(Y,Y1).
% This clause is redundant now, sort trafo can be done only after 'all' removal
sort_transform(all(Svars,Trm,Frm),all(Qvars,Trm1,Preds & Frm1)):-	
	sort_transform_qlist(Svars,Qvars,Preds),
	sort_transform(Trm,Trm1),
	sort_transform(Frm,Frm1).
sort_transform(sort(X,Y1 & Y2),Z1 & Z2):-
	sort_transform(sort(X,Y1),Z1),
	sort_transform(sort(X,Y2),Z2). 
sort_transform(sort(X,~Y),~Z):-
	sort_transform(sort(X,Y),Z). 
sort_transform(sort(_,$true),$true).
sort_transform(sort(_,$false),$false).
sort_transform(sort(X,Y),Z):-
	Y =.. [F|Args],
	maplist(sort_transform,[X|Args],Args1),
	Z =.. [F|Args1].
% we should not get here
sort_transform(sort(_,_),_):- throw(sort).
% functor traversal
sort_transform(X1,X2):-
	X1 =.. [H1|T1],
	maplist(sort_transform,T1,T2),
	X2 =.. [H1|T2].

% First we replace fraenkels by placeholder variables which we remember, and collect
% the fraenkels (with their context) into a list corresponding to the variables.
% Then we find optimal 'skolem' definitions corresponding to the fraenkels,
% instantiate such functors for each frankel with its context, and
% put them into the formulas by unifying them with the placeholder variables.
% all_collect(+InTerm,-OutTerm,+Context=[(Var:Sort1)|RestV],
%             -Info=[[NewVar,Context,all(Svars1,Trm1,Frm1)]|RestI])


all_collect_qlist([],[],C,C,[]).
all_collect_qlist([(X:S)|T],[(X:S1)|T1],Context,NewContext,Info):-
	all_collect(S,S1,Context,Info_s),
	all_collect_qlist(T,T1,[(X:S1)|Context],NewContext,Info_t),
	append(Info_s,Info_t,Info).

all_collect_top(In,Out,Info):- all_collect(In,Out,[],Info),!.
% end of traversal
all_collect(X,X,_,[]):- atomic(X); var(X).
% todo: use all_collect_qlist here for multiple quantifs, consider unsorted
all_collect(! Svars : Y, ! Svars1 : Y1, Context, Info_r):-
	all_collect_qlist(Svars,Svars1,Context,NewContext,Info_s),
	all_collect(Y,Y1,NewContext,Info_y),
	append(Info_s,Info_y,Info_r).
all_collect(? Svars : Y, ? Svars1 : Y1, Context, Info_r):-
	all_collect_qlist(Svars,Svars1,Context,NewContext,Info_s),
	all_collect(Y,Y1,NewContext,Info_y),
	append(Info_s,Info_y,Info_r).
% fix context!!
all_collect(all(Svars,Trm,Frm),NewVar,Context,
	    [[NewVar,Context,all(Svars1,Trm1,Frm1)]|Info_r]):-
	all_collect_qlist(Svars,Svars1,Context,NewContext,Info),
	all_collect_l([Trm,Frm],[Trm1,Frm1],NewContext,Info_l),
	append(Info,Info_l,Info_r).

% this can only be used if contexts of arguments are independent!
all_collect(X1,X2,Context,Info):-
	X1 =.. [H1|T1],
	all_collect_l(T1,T2,Context,Info),
	X2 =.. [H1|T2].

% this can only be used if contexts of arguments are independent!
all_collect_l([],[],_,[]).
all_collect_l([H1|T1],[H2|T2],Context,Info_r):-
	all_collect(H1,H2,Context,Info),
	all_collect_l(T1,T2,Context,Info_l),
	append(Info,Info_l,Info_r).


% ##todo: this now assumes that length of Context is number of
%       preceding variables - fix for other quantification formats
% create freankel (skolem) definition:
% all([X1,X2:integer], plus(X1,X2), (X1 < X2)) generates:
% ![X]:( in(X,fraenkel_skolem_functor_1) <=>
%         ?[X1,X2:integer]:( X = plus(X1,X2) & X1 < X2))
% Take care with variables, context is shared with formulas,
% newvars are used as placeholders for other fraenkels.
% +Info=[[NewVar,Context,all(Svars1,Trm1,Frm1)]|RestI])
% +FrInfo, -NewFrInfo ... unordered list of
% lists of Fraenkel symbols starting with their arity, e.g.:
% [[0|FrSymsOfArity_0],[2|FrSymsOfArity_2],[1|FrSymsOfArity_1]],

split_svars(Vars,Sorts,Svars):- zip_s(':', Vars, Sorts, Svars).

new_fr_sym(Arity, FrInfo, NewFrInfo, NewSym):-
	( select([Arity|FrSyms], FrInfo, TmpInfo);
	    ( FrSyms = [], TmpInfo = FrInfo )),
	length(FrSyms, Nr),
	concat_atom([fraenkel,Arity,Nr], '_', NewSym),
	select([Arity,NewSym|FrSyms], NewFrInfo, TmpInfo), ! .

mk_fraenkel_def(Var, Context, all(Svars1,Trm1,Frm1), FrInfo, NewFrInfo, Def) :-
	split_svars(Vars, _, Context),
	length(Vars, Arity),
	new_fr_sym(Arity, FrInfo, NewFrInfo, NewSym),	
	FrTrm =.. [NewSym|Vars],
	InPred =.. [r2_hidden, X, FrTrm],
	ExFla = ( ? Svars1 : ( ( X = Trm1 ) & Frm1)),
	Def = ( ! [(X : $true)|Context] : ( InPred <=> ExFla ) ),
	Var = FrTrm.

mk_fraenkel_defs([], FrSyms, FrSyms, []).
mk_fraenkel_defs([Info|T], FrSyms, NewFrSyms, Defs):-
	mk_fraenkel_defs1(Info, FrSyms, FrSyms1, Defs1),
	mk_fraenkel_defs(T, FrSyms1, NewFrSyms, Defs2),
	append(Defs1, Defs2, Defs).

mk_fraenkel_defs1([], FrSyms, FrSyms, []).
mk_fraenkel_defs1([[V,C,Trm]|T], FrSyms, NewFrSyms, [D|Defs]):-
	mk_fraenkel_def(V, C, Trm, FrSyms, FrSyms1, D),
	mk_fraenkel_defs1(T, FrSyms1, NewFrSyms, Defs).

% loads def. theorems too
load_theorems:-
	expand_file_name("/home/urban/tmp-miz/mml/*.the2",K),
	consult(K).

load_clusters:-
	expand_file_name("/home/urban/tmp-miz/mml/*.dcl2",K),
	consult(K).

load_constructors:-
	expand_file_name("/home/urban/tmp-miz/mml/*.dco2",K),
	consult(K).

fraenkel_ths(S):-
	findall(A,(fof(A,theorem,D,_,_),collect_symbols_top(D,L),
		   member(all,L)),S),
	length(S,N),write(N).

fraenkel_info(S1):-
	fraenkel_ths(S),
	findall([Out,Info],(member(A,S),fof(A,theorem,In,_,_),
			 all_collect_top(In,Out,Info)),S1).

expanded_franks(Flas,D):-
	fraenkel_info(S),
	zip(Flas,Infos,S),
	mk_fraenkel_defs(Infos,[],NewFrSyms,D),
	length(D,N1), maplist(length,NewFrSyms,Lengths),
	sumlist(Lengths,N2),
	maplist(collect_symbols_top,Flas,L),
	union1(L,[],L2),length(L2,N3),write([N1,N2,N3]).

dotimes(_,0).
dotimes(X,N) :- N1 is N-1, (call(X);true), dotimes(X,N1).

