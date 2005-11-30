%%- -*-Mode: Prolog;-*--------------------------------------------------
%%
%% File  : utils.pl
%%
%% Author: Josef Urban
%%
%%  MPTP2 Prolog utilities, tested only with SWI Prolog 5.2 now.
%%  The top-level predicate is now mk_first100/0.
%%------------------------------------------------------------------------


%% set this to the location of Prolog files created from MML
%% ('pl' directory in the distro).
%mml_dir("/home/urban/miztmp/distro/pl/").
mml_dir("/big/urban/miztmp/mml3/tmp/").
mml_dir_atom(A):- mml_dir(S), string_to_atom(S,A).

%% switch to fail for debug
optimize_fraenkel:- fail.


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


%% succeed once or throw ecxeption
ensure(Goal, Exception):- Goal,!; throw(Exception).

%% equivalence classes under binary predicate P
%% eqclasses(EquivalencePredicate, InputList, EqClasses)
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
logic_syms([++,--,'$',~,'|','~|',&,~&,=>,<=,<=>,<~>,!,?,:,'..',sort,all,'.',[]]).

%% uncomment this for E prover versions earlier than 0.9
% portray(A = B):- format(' equal(~p,~p) ',[A,B]).
portray(~A):- write(' ~ ('), print(A), write(') ').
portray(A & B):- format(' (~p & ~p) ',[A,B]).
portray(A => B):- format(' (~p => ~p) ',[A,B]).
portray(A <=> B):- format(' (~p <=> ~p) ',[A,B]).
portray(A : B):- var(A), format(' ( ~p : ~p) ',[A,B]).
portray(! A : B):- format(' (! ~p : ~p) ',[A,B]).
portray(? A : B):- format(' (? ~p : ~p) ',[A,B]).
portray(A):- atom(A), constr_name(A,Name,Quote),
	((Quote==0, write(Name));
	    (Quote==1, write(''''),write(Name),write(''''))).
portray(A):- compound(A), A =.. [F|L], constr_name(F,Name,Quote),
	((Quote==0, write(Name));
	    (Quote==1, write(''''),write(Name),write(''''))),
	write('('), print_many(L), write(')').

print_many([X]):- print(X).
print_many([X|Y]):- print(X), write(','), print_many(Y). 

% explain tstp parsing
d2l(X,X):- atomic(X);var(X).
d2l([],[]).
d2l(X,Y):- X =.. Z, d2ls(Z,Y).
d2ls([],[]).
d2ls([H|T],[H1|T1]):- d2l(H,H1), d2ls(T,T1).

declare_mptp_predicates:-
 abolish(fof/4),
 abolish(fof/5),
 abolish(theory/2),
 abolish(constr_name/3),
 multifile(fof/4),
 multifile(fof/5),
 dynamic(fof/5),
 dynamic(constr_name/3),
 multifile(constr_name/3),
 multifile(theory/2),
 abolish(fof_name/2),
 abolish(fof_section/2),
 abolish(fof_level/2),
 abolish(fof_parentlevel/2),
 abolish(fof_cluster/3),
 abolish(fof_req/3),
 index(fof(1,1,0,1,1)),
 index(fof(1,1,0,1)).

%% collect subterms satisfying P with count, test by equality
collect_with_count_top(Pred,Term,OutList,OutCounts):-
	collect_with_count(Pred,Term,[],[],OutList,OutCounts),!.
collect_with_count(P,X,In,InC,Out,OutC):-
	(var(X);atomic(X)), !, collect_with_count1(P,X,In,InC,Out,OutC).
%% neglects the head functor!
collect_with_count(P,X,In,InC,Out,OutC):-
	collect_with_count1(P,X,In,InC,Out1,OutC1),
	X =.. [_|T1],
	collect_with_countl(P,T1,Out1,OutC1,Out,OutC).
collect_with_countl(_,[],In,InC,In,InC).
collect_with_countl(P,[H|T],In,InC,Out,OutC):-
	collect_with_count(P,H,In,InC,Out1,OutC1),
	collect_with_countl(P,T,Out1,OutC1,Out,OutC).

%% do the collecting
collect_with_count1(P,X,In,InC,Out,OutC):- apply(P,[X]),!,
	(enth1(N,In,X) ->
	    (Out = In, remove_at(C_X,InC,N,Tmp),
		succ(C_X,C1_X), insert_at(C1_X,Tmp,N,OutC));
	    (Out = [X|In], OutC = [1|InC])).
collect_with_count1(_,_,In,InC,In,InC).

%% exact select
eselect(A, [C|B], B):- A == C,!.
eselect(A, [B|C], [B|D]):- eselect(A, C, D).
%% exact nth1
enth1(A, B, C):- var(A), !, enth_gen(B, C, 1, A).
enth_gen([A|_], A1, C, C):- A == A1. 
enth_gen([_|B], C, D, E) :-
        succ(D, F),
        enth_gen(B, C, F, E).

%% remove K-th element (1-based)
% remove_at(X,L,K,R) :- X is the K'th element of the list L; R is the
%    list that remains when the K'th element is removed from L.
%    (element,list,integer,list) (?,?,+,?)
remove_at(X,[X|Xs],1,Xs).
remove_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1, 
   K1 is K - 1, remove_at(X,Xs,K1,Ys).

% Insert an element at a given position into a list (1-based)
% insert_at(X,L,K,R) :- X is inserted into the list L such that it
%    occupies position K. The result is the list R.
%    (element,list,integer,list) (?,?,+,?)
insert_at(X,L,K,R) :- remove_at(X,R,K,L).

% Split a list into two parts
% split(L,N,L1,L2) :- the list L1 contains the first N elements
%    of the list L, the list L2 contains the remaining elements.
%    (list,integer,list,list) (?,+,?,?)
split(L,0,[],L).
split([X|Xs],N,[X|Ys],Zs) :- N > 0, N1 is N - 1, split(Xs,N1,Ys,Zs).

%% mptp_func with args
mptp_func(X):- X =..[H|_],atom_chars(H,[F|_]),member(F,[k,g,u,'0','1','2','3','4','5','6','7','8','9']).
ground_mptp_func(X):- ground(X),mptp_func(X).
sch_symbol(X):- atom_chars(X,[F|_]),member(F,[f,p]).
mptp_local_const(X):- atom_chars(X,[c|_]).

%% collect ground counts into buk/3, stack failure otherwise
%% then print and run perl -e 'while(<>) { /.([0-9]+), (.*)./; $h{$2}+=$1;} foreach $k (sort {$h{$b} <=> $h{$a}} (keys %h)) {print "$h{$k}:$k\n";}'
%% on the result
get_ground_info:-
	fof(Ref,_,Fla,file(_,_), [mptp_info(_,_,theorem,_,_)|_]),
	collect_with_count_top(ground_mptp_func,Fla,Out,OutC),
	not(buk(Ref,_,_)),assert(buk(Ref,Out,OutC)),
	fail.
print_ground_info:-
	tell('00ground'),
	findall(e,(buk(_,B,C),zip(C,B,S),findall(d,(member(X,S),print(X),nl),_)),_),
	told.


%% collect nonvar symbols from term
collect_symbols_top(X,L):-
	collect_symbols(X,L1),!,
	flatten(L1,L2),
	sort(L2,L).
collect_symbols(X,[]):- var(X),!.
collect_symbols(X,[X]):- atomic(X),!.
collect_symbols(X1,T2):-
	X1 =.. [H1|T1],
	maplist(collect_symbols,T1,T3),
	flatten(T3,T4),
	sort([H1|T4],T2).

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
% GroundCopy is kept for exact comparisons envolving context
% all_collect(+InTerm,-OutTerm,+Context=[(Var:Sort1)|RestV],
%             -Info=[[NewVar,Context,all(Svars1,Trm1,Frm1),GroundCopy]|RestI])

all_collect_qlist([],[],C,C,[]).
all_collect_qlist([(X:S)|T],[(X:S1)|T1],Context,NewContext,Info):-
	all_collect(S,S1,Context,Info_s),
	append(Context,[(X:S1)],C1),
	all_collect_qlist(T,T1,C1,NewContext,Info_t),
	append(Info_s,Info_t,Info).

fr_vars(FrInfo, FrVars):- maplist(nth1(1), FrInfo, FrVars).
split_svars(Vars,Sorts,Svars):- zip_s(':', Vars, Sorts, Svars).

all_collect_top(In,Out,Info):- all_collect(In,Out,[],Info),!.
% end of traversal
all_collect(X,X,_,[]):- atomic(X); var(X).

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
	    [[NewVar,RContext,all(Svars1,Trm1,Frm1),GroundCopy]|Info_r]):-
	(optimize_fraenkel ->
	    (free_variables(all(Svars,Trm,Frm), FreeVars),
		real_context(FreeVars, Context, RContext));
	    RContext = Context),
	copy_term([RContext,all(Svars,Trm,Frm)],GroundCopy),
	numbervars(GroundCopy,0,_),
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

%% only select context necessary for InVars from BigC
%% done by copy & number to avoid freevars
is_numvar(X):- ground(X), X=..['$VAR'|_].


%% add variables recursively needed for New (i.e. in their sorts)
%% AllVars and AllSorts must be aligned
add_real_numvars(_,_,Old,[],Old).
add_real_numvars(AllVars,AllSorts,Old,New,Result):-
	append(Old,New,Old1),
	findall(S, (member(V,New),nth1(N,AllVars,V),nth1(N,AllSorts,S)), Srts0),
	collect_with_count_top(is_numvar,Srts0,New0,_),
	intersection(AllVars,New0,New1),
	subtract(New1,Old1,New2),
	add_real_numvars(AllVars,AllSorts,Old1,New2,Result).

%% real_context(+InVars, +BigC, -RealC)
%% puts the "really needed" variables from BigContext into RealContext
%% BigC and RealC are lists of Var:Sort
%% bagof needed in the end instead of findall! (not to spoil freevars);
%% hence the alternative - bagof fails with empty list unlike findall
%% N1^ is needed for bagof not to bind N1
real_context(InVars, BigC, RealC):-
	split_svars(Vars,Sorts,BigC),
	copy_term([InVars,Vars,Sorts],[InVars1,Vars1,Sorts1]),
	numbervars([InVars1,Vars1,Sorts1],0,_),
	intersection(Vars1,InVars1,Added),
	add_real_numvars(Vars1,Sorts1,[],Added,AllAdded),
	(AllAdded = [] -> RealC = [];
	    (findall(N, (member(V,AllAdded),nth1(N,Vars1,V)), Nrs),
		sort(Nrs,Nrs1),
		bagof(C, (N1^member(N1,Nrs1),nth1(N1,BigC,C)), RealC))).


% ##todo: this now assumes that length of Context is number of
%       preceding variables - fix for other quantification formats
% create freankel (skolem) definition:
% all([X1,X2:integer], plus(X1,X2), (X1 < X2)) generates:
% ![X]:( in(X,fraenkel_skolem_functor_1) <=>
%         ?[X1,X2:integer]:( X = plus(X1,X2) & X1 < X2))
% Take care with variables, context is shared with formulas,
% newvars are used as placeholders for other fraenkels.
% +Info=[[NewVar,Context,all(Svars1,Trm1,Frm1),GroundCopy]|RestI])
% where GroundCopy is cretaed in all_collect by following:
% copy_term([RContext,all(Svars,Trm,Frm)],GroundCopy),
% numbervars(GroundCopy,0,_),
% which means that two fraenkels with the same GroundCopy are 
% the same (because the RContext is complete, and all symbol names
% are absolute inside one article)
% +FrInfo, -NewFrInfo ... unordered list of
% lists of Fraenkel symbols starting with their arity, e.g.:
% [[0|FrSymsOfArity_0],[2|FrSymsOfArity_2],[1|FrSymsOfArity_1]],

new_fr_sym(Arity, FrInfo, NewFrInfo, NewSym):-
	( select([Arity|FrSyms], FrInfo, TmpInfo);
	    ( FrSyms = [], TmpInfo = FrInfo )),
	length(FrSyms, Nr),
	concat_atom([all,Arity,Nr], '_', NewSym),
	select([Arity,NewSym|FrSyms], NewFrInfo, TmpInfo), ! .

mk_fraenkel_def(Var, Context, all(Svars1,Trm1,Frm1), FrInfo, NewFrInfo, NewSym, Def) :-
	split_svars(Vars, _, Context),
	length(Vars, Arity),
	new_fr_sym(Arity, FrInfo, NewFrInfo, NewSym),	
	FrTrm =.. [NewSym|Vars],
	InPred =.. [r2_hidden, X, FrTrm],
	ExFla = ( ? Svars1 : ( ( X = Trm1 ) & Frm1)),
	Def = ( ! [(X : $true)|Context] : ( InPred <=> ExFla ) ),
	Var = FrTrm.

mk_fraenkel_defs_top(Infos, NewFrSyms, NewDefs):-
	mk_fraenkel_defs(Infos, [], [], NewFrSyms, NewDefs),!.

mk_fraenkel_defs([], _, FrSyms, FrSyms, []).
mk_fraenkel_defs([[V,C,_,GrC]|T], GrCopies, FrSyms, NewFrSyms, Defs):-
	member([FoundSym,GrC], GrCopies), !,
	split_svars(Vars, _, C),
	V =.. [FoundSym|Vars],
	mk_fraenkel_defs(T, GrCopies, FrSyms, NewFrSyms, Defs).

mk_fraenkel_defs([[V,C,Trm,GrC]|T], GrCopies, FrSyms, NewFrSyms, [D|Defs]):-
	mk_fraenkel_def(V, C, Trm, FrSyms, FrSyms1, NewSym, D),
	mk_fraenkel_defs(T, [[NewSym,GrC]|GrCopies], FrSyms1, NewFrSyms, Defs).

% should be unique for Ref
get_ref_fla(Ref,Fla):- fof_name(Ref,Id),clause(fof(Ref,_,Fla,_,_),_,Id),!.
get_ref_fof(Ref,fof(Ref,R1,R2,R3,R4)):-
	fof_name(Ref,Id),
	clause(fof(Ref,R1,R2,R3,R4),_,Id),!.

% not unique for Sec and Info
get_sec_info_refs(RefsIn, Secs, Info, NewRefs):- !,
	findall(Ref1, (member(Sec1,Secs), fof_section(Sec1,Id),
			  clause(fof(Ref1,_,_,file(_,Sec1), Info),_,Id)), Refs1),
	subtract(Refs1, RefsIn, NewRefs).
%% this was very slow for many clauses, even with maximum prolog indexing
%% so the previous clause is used instead with hommade indexing
get_sec_info_refs(RefsIn, Secs, Info, NewRefs):-
	findall(Ref1, (member(Sec1,Secs), fof(Ref1,_,_,file(_,Sec1), Info)), Refs1),
	subtract(Refs1, RefsIn, NewRefs).

%% add properties for SymsIn
get_properties(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,_,_,[property(_)])|_], AddedRefs).

get_existence(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,_,_,[existence])|_], AddedRefs).

get_redefinitions(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,_,_,[redefinition(_,_,_,_)])|_],
			  AddedRefs).

get_types(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,_,_,[ctype])|_], Refs1),
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,constant,_,[_,type])|_], Refs2),
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,functor,_,[scheme,type])|_], Refs3),
	flatten([Refs1, Refs2, Refs3], AddedRefs).

get_equalities(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,constant,_,[_,equality])], AddedRefs).

%% version for mizar_by and mizar_proof; mizar_proof should be
%% enhanced a bit probably
%% OldSyms are used only for clusters and requirements
one_pass(F,InfKind,RefsIn,OldSyms,NewSyms,AddedRefs):-
	member(InfKind,[mizar_by,mizar_proof]),
	theory(F, Theory),
	member(registrations(Regs),Theory),
	member(requirements(Reqs),Theory),
	get_properties(RefsIn,NewSyms,Refs0),
	get_existence(RefsIn,NewSyms,Refs1),
	get_redefinitions(RefsIn,NewSyms,Refs2),
	get_types(RefsIn,NewSyms,Refs3),
	get_equalities(RefsIn,NewSyms,Refs4),
%% Refs5=[],
	get_clusters([F|Regs],RefsIn,OldSyms,NewSyms,Refs5),
	get_requirements(Reqs,RefsIn,OldSyms,NewSyms,Refs6),
	flatten([Refs0,Refs1,Refs2,Refs3,Refs4,Refs5,Refs6], AddedRefs).

%% version for mizar_from
%% OldSyms are used only for clusters and requirements
one_pass(F,mizar_from,RefsIn,OldSyms,NewSyms,AddedRefs):-
	theory(F, Theory),
	member(registrations(Regs),Theory),
	get_properties(RefsIn,NewSyms,Refs0),
	get_redefinitions(RefsIn,NewSyms,Refs2),
	get_types(RefsIn,NewSyms,Refs3),
	get_clusters([F|Regs],RefsIn,OldSyms,NewSyms,Refs5),
	flatten([Refs0,Refs2,Refs3,Refs5], AddedRefs).



%% add symbol references until nothing added
fixpoint(F,InfKind,RefsIn,OldSyms,NewSyms,RefsOut):-
	one_pass(F, InfKind, RefsIn, OldSyms, NewSyms, Refs1), !,
	(Refs1 = [] -> RefsOut = RefsIn;	    
	    union(Refs1, RefsIn, Refs2),
	    union(OldSyms, NewSyms, OldSyms1),
	    maplist(get_ref_fla, Refs1, Flas1),
	    collect_symbols_top(Flas1, Syms1),
	    subtract(Syms1, OldSyms1, NewSyms1), 
	    fixpoint(F, InfKind, Refs2, OldSyms1, NewSyms1, RefsOut)).


%% antecedent symbols needed for fcluster or ccluster
cl_needed_syms_top(Fla,Syms):-
	cl_needed_syms(Fla,Syms1),
	logic_syms(Syms2),
	subtract(Syms1, Syms2, Syms), !.
cl_needed_syms( ! Svars : Fla, AnteSyms):- !,
	collect_symbols_top(Svars, Syms1),
	cl_needed_syms(Fla, Syms2),
	union(Syms1, Syms2, AnteSyms).
cl_needed_syms( sort(_,Ante) => sort(_,_), AnteSyms):- !, collect_symbols_top(Ante, AnteSyms).
cl_needed_syms( sort(Trm,_), AnteSyms):- !, collect_symbols_top(Trm, AnteSyms).
%% should not get here
cl_needed_syms(_,_):- throw(cluster).


%% fof_cluster contains precomputed info
get_clusters(Files,RefsIn,OldSyms,NewSyms,AddedRefs):-
	union(OldSyms, NewSyms, AllSyms),
	findall(Ref1, (member(F1,Files),
			  fof_cluster(F1,Ref1,AnteSyms),
			  not(member(Ref1, RefsIn)),
			  subset(AnteSyms, AllSyms)),
		AddedRefs).

%%
get_requirements(Files,RefsIn,OldSyms,NewSyms,AddedRefs):-
	union(OldSyms, NewSyms, AllSyms),
	findall(Ref1, (member(F1,Files),
			  fof_req(F1,Ref1,Syms),
			  not(member(Ref1, RefsIn)),
			  subset(Syms, AllSyms)),
		AddedRefs).

nr_boole(0,fof(spc0_boole,theorem, v1_xboole_0(0),
	       file(boole,spc0_boole),
	       [mptp_info(0,[],theorem,position(0,0),[0])])):- !.
nr_boole(N,Res):-
	integer(N), N > 0, concat_atom([spc,N,'_boole'],Name),
	Res= fof(Name,theorem, ~ (v1_xboole_0(N)),
		 file(boole,Name),[mptp_info(0,[],theorem,position(0,0),[0])]).

nr_numerals(N,Res):-
	integer(N), N > 0, concat_atom([spc,N,'_numerals'],Name),
	Res= fof(Name,theorem,
		 sort(N,(v2_xreal_0 & m1_subset_1(k5_numbers))),
		 file(numerals,Name),
		 [mptp_info(0,[],theorem,position(0,0),[0])]).

%% other requirements:
%% - 2 distinct numerals are not equal
%% - functor rqImaginaryUnit (k1_xcmplx_0) equals to i (what is i? - I'll ignore
%%   "complex numerals" in the first version probably
%% - +,*,-/1, 1/x, -/2,/ evaluation (rqRealAdd, rqRealMult, rqRealNeg, rqRealInv,
%%                 rqRealDiff, rqRealDiv -
%%         - k2_xcmplx_0, k3_xcmplx_0, k4_xcmplx_0, k5_xcmplx_0, k6_xcmplx_0, k7_xcmplx_0)
%%   special cases for 0 and 1 mentioned in arithm.miz


first100([
	  xboole_0,boole,xboole_1,enumset1,zfmisc_1,subset_1,subset,relat_1,
	  funct_1,grfunc_1,relat_2,ordinal1,wellord1,setfam_1,relset_1,partfun1,
	  mcart_1,wellord2,funct_2,funct_3,domain_1,binop_1,funcop_1,funct_4,
	  ordinal2,ordinal3,arytm_3,arytm_2,arytm_1,finset_1,finsub_1,setwiseo,
	  fraenkel,
	  numbers,arytm_0,numerals,xcmplx_0,arithm,xreal_0,real,xcmplx_1,
	  xreal_1,axioms,real_1,square_1,nat_1,int_1,rat_1,binop_2,membered,
	  complex1,absvalue,card_1,finseq_1,zf_lang,zf_model,zf_colla,orders_1,
	  eqrel_1,funct_5,card_2,trees_1,finseq_2,recdef_1,classes1,card_3,
	  classes2,ordinal4,finseq_3,zfmodel1,zf_lang1,zf_refle,zfrefle1,qc_lang1,
	  qc_lang2,qc_lang3,cqc_lang,pboole,seq_1,seq_2,prob_1,wellset1,seqm_3,
	  seq_4,real_2,margrel1,prob_2,rcomp_1,multop_1,mcart_2,mcart_3,mcart_4,
	  mcart_5,mcart_6,finseq_4,finseqop,finsop_1,setwop_2]).


all_articles(List):-
	mml_dir(Dir),
	sformat(AList, '~s../mml.lar', [Dir]),
	open(AList,read,S),
	read_lines(S,List),
	close(S).

read_lines(S,Res):-
	read_line_to_codes(S,C),
	(C= end_of_file,Res=[];
	    read_lines(S,Res1),
	    string_to_atom(C,C1),
	    Res = [C1|Res1]).

%% Top level predicate for creating 'by' problems for
%% first 100 MML articles.
mk_first100:-
	declare_mptp_predicates,load_mml,first100(L),!,
	member(A,L),mk_article_problems(A,[[mizar_from],[theorem,sublemma,top_level_lemma]]),fail.

test_refs_first100:-
	declare_mptp_predicates,first100(L),!,
	member(A,L),test_refs(A),fail.


%% print defs and thms, with only top-level references 
print_thms_and_defs_for_learning:-
	declare_mptp_predicates,
	load_theorems,
	install_index,
	tell('Proof_learning'),
	%% print definitions - they have no proof
	((fof(Name,definition,A1,A2,A3),
	  numbervars(A1,0,_),
	  print(fof(Name,definition,A1,A2,A3)),
	  write('.'),nl,
	  fail);
	    true),
	%% print theorems, with only thm and def references
	((fof(Name,theorem,A1,A2,[A3,inference(A4,A5,Refs)]),
	  findall(Ref,(member(Ref,Refs),atom_chars(Ref,[C|_]),
		       member(C,[t,d])), Refs1),
	  numbervars(A1,0,_),
	  print(fof(Name,theorem,A1,A2,[A3,inference(A4,A5,Refs1)])),
	  write('.'),nl,
	  fail);
	    told).


%%%%%%%%%%%%%%% generating scheme instances  %%%%%%%%%%%%%%%%%%%%%%%

%% find the corresponding subst for X, throw exception if not
%% X is assumed to be a scheme functor or predicate
get_sym_subst(X,[],_):- !, throw(sch_subst(X)).
get_sym_subst(X,[Z/Y|_],Subst):- X == Z, !, copy_term(X/Y, Subst).
get_sym_subst(X,[_|T],Subst):- get_sym_subst(X,T,Subst).

%% apply one substitution to Sym
%% Sym is assumed to be a scheme functor or predicate
apply_sch_subst0(Sym,Sym/([]:Val),Val):- !.
apply_sch_subst0(Sym,Sym/([H|T]:Val),Val):- !,throw(sch_subst(Sym,[H|T]:Val)).
apply_sch_subst0(Sym,Sym/Val,Val):-!.
apply_sch_subst0(Sym,Subst,_):- throw(sch_subst(Sym,Subst)).

apply_sch_subst1([Sym|Args],Sym/(Vars:Val),Val):- !,
	(Vars = Args,!; throw(sch_subst(Sym,Vars:Val))).

apply_sch_subst1([Sym|Args],Sym/Val,Val1):- Val1 =.. [Val|Args],!.
apply_sch_subst1(Sym,Subst,_):- throw(sch_subst(Sym,Subst)).


%% apply_sch_substs(+Substs,+Fla,-Fla1)
%% applies scheme substitutions to Fla, throwing exception
%% if no substitution for any scheme symbol does not exist

apply_sch_substs_top(Substs,Fla,Fla1):-
	apply_sch_substs(Substs,Fla,Fla1), !.

%% end of traversal
apply_sch_substs(_,X,X):- var(X),!.
apply_sch_substs(Substs,X1,X2):- atomic(X1), sch_symbol(X1), !,
	get_sym_subst(X1,Substs,Subst),
	apply_sch_subst0(X1,Subst,X2).

apply_sch_substs(_,X,X):- atomic(X),!.	

apply_sch_substs(Substs,X1,X2):- X1 =.. [H1|T1], sch_symbol(H1), !,
	maplist(apply_sch_substs(Substs),T1,T2),
	get_sym_subst(H1,Substs,Subst),
	apply_sch_subst1([H1|T2],Subst,X2).
	
apply_sch_substs(Substs,X1,X2):- 
	X1 =.. [H1|T1],
	maplist(apply_sch_substs(Substs),T1,T2),
	X2 =.. [H1|T2].


%% gen_sch_instance(?SchInstName,?File,-Res)
%% - generate instance of SchName as fof with the same level
gen_sch_instance(SI_Name,F,Res):-
	fof(Ref,_,_,file(F,_),
	    [MPTPInfo,inference(mizar_from,[InstInfo],_Refs)]),
	InstInfo = scheme_instance(SI_Name,S_Name,Ref,_,Substs),
	MPTPInfo= mptp_info(_Nr,Lev,_Kind,Pos,_Args),
	once(fof(S_Name,theorem,Fla,_,_)),
	copy_term(Fla,Tmp),
	apply_sch_substs_top(Substs,Tmp,Fla1),
	Res = fof(SI_Name,theorem, Fla1, file(F,SI_Name),
		  [mptp_info(0,Lev,scheme_instance,Pos,[]),
		   inference(mizar_sch_instance,[InstInfo],[S_Name])]).

%% create and assert all scheme instances for a given article
assert_sch_instances(File):-
	repeat,
	( gen_sch_instance(_,File,Res), assert(Res), fail; true).
	

%%%%%%%%%%%% Constant generalization (abstraction) %%%%%%%%%%%%%%%%%%%%

%% add_const_vars(+RefsIn, +ConstsIn, +AddedConsts, -RefsOut, -ConstsOut)
%% collect all constants together with their type definitions
add_consts(RefsIn, ConstsIn, AddedConsts, RefsOut, ConstsOut):-
	get_sec_info_refs(RefsIn, AddedConsts,
			  [mptp_info(_,_,constant,_,[_,type])|_], NewRefs),
	([] = NewRefs ->
	    (RefsOut = RefsIn, ConstsOut = ConstsIn)
	;
	    maplist(get_ref_fla, NewRefs, Sorts0),
	    collect_symbols_top(Sorts0, Syms0),
	    sublist(mptp_local_const, Syms0, Consts0),
	    subtract( Consts0, ConstsIn, Consts1),
	    union(ConstsIn, Consts1, AllConsts1),
	    union(RefsIn, NewRefs, AllRefs1),
	    add_consts(AllRefs1, AllConsts1, Consts1, RefsOut, ConstsOut)
	).

%% this ensures that the infos are comparable and different
cmp_const_info(Res, fof(_,_,_,_,[mptp_info(Nr1,Lev1,_,_,_)|_]),
	       fof(_,_,_,_,[mptp_info(Nr2,Lev2,_,_,_)|_])):- !,
	(Lev1 = Lev2 ->
	    (compare(Res,Nr1,Nr2),
		ensure(Nr1 \= Nr2, cmp_const_info(Nr1,Lev1)))
	;
	    (sublevel(Lev1, Lev2) ->
		Res = '>'
	    ;
		(ensure(sublevel(Lev2, Lev1), cmp_const_info(Lev1,Lev2)),
		    Res = '<'))).

%% must not be reached
cmp_const_info(_,I1,I2):- ensure( fail, cmp_const_info(I1,I2)).

insrt_by_info(X, [], [X]).
insrt_by_info(F1, [F2|T], Sorted):-
	cmp_const_info(Res, F1, F2),
	(Res = '<' ->
	    Sorted = [F1,F2|T]
	;
	    (insrt_by_info(F1, T, Sorted1),
		Sorted = [F2|Sorted1])
	).

%% this is an insert sort by mptp_info
%% all consts must be comparable
sort_by_const_names([], Sorted, Sorted).
sort_by_const_names([H|T], Sorted, Res):-
	insrt_by_info(H, Sorted, Sorted1),
	sort_by_const_names(T, Sorted1, Res).
	


%% find the corresponding subst for X, throw exception if not
%% X is assumed to be a local constant
get_const_subst(X,[],_):- !, throw(const_subst(X)).
get_const_subst(X,[Z/Y|_],Z/Y):- X == Z, !.
get_const_subst(X,[_|T],Subst):- get_const_subst(X,T,Subst).

%% apply_const_substs(+Substs,+Fla,-Fla1)
%% applies local constant substitutions to Fla, throwing exception
%% if no substitution for any local const exists
apply_const_substs_top(Substs,Fla,Fla1):-
	apply_const_substs(Substs,Fla,Fla1), !.

%% end of traversal
apply_const_substs(_,X,X):- var(X),!.

apply_const_substs(Substs,X1,Var1):- atomic(X1), mptp_local_const(X1), !,
	get_const_subst(X1,Substs,X1/Var1).

apply_const_substs(_,X,X):- atomic(X),!.	
apply_const_substs(Substs,X1,X2):- 
	X1 =.. [H1|T1],
	maplist(apply_const_substs(Substs),T1,T2),
	X2 =.. [H1|T2].

%% make new variables for the constants
make_const_var_substs([], []).
make_const_var_substs([H|T], [(H/_NewVar)|NewVars]):-
	make_const_var_substs(T, NewVars).

%% generalize local constants into universally bound variables
%% generalize_consts(+In, -Out, -UnivContext, -Subst)
%% all local consts in -Out are replaced with new vars, these vars
%% are collected in UnivContext (with their sorts),
%% -Subst is the substitution which turns -Out into +In again
generalize_consts(In, Out, UnivContext, NewConstSubst):-
	collect_symbols_top(In, Syms0),
	sublist(mptp_local_const, Syms0, Consts0),
	add_consts([], Consts0, Consts0, SortRefs, AllConsts),
	length(AllConsts, N),
	ensure(length(SortRefs, N), gen_consts(In,AllConsts,SortRefs)),
	maplist(get_ref_fof, SortRefs, SortFofs),
	sort_by_const_names(SortFofs, [], SortFofs1),
	maplist(arg(3), SortFofs1, SortFlas1),
	ensure( zip_s(sort, SortedConsts,Sorts,SortFlas1),gen_consts),
	zip_s(':', SortedConsts, Sorts, Context1),
	once(make_const_var_substs(SortedConsts, NewConstSubst)),
	apply_const_substs(NewConstSubst, [In,Context1], [Out,UnivContext]).
	
%%%%%%%%%%%% End of constant generalization %%%%%%%%%%%%%%%%%%%%%%%%%%%	


%% test uniqueness of references inside Article
test_refs(Article):-
	mml_dir_atom(MMLDir),
	concat_atom([MMLDir, Article,'.xml2'],File),
	consult(File),
	install_index,
	findall([Fof1,Fof2], (fof_name(X,Id1),fof_name(X,Id2),
				 Id1 < Id2, clause(Fof1,_,Id1),
				 clause(Fof2,_,Id2)),S),
	print(S),nl,length(S,N),print(N),
	retractall(fof(_,_,_,file(Article,_),_)).

%% Shorter is an ancestor level or equal to Longer
sublevel(Longer,Shorter) :- append(Shorter,_,Longer).

%% filter out refs with more special level
filter_level_refs(Lev,RefsIn,RefsOut):-
	sort(RefsIn,Refs1),
	findall(Ref,(member(Ref,Refs1),
		     get_ref_fof(Ref,fof(Ref,_,_,_,Info)),
		     Info = [mptp_info(_,Lev1,_,_,_)|_],
		     sublevel(Lev,Lev1)), RefsOut).
%	findall(Ref,(member(Ref,RefsIn),atom_chars(Ref,[C|_]),
%		     member(C,[t,d,l])), Refs1),


%% childern and decendants of a level expressed as atom (for speed)
at_level_children(At1,Childs):- findall(C, fof_parentlevel(At1,C), Childs).
at_level_descendents(At1,Descs):-
	at_level_children(At1,Childs),
	( Childs = [], Descs = [];
	    Childs \= [],
	    maplist(at_level_descendents,Childs,Ds1),
	    flatten([Childs|Ds1], Descs)).

%% all fof names on this level (block) and below, uses fof_level/2 and
%% fof_parentlevel/2 for speed 
get_sublevel_names(Lev,Names):-
	level_atom(Lev, At1),
	at_level_descendents(At1, Descs),
	findall(Name,( member(L1, [At1|Descs]),
		      fof_level(L1, Id),		     
		      clause(fof(Name,_,_,_,_),_,Id)), Names).

%% get all (also proof-local) symbols used in this proof, and all
%% formula names in the proof. the proof
%% is described by the explicit references and the proof level.
%% Symbols from the references and from all formulas on this level
%% and below are collected.
%% used as input for the fixpoint algorithm
%% NOTE: proof-local symbols cannot be filtered-out here. E.g.
%%   if we filtered-out a locally reconsidered constant, whose
%%   original is outside this proof, we would never reach the original
%%   in the fixpoint algorithm. So we have to pass all symbols to
%%   the fixpoint algorithm, and filter the proof-local flas afterwards.
get_proof_syms_and_flas(RefsIn, PLevel, PSyms, PRefs):-
	get_sublevel_names(PLevel, Names),
	append(RefsIn, Names, AllNames),
	sort(AllNames, PRefs),!,
	maplist(get_ref_fla, PRefs, Flas1),
	collect_symbols_top(Flas1, PSyms).
	
%% Kinds is a list [InferenceKinds, PropositionKinds]
%% possible InferenceKinds are now [mizar_by, mizar_from, mizar_proof]
%% possible PropositionKinds are now [theorem, top_level_lemma, sublemma]
mk_article_problems(Article,Kinds):-
%	declare_mptp_predicates,
%	load_mml,
	mml_dir_atom(MMLDir),
	concat_atom([MMLDir, Article,'.xml2'],File),
	consult(File),
	once(assert_sch_instances(Article)),
	install_index,
	concat_atom(['problems/',Article,'/'],Dir),
	(exists_directory(Dir) -> (string_concat('rm -r -f ', Dir, Command),
				      shell(Command)); true),
	make_directory(Dir),
	repeat,(mk_problem(_,Article,Dir,Kinds),fail; !,true),
%% retract current file but return mml parts
	retractall(fof(_,_,_,file(Article,_),_)),
	concat_atom([MMLDir ,Article, '.dcl2'],DCL),
	concat_atom([MMLDir ,Article, '.dco2'],DCO),
	concat_atom([MMLDir ,Article, '.the2'],THE),
	concat_atom([MMLDir ,Article, '.sch2'],SCH),
	sublist(exists_file,[DCL,DCO,THE,SCH],ToLoad),
	load_files(ToLoad,[silent(true)]).
%	retractall(fof(_,_,_,file(Article,_),[mptp_info(_,_,proposition,_,_)|_])),
%	retractall(fof(_,_,_,file(Article,_),[mptp_info(_,_,constant,_,_)|_])),!.


%% create proving problem for a given proposition,
%% (P does not have to be instantiated), store it in file Prefix.P
%% propositions only from the current article can be loaded -
%% - otherwise change their naming
%% mk_problem(?P,+F,+Prefix,+Kinds)
%% possible InferenceKinds are now [mizar_by, mizar_proof, mizar_from]
%% possible PropositionKinds are now [theorem, top_level_lemma, sublemma]
mk_problem(P,F,Prefix,[InferenceKinds,PropositionKinds]):-
	theory(F, Theory),
	member(InfKind,InferenceKinds),
	member(PropKind,PropositionKinds),
	((PropKind == theorem, MPropKind = theorem);
	    (PropKind \= theorem,MPropKind = proposition,
		((PropKind == top_level_lemma,Lev = []);
		    PropKind \= top_level_lemma))),
	fof(P,_,Fla,file(F,P),
	    [mptp_info(Nr,Lev,MPropKind,position(Line,Col),_),
	     inference(InfKind,InfInfo,Refs0)]),
%	filter_level_refs(Lev,Refs0,Refs),
	concat_atom([Prefix,F,'__',P],Outfile),
	tell(Outfile),
	format('% Mizar problem: ~w,~w,~w,~w ~n', [P,F,Line,Col]),
	(
	  InfKind == mizar_proof,
	  ensure(InfInfo = [proof_level(PLevel)|_], inf_info(InfInfo,PLevel)),
	  get_proof_syms_and_flas([P|Refs0], PLevel, PSyms, PRefs),
	  Syms1 = PSyms,
	  once(fixpoint(F, InfKind, PRefs, [], Syms1, AllRefs0)),
	  %% incorrect, but needs handling of fraenkels and sch_insts
	  filter_level_refs(Lev,AllRefs0,AllRefs)
	;
	  InfKind \== mizar_proof,
	  Refs = Refs0,
	  maplist(get_ref_fla, [P|Refs], Flas1),
	  collect_symbols_top(Flas1, Syms0),
	  Syms1 = Syms0,
	  once(fixpoint(F, InfKind, [P|Refs], [], Syms1, AllRefs))
	),
	%% collect fraenkel infos and put variables into fraenkel flas
	findall([fof(R,R1,Out,R3,R4),Info],
		(member(R,AllRefs), get_ref_fof(R,fof(R,R1,R2,R3,R4)),
		    all_collect_top(R2,Out,Info)),
		S1),
	zip(Flas, Infos1, S1),
	append_l(Infos1,Infos),
	%% instantiate fraenkels with their defs, create the defs
	mk_fraenkel_defs_top(Infos, NewFrSyms, Defs),
	findall(dummy,(nth1(Pos,Defs,D),
		       sort_transform_top(D,D1), numbervars(D1,0,_),
		       print(fof(Pos,axiom,D1,file(F,Pos),[freankel])),write('.'),nl),_),

	( member(_,Defs) -> Refs1 = [t2_tarski|AllRefs]; Refs1 = AllRefs),
	findall(dummy,(member(Q,Refs1), (member(fof(Q,Q1,Q2,Q3,Q4),Flas);
					    (Q=t2_tarski,fof(Q,Q1,Q2,Q3,Q4))),
		       sort_transform_top(Q2,SR2), numbervars([SR2,Q3,Q4],0,_),
		       (Q=P -> Status = conjecture; Status = axiom),
		       print(fof(Q,Status,SR2,Q3,Q4)),write('.'),nl),_),
%	fof(P,_,P2,file(F,P),P4),
%	member(fof(P,_,P2,file(F,P),P4),Flas),
%	sort_transform_top(P2,SP2), numbervars(SP2,0,_),
%	print(fof(P,conjecture,SP2,file(F,P),[P4])), write('.'),nl,
	told.



%% allowed file estensions for theory files
theory_exts([dcl,dco,evl,sch,the]).

%% Kind must be in theory_exts
load_theory_files(Kind):-
	atom(Kind),
	theory_exts(Exts),
	member(Kind, Exts),
	mml_dir_atom(MMLDir),
	concat_atom([MMLDir, '*.', Kind, '2'], Anames),
	string_to_atom(WildCard, Anames),
	expand_file_name(WildCard, Names),
	load_files(Names,[silent(true)]).


load_clusters:- load_theory_files(dcl).
load_constructors:- load_theory_files(dco).
load_environs:- load_theory_files(evl).
load_schemes:- load_theory_files(sch).
load_theorems:- load_theory_files(the).

load_mml:-
	load_clusters,load_theorems,load_schemes,
	load_constructors,load_environs.


% should fail - load with theorems and propositions first
check_refs:-
	fof(_,_,_,_,[mptp_info(_,_,_,_,_),inference(mizar_by,_,I)]),
	member(P,I),
	not(fof(P,_,_,_,_)).

% should fail
check_consts:-
	File=abcmiz_0,
	findall(L1,(fof(_,_,F,file(File,_),_),collect_symbols_top(F,L1)),L2),
	union1(L2,[],L),!,
	member(Const,L),
	atom_prefix(Const,'c'),
	not(fof(_,_,_,file(File,Const),_)).

%% encode and decode between [1,2,3] and '1_2_3'
level_atom(List,Atom):- atom(Atom), !, concat_atom(V1,'_',Atom),
	maplist(atom_number,V1,List).
level_atom([H|T],Atom):- ground([H|T]),
	maplist(atom_number,V1,[H|T]),
	concat_atom(V1,'_',Atom).

%% installs the indeces for fast lookup of fof's;
%% should be called only after addition of custom fof's like
%% scheme instance, e.g.:
%% declare_mptp_predicates,assert_sch_instances(File), install_index
install_index:-
	abolish(fof_name/2),
	abolish(fof_section/2),
	abolish(fof_level/2),
	abolish(fof_parentlevel/2),
	abolish(fof_cluster/3),
	abolish(fof_req/3),
%	add_hidden,
	logic_syms(LogicSyms),
	repeat,
	( clause(fof(Ref,_,_,_,_),_,Id),
	    assert(fof_name(Ref, Id)), fail; true),
	repeat,
	( clause(fof(_,_,_,file(_,Sec1), _),_,Id),
	    assert(fof_section(Sec1, Id)), fail; true),
	findall(L_l, (
		       clause(fof(_,_,_,_,[mptp_info(_,L_l,_,_,_)|_]),_,Id),
		       L_l = [_|_],
		       level_atom(L_l,Lev1),
		       assert(fof_level(Lev1, Id))), Levs),
	sort(Levs,Levs1),
	repeat,
	( member(L_l1,Levs1),
	    L_l1 = [_,_|_],
	    level_atom(L_l1,Lev1),
	    append(L2, [_], L_l1),
	    level_atom(L2,Lev2),
	    assert(fof_parentlevel(Lev2,Lev1)), fail; true),
	findall(d, (
		     member(Cl,[fcluster,ccluster]),
		     fof(Ref,_,Fla,file(F,_),[mptp_info(_,_,Cl,_,_)|_]),
		     cl_needed_syms_top(Fla,AnteSyms),
		     assert(fof_cluster(F,Ref,AnteSyms))), _),
	findall(d, (
		     fof(Ref,_,Fla,file(F,_),[mptp_info(_,_,rcluster,_,_)|_]),
		     collect_symbols_top(Fla,AllSyms),
		     subtract(AllSyms,LogicSyms,Syms),
		     assert(fof_cluster(F,Ref,AnteSyms))), _),
	findall(d, (
		     member(F,[numerals, boole, subset, arithm, real]),
		     fof(Ref,_,Fla,file(F,_),[mptp_info(_,_,theorem,_,_)|_]),
		     collect_symbols_top(Fla,AllSyms),
		     subtract(AllSyms,LogicSyms,Syms),
		     assert(fof_req(F,Ref,Syms))), _).


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
	zip(Flas,Infos1,S),
	append_l(Infos1,Infos),
	mk_fraenkel_defs_top(Infos,NewFrSyms,D),
	length(D,N1), maplist(length,NewFrSyms,Lengths),
	sumlist(Lengths,N2),
	maplist(collect_symbols_top,Flas,L),
	union1(L,[],L2),length(L2,N3),write([N1,N2,N3]).

dotimes(_,0).
dotimes(X,N) :- N1 is N-1, (call(X);true), dotimes(X,N1).

