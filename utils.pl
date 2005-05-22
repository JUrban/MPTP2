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
logic_syms([++,--,'$',~,'|','~|',&,~&,=>,<=,<=>,<~>,!,?,:,'..',sort,all,'.',[]]).


portray(A = B):- format(' equal(~p,~p) ',[A,B]).
portray(~A):- write(' ~ ('), print(A), write(') ').
portray(A & B):- format(' (~p & ~p) ',[A,B]).
portray(A => B):- format(' (~p => ~p) ',[A,B]).
portray(A <=> B):- format(' (~p <=> ~p) ',[A,B]).
portray(A : B):- var(A), format(' ( ~p : ~p) ',[A,B]).
portray(! A : B):- format(' (! ~p : ~p) ',[A,B]).
portray(? A : B):- format(' (? ~p : ~p) ',[A,B]).

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
 multifile(fof/4),
 multifile(fof/5),
 dynamic(fof/5),
 multifile(theory/2),
 abolish(fof_name/2),
 abolish(fof_section/2),
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

%% collect ground counts into buk/3, stack failure otherwise
%% then print and run perl -e 'while(<>) { /.([0-9]+), (.*)./; $h{$2}+=$1;} foreach $k (sort {$h{$b} <=> $h{$a}} (keys %h)) {print "$h{$k}:$k\n";}'
%% on the result
get_ground_info:-
	fof(Ref,_,Fla,file(_,_), mptp_info(_,theorem)),
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
mk_fraenkel_defs([[V,C,Trm]|T], _, FrSyms, NewFrSyms, [D|Defs]):-
	mk_fraenkel_def(V, C, Trm, FrSyms, FrSyms1, _, D),
	mk_fraenkel_defs(T, _, FrSyms1, NewFrSyms, Defs).

% should be unique for Ref
get_ref_fla(Ref,Fla):- fof_name(Ref,Id),clause(fof(Ref,_,Fla,_,_),_,Id),!.

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
	get_sec_info_refs(RefsIn, SymsIn, mptp_info(_,_,property(_)), AddedRefs).

get_existence(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn, mptp_info(_,_,existence), AddedRefs).

get_redefinitions(RefsIn,SymsIn,AddedRefs):-	get_sec_info_refs(RefsIn, SymsIn,
			  mptp_info(_,_,redefinition(_,_,_,_)), AddedRefs).

get_types(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn, mptp_info(_,_,ctype), Refs1),
	get_sec_info_refs(RefsIn, SymsIn, mptp_info(_,_,constant(_,type)), Refs2),
	get_sec_info_refs(RefsIn, SymsIn, mptp_info(_,_,functor(scheme,type)), Refs3),
	flatten([Refs1, Refs2, Refs3], AddedRefs).

get_equalities(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  mptp_info(_,_,constant(_,equality)), AddedRefs).

%% OldSyms are used only for clusters and requirements
one_pass(F,RefsIn,OldSyms,NewSyms,AddedRefs):-
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

%% add symbol references until nothing added
fixpoint(F,RefsIn,OldSyms,NewSyms,RefsOut):-
	one_pass(F, RefsIn, OldSyms, NewSyms, Refs1), !,
	(Refs1 = [] -> RefsOut = RefsIn;	    
	    union(Refs1, RefsIn, Refs2),
	    union(OldSyms, NewSyms, OldSyms1),
	    maplist(get_ref_fla, Refs1, Flas1),
	    collect_symbols_top(Flas1, Syms1),
	    subtract(Syms1, OldSyms1, NewSyms1), 
	    fixpoint(F, Refs2, OldSyms1, NewSyms1, RefsOut)).


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
		  file(boole,spc0_boole),mptp_info(spc0_boole,theorem))):- !.
nr_boole(N,Res):-
	integer(N), N > 0, concat_atom([spc,N,'_boole'],Name),
	Res= fof(Name,theorem, ~ (v1_xboole_0(N)),
		 file(boole,Name),mptp_info(Name,theorem)).

nr_numerals(N,Res):-
	integer(N), N > 0, concat_atom([spc,N,'_numerals'],Name),
	Res= fof(Name,theorem,
		 sort(N,(v4_ordinal2 & m1_subset_1(k5_numbers))),
		 file(numerals,Name),mptp_info(Name,theorem)).


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

mk_first100:-
	declare_mptp_predicates,load_mml1,first100(L),!,
	member(A,L),mk_article_problems(A),fail.

mk_article_problems(Article):-
%	declare_mptp_predicates,
%	load_mml1,
	MMLDir='/home/urban/mizsrc.current.test6/test/tmp/',
	concat_atom([MMLDir, Article,'.xml2'],File),
	consult(File),
	install_index,
	concat_atom(['problems/',Article,'/'],Dir),
	(exists_directory(Dir) -> (string_concat('rm -r -f ', Dir, Command),
				      shell(Command)); true),
	make_directory(Dir),
	findall(P,mk_prop_problem(P,Article,Dir),_),
%% retract current file but return mml parts
	retractall(fof(_,_,_,file(Article,_),_)),
	concat_atom([MMLDir ,Article, '.dcl2'],DCL),
	concat_atom([MMLDir ,Article, '.dco2'],DCO),
	concat_atom([MMLDir ,Article, '.the2'],THE),
	sublist(exists_file,[DCL,DCO,THE],ToLoad),
	load_files(ToLoad,[silent(true)]).
%	retractall(fof(_,_,_,file(Article,_),mptp_info(_,_,proposition(_,_,_)))),
%	retractall(fof(_,_,_,file(Article,_),mptp_info(_,_,proposition(_,_,_),_))),
%	retractall(fof(_,_,_,file(Article,_),mptp_info(_,_,constant(_,_)))),!.

%theory(fraenkel, [registrations([finsub_1, funct_1, relset_1, subset_1, finset_1, relat_1,
%				zfmisc_1, xboole_0, fraenkel]),
%		  requirements([boole, subset])]).
% create proving problem for a given proposition, store it in file Prefix.P
% propositions only from the current article can be loaded -
% - otherwise change their naming
mk_prop_problem(P,F,Prefix):-
	theory(F, Theory),
	fof(P,lemma-derived,Fla,file(F,P),
		   mptp_info(Nr,Lev,proposition(Line,Col,_),
			     inference(mizar_by,_,Refs))),
	atom_concat(Prefix,P,Outfile),
	tell(Outfile),
	format('% Mizar problem: ~w,~w,~w,~w ~n', [P,F,Line,Col]),
	maplist(get_ref_fla, [P|Refs], Flas1),
	collect_symbols_top(Flas1, Syms1),
	once(fixpoint(F, [P|Refs], [], Syms1, AllRefs)),
	findall([fof(R,R1,Out,R3,R4),Info],
		(member(R,AllRefs), fof_name(R,Id),
		    clause(fof(R,R1,R2,R3,R4),_,Id),
		    all_collect_top(R2,Out,Info)),
		S1),
	zip(Flas, Infos1, S1),
	append_l(Infos1,Infos),
	mk_fraenkel_defs_top(Infos, NewFrSyms, Defs),
	findall(dummy,(nth1(Pos,Defs,D),
		       sort_transform_top(D,D1), numbervars(D1,0,_),
		       print(fof(Pos,axiom,D1,file(F,Pos),[freankel])),write('.'),nl),_),

	( member(_,Defs) -> Refs1 = [t2_tarski|AllRefs]; Refs1 = AllRefs),
	findall(dummy,(member(Q,Refs1), (member(fof(Q,Q1,Q2,Q3,Q4),Flas);
					    (Q=t2_tarski,fof(Q,Q1,Q2,Q3,Q4))),
		       sort_transform_top(Q2,SR2), numbervars(SR2,0,_),
		       (Q=P -> Status = conjecture; Status = axiom),
		       print(fof(Q,Status,SR2,Q3,[Q4])),write('.'),nl),_),
%	fof(P,_,P2,file(F,P),P4),
%	member(fof(P,_,P2,file(F,P),P4),Flas),
%	sort_transform_top(P2,SP2), numbervars(SP2,0,_),
%	print(fof(P,conjecture,SP2,file(F,P),[P4])), write('.'),nl,
	told.

% loads def. theorems too
load_theorems:-
	expand_file_name("/home/urban/tmp-miz/mml/*.the2",K),
	load_files(K,[silent(true)]).

load_theorems1:-
	expand_file_name("/home/urban/mizsrc.current.test6/test/tmp/*.the2",K),
	load_files(K,[silent(true)]).

% should fail - load with theorems and propositions first
check_refs:-
	fof(_,_,_,_,mptp_info(_,_,_,inference(mizar_by,_,I))),
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
	

load_clusters:-
	expand_file_name("/home/urban/tmp-miz/mml/*.dcl2",K),
	load_files(K,[silent(true)]).

load_clusters1:-
	expand_file_name("/home/urban/mizsrc.current.test6/test/tmp/*.dcl2",K),
	load_files(K,[silent(true)]).


load_constructors:-
	expand_file_name("/home/urban/tmp-miz/mml/*.dco2",K),
	load_files(K,[silent(true)]).

load_constructors1:-
	expand_file_name("/home/urban/mizsrc.current.test6/test/tmp/*.dco2",K),
	load_files(K,[silent(true)]).

load_environs1:-
	expand_file_name("/home/urban/mizsrc.current.test6/test/tmp/*.evl2",K),
	load_files(K,[silent(true)]).

load_mml1:- load_clusters1,load_theorems1,load_constructors1,load_environs1.

install_index:-
	abolish(fof_name/2),
	abolish(fof_section/2),
	abolish(fof_cluster/3),
	abolish(fof_req/3),
%	add_hidden,
	logic_syms(LogicSyms),
	findall(d, (
		     clause(fof(Ref,_,_,_,_),_,Id),
		     assert(fof_name(Ref, Id))), _),
	findall(d, (
		     clause(fof(_,_,_,file(_,Sec1), _),_,Id),
		     assert(fof_section(Sec1, Id))), _),
	findall(d, (
		     member(Cl,[fcluster,ccluster]),
		     fof(Ref,_,Fla,file(F,_),mptp_info(_,Cl)),
		     cl_needed_syms_top(Fla,AnteSyms),
		     assert(fof_cluster(F,Ref,AnteSyms))), _),
	findall(d, (
		     fof(Ref,_,Fla,file(F,_),mptp_info(_,rcluster)),
		     collect_symbols_top(Fla,AllSyms),
		     subtract(AllSyms,LogicSyms,Syms),
		     assert(fof_cluster(F,Ref,AnteSyms))), _),
	findall(d, (
		     member(F,[numerals, boole, subset, arithm, real]),
		     fof(Ref,_,Fla,file(F,_),mptp_info(_,theorem)),
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

