%% File: leancop_dnf.pl  -  Version: 1.32  -  Date: 2011
%%
%% Purpose: Call the leanCoP core prover for a given formula with a machine learning server.
%%
%% Authors: Jiri Vyskocil
%%
%% Usage:   leancop_dnf(X,S,R). % proves formula in file X with
%%                               %  settings S and returns result R
%%
%% Copyright: (c) 2010 by Jiri Vyskocil
%% License:   GNU General Public License

:- dynamic(ai_advisor/0).  % with format: ai_advisor(server_location:port)
:- dynamic(cache_table/2). % format: cache_table(<query>,<table_functor_name>)
:- dynamic(dnf/4).         % dnf table with format: dnf(Index,Type,Clause,Ground)
:- flag(table_index,_,1).  % index of the last created table
:- dynamic(lit/5).
:- dynamic(advised_lit/5).
:- dynamic(best_lit_mode/1).
:- dynamic(machine_learning_of_subtrees/0).

%:- assert(best_lit_mode(original_leancop)). 
% original_leancop
% naive_and_complete
% naive
% full_caching_and_complete

:- [leancop_main].

:- [tcp_client].

:- op(100,fy,-). % due to problems with -3^[] as (-3)^[] instead of -(3^[])
	
load_dnf(File,Ls) :-
    open(File,read,Stream), 
    ( findall(dnf(Index,Type,Cla,G),(repeat,read(Stream,T),(T \== end_of_file -> T=dnf(Index,Type,Cla,G) ; (!,fail))),Ls)
    -> close(Stream) ; close(Stream), fail ).


leancop_dnf(File,Settings,Result) :-
%    axiom_path(AxPath), ( AxPath='' -> AxDir='' ;
%    name(AxPath,AxL), append(AxL,[47],DirL), name(AxDir,DirL) ),
%    ( leancop_tptp2(File,AxDir,[_],F,Conj) ->
%      Problem=F ; [File], f(Problem), Conj=non_empty ),
%    ( Conj\=[] -> Problem1=Problem ; Problem1=(~Problem) ),
%    leancop_equal(Problem1,Problem2),
%    make_matrix(Problem2,Matrix,Settings),
    flag(best_lit_attempts,_,0),
    Conj=non_empty,
    load_dnf(File,M),
    ( member(reo(I),Set) -> mreorder(M,Matrix,I) ; Matrix=M ),
    forall(member(X,Matrix),assertz(X)),
    (
      best_lit_mode(original_leancop),!,
      leancop_get_result(File,Matrix,Settings,Advisor_In,Advisor_Out,Proof,Result)    
     ;
      ai_advisor(DNS:PORT),
      create_client(DNS:PORT,Advisor_In,Advisor_Out),
      write(Advisor_Out,[File,16]),nl(Advisor_Out),flush_output(Advisor_Out),
      leancop_get_result(File,Matrix,Settings,Advisor_In,Advisor_Out,Proof,Result),    
      close_connection(Advisor_In,Advisor_Out)
     ). 

leancop_get_result(File,Matrix,Settings,Advisor_In,Advisor_Out,Proof,Result) :-    
    ( prove2(Matrix,Settings,Advisor_In,Advisor_Out,Proof) ->
      ( Conj\=[] -> Result='Theorem' ; Result='Unsatisfiable' ) ;
      ( Conj\=[] -> Result='Non-Theorem' ; Result='Satisfiable' )
    ),
    output_result(File,Matrix,Proof,Result,Conj).
    
%% File: leancop21_swi.pl  -  Version: 2.1  -  Date: 30 Aug 2008
%%
%%         "Make everything as simple as possible, but not simpler."
%%                                                 [Albert Einstein]
%%
%% Purpose: leanCoP: A Lean Connection Prover for Classical Logic
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de
%%
%% Usage: prove(M,P).    % where M is a set of clauses and P is
%%                       %  the returned connection proof
%%                       %  e.g. M=[[q(a)],[-p],[p,-q(X)]]
%%                       %  and  P=[[q(a)],[[-(q(a)),p],[[-(p)]]]]
%%        prove(F,P).    % where F is a first-order formula and
%%                       %  P is the returned connection proof
%%                       %  e.g. F=((p,all X:(p=>q(X)))=>all Y:q(Y))
%%                       %  and  P=[[q(a)],[[-(q(a)),p],[[-(p)]]]]
%%        prove2(F,S,P). % where F is a formula, S is a subset of
%%                       %  [nodef,def,conj,reo(I),scut,cut,comp(J)]
%%                       %  (with numbers I,J) defining attributes
%%                       %  and P is the returned connection proof
%%
%% Copyright: (c) 1999-2008 by Jens Otten
%% License:   GNU General Public License


% :- [def_mm].  % load program for clausal form translation

assert_mode(original_leancop,Pred) :-
XXX=((
best_lit(_Advisor_In,_Advisor_Out,_Cla,_Path,_PathLength,_PathLim,_Lem,NegLit,Clause,Ground,IDX) :-
         lit(NegLit,NegL,Clause,Ground,IDX),
         unify_with_occurs_check(NegL,NegLit)
)), XXX=(HHH:-BBB), HHH=..[_|REST], QQQ=..[Pred|REST], assert((QQQ :-BBB)).

%%%

assert_mode(naive_and_complete,Pred) :-
XXX=((
best_lit(Advisor_In,Advisor_Out,_Cla,Path,_PathLength,_PathLim,_Lem,NegLit,Clause,Ground,I) :-
         collect_symbols_top([NegLit|Path],Ps,Fs),
         append(Ps,Fs,Ss),
%         writeln('sending to AI...'),
         write(Advisor_Out,Ss),nl(Advisor_Out),flush_output(Advisor_Out),
%         writeln('reading from AI...'),
         read(Advisor_In,Indexes),!,
%         writeln('done'),
         (
		 ( member(I,Indexes),
		   lit(NegLit,NegL,Clause,Ground,I),
		   unify_with_occurs_check(NegL,NegLit)
		 )   
		  ;
		 (
		   lit(NegLit,NegL,Clause,Ground,I),
		   \+ member(I,Indexes),
		   unify_with_occurs_check(NegL,NegLit)
		 )
         )
)), XXX=(HHH:-BBB), HHH=..[_|REST], QQQ=..[Pred|REST], assert((QQQ :-BBB)).


%%%

assert_mode(naive,Pred) :-
XXX=((
best_lit(Advisor_In,Advisor_Out,_Cla,Path,_PathLength,_PathLim,_Lem,NegLit,Clause,Ground,I) :-
         collect_symbols_top([NegLit|Path],Ps,Fs),
         append(Ps,Fs,Ss),
         write(Advisor_Out,Ss),nl(Advisor_Out),flush_output(Advisor_Out),
         read(Advisor_In,Indexes),!,
         member(I,Indexes),
	 lit(NegLit,NegL,Clause,Ground,I),
	 unify_with_occurs_check(NegL,NegLit)
)), XXX=(HHH:-BBB), HHH=..[_|REST], QQQ=..[Pred|REST], assert((QQQ :-BBB)).


%%%

assert_mode(full_caching_and_complete,Pred) :-
XXX=((
best_lit(Advisor_In,Advisor_Out,_Cla,Path,_PathLength,_PathLim,_Lem,NegLit,Clause,Ground,IDX) :-
         collect_symbols_top([NegLit|Path],Ps,Fs),
         append(Ps,Fs,Ss),
         (
           cache_table(Ss,Table) ->
             true
           ;
             write(Advisor_Out,Ss),nl(Advisor_Out),flush_output(Advisor_Out),
             read(Advisor_In,Indexes),!,
             findall(dnf(Index,Type,Cla,G),(
               ( member(Index,Indexes), 
                 dnf(Index,Type,Cla,G)
               ; dnf(Index,Type,Cla,G), 
                 \+ member(Index,Indexes)
               )
             ),Cs),
             flag(table_index,I,I+1),
             name(I,Is),name(t,Ts),append(Is,Ts,Ns),
             name(Table,Ns),
             assert(cache_table(Ss,Table)),
             assert_clauses(Cs,Table,conj)
         ),!,
         Query=..[Table,NegLit,NegL,Clause,Ground,IDX],
         call(Query),
         unify_with_occurs_check(NegL,NegLit)
)), XXX=(HHH:-BBB), HHH=..[_|REST], QQQ=..[Pred|REST], assert((QQQ :-BBB)).


%%%

assert_mode(smart_caching_and_complete,Pred) :-
XXX=((
best_lit(Advisor_In,Advisor_Out,_Cla,Path,_PathLength,_PathLim,_Lem,NegLit,Clause,Ground,Index) :-
         collect_symbols_top([NegLit|Path],Ps,Fs),
         append(Ps,Fs,Ss),
         (
           cache_table(Ss,Table) ->
             true
           ;
             write(Advisor_Out,Ss),nl(Advisor_Out),flush_output(Advisor_Out),
             read(Advisor_In,Indexes),!,
             findall(dnf(IDX,Type,Cla,G),(
               member(IDX,Indexes), 
               dnf(IDX,Type,Cla,G)
             ),Cs),
             flag(table_index,I,I+1),
             name(I,Is),name(t,Ts),append(Is,Ts,Ns),
             name(Table,Ns),
	     Shape=..[Table,_,_,_,_,_],
	     dynamic(Shape),
   	     assert(cache_table(Ss,Table)),
	     assert_clauses(Cs,Table,conj)  
         ),!,
         (
           Query=..[Table,NegLit,NegL,Clause,Ground,Index],
           call(Query)
          ;  
            lit(NegLit,NegL,Clause,Ground,Index),
            Query=..[Table,_,_,_,_,Index],
            \+ call(Query)
         ),
         unify_with_occurs_check(NegL,NegLit)

)), XXX=(HHH:-BBB), HHH=..[_|REST], QQQ=..[Pred|REST], assert((QQQ :-BBB)).


%%%

assert_mode(smart_caching,Pred) :-
XXX=((
best_lit(Advisor_In,Advisor_Out,_Cla,Path,_PathLength,_PathLim,_Lem,NegLit,Clause,Ground,Index) :-
         collect_symbols_top([NegLit|Path],Ps,Fs),
         append(Ps,Fs,Ss),
         (
           cache_table(Ss,Table) ->
             true
           ;
             write(Advisor_Out,Ss),nl(Advisor_Out),flush_output(Advisor_Out),
             read(Advisor_In,Indexes),!,
             findall(dnf(IDX,Type,Cla,G),(
               member(IDX,Indexes), 
               dnf(IDX,Type,Cla,G)
             ),Cs),
             flag(table_index,I,I+1),
             name(I,Is),name(t,Ts),append(Is,Ts,Ns),
             name(Table,Ns),
	     Shape=..[Table,_,_,_,_,_],
	     dynamic(Shape),
   	     assert(cache_table(Ss,Table)),
	     assert_clauses(Cs,Table,conj)  
         ),!,
         (
           Query=..[Table,NegLit,NegL,Clause,Ground,Index],
           call(Query)
         ),
         unify_with_occurs_check(NegL,NegLit)

)), XXX=(HHH:-BBB), HHH=..[_|REST], QQQ=..[Pred|REST], assert((QQQ :-BBB)).

%%%

assert_mode(original_leancop_with_first_advise,Pred) :-
XXX=((
best_lit(_Advisor_In,_Advisor_Out,_Cla,_Path,_PathLength,_PathLim,_Lem,NegLit,Clause,Ground,IDX) :-
         advised_lit(NegLit,NegL,Clause,Ground,IDX),
         unify_with_occurs_check(NegL,NegLit)

)), XXX=(HHH:-BBB), HHH=..[_|REST], QQQ=..[Pred|REST], assert((QQQ :-BBB)).


%%%

assert_mode(limited_smart_and_complete_with_first_advise(Limitation),Pred) :-
XXX=((
best_lit(Advisor_In,Advisor_Out,Cla,Path,PathLength,PathLim,_Lem,NegLit,Clause,Ground,Index) :-
         append([NegLit/*|Cla*/],Path,Targets),
         collect_symbols_top(Targets,Ps,Fs),
         append(Ps,Fs,Ss),!,
         (
             /*length(Path,K), K*/ PathLength > Limitation ->
             %write(user_error,K),nl(user_error),
             (
               cache_table(Ss,Table) ->
                  !,(
		   Query=..[Table,NegLit,NegL,Clause,Ground,Index],
		   call(Query)
		   ;
		   advised_lit(NegLit,NegL,Clause,Ground,Index),
                   Query=..[Table,_,_,_,_,Index],
                   \+ call(Query)
                  ) 
                 ;
                 advised_lit(NegLit,NegL,Clause,Ground,Index)
             )
           ;
           (
		   cache_table(Ss,Table) ->
		     true
		   ; 
		     write(Advisor_Out,Ss),nl(Advisor_Out),flush_output(Advisor_Out),
		     read(Advisor_In,Indexes),!,%write(user_error,Indexes),nl(user_error),
		     findall(dnf(IDX,Type,Cla,G),(
		       member(IDX,Indexes), 
		       dnf(IDX,Type,Cla,G)
		     ),Cs),
		     flag(table_index,I,I+1),
		     name(I,Is),name(t,Ts),append(Is,Ts,Ns),
		     name(Table,Ns),
		     Shape=..[Table,_,_,_,_,_],
		     dynamic(Shape),
		     assert(cache_table(Ss,Table)),
		     assert_clauses(Cs,Table,conj)
		 ),!,
		 (
		 Query=..[Table,NegLit,NegL,Clause,Ground,Index],
		 call(Query)
		 ;  
		 advised_lit(NegLit,NegL,Clause,Ground,Index),
                 Query=..[Table,_,_,_,_,Index],
                 \+ call(Query)
		 )
         ),
         unify_with_occurs_check(NegL,NegLit)
         
)), XXX=(HHH:-BBB), HHH=..[_|REST], QQQ=..[Pred|REST], assert((QQQ :-BBB)).


%%%

assert_mode(limited_smart_with_first_advise(Limitation),Pred) :-
XXX=((
best_lit(Advisor_In,Advisor_Out,Cla,Path,PathLength,PathLim,_Lem,NegLit,Clause,Ground,Index) :-
         append([NegLit/*|Cla*/],Path,Targets),
         collect_symbols_top(Targets,Ps,Fs),
         append(Ps,Fs,Ss),!,
         (
             /*length(Path,K), K*/ PathLength > Limitation ->
             %write(user_error,K),nl(user_error),
             (
               cache_table(Ss,Table) ->
                  !,(
		   Query=..[Table,NegLit,NegL,Clause,Ground,Index],
		   call(Query)
                  ) 
                 ;
                 advised_lit(NegLit,NegL,Clause,Ground,Index)
             )
           ;
           (
		   cache_table(Ss,Table) ->
		     true
		   ; 
		     write(Advisor_Out,Ss),nl(Advisor_Out),flush_output(Advisor_Out),
		     read(Advisor_In,Indexes),!,%write(user_error,Indexes),nl(user_error),
		     findall(dnf(IDX,Type,Cla,G),(
		       member(IDX,Indexes), 
		       dnf(IDX,Type,Cla,G)
		     ),Cs),
		     flag(table_index,I,I+1),
		     name(I,Is),name(t,Ts),append(Is,Ts,Ns),
		     name(Table,Ns),
		     Shape=..[Table,_,_,_,_,_],
		     dynamic(Shape),
		     assert(cache_table(Ss,Table)),
		     assert_clauses(Cs,Table,conj)
		 ),!,
		 (
		 Query=..[Table,NegLit,NegL,Clause,Ground,Index],
		 call(Query)
		 )
         ),
         unify_with_occurs_check(NegL,NegLit)
         
)), XXX=(HHH:-BBB), HHH=..[_|REST], QQQ=..[Pred|REST], assert((QQQ :-BBB)).



%%% best_lit
%%% finds best lit according to a machine learning advisor.

:- best_lit_mode(M), (
assert_mode(M,best_lit)
;
M=scalable_with_first_advise(PreCounting_Threshold,Query_Threshold,Mode_After_Threshold),
assert_mode(Mode_After_Threshold,mode_lit),
assert((
best_lit(Advisor_In,Advisor_Out,Cla,Path,PathLength,PathLim,Lem,NegLit,Clause,Ground,Index) :-
%        length(Path,L), 
        copy_term(NegLit,NegLit1),
        L is PathLength + 1000000, % to ensure that this flag is not in collision with flag prove/9!
        flag(L,_,PreCounting_Threshold),!,
        ( 
		 advised_lit(NegLit1,NegL1,Clause1,Ground1,Index1),
		 
		 flag(L,I,I-1),
		 %write(user_error,(try(L,I))),nl(user_error),
		 ((I =< 1),!, %write(user_error,(go(L,I))),nl(user_error),
%	    ;
		 findall(advised_lit(NegLit,NegL_,Clause_,Ground_,IDX),
			(advised_lit(NegLit,NegL_,Clause_,Ground_,IDX) %,unify_with_occurs_check(NegL_,NegLit)
			)
			,Gs),
		 length(Gs,N),
%		 write(user_error,(NegLit-->N)),nl(user_error),
%		 write(user_error,N),nl(user_error),
		 (
		   N =< Query_Threshold ->
		         tail_list(PreCounting_Threshold,Gs,Ws),
			 member(advised_lit(NegLit,NegL,Clause,Ground,Index),Ws),
			 advised_lit(NegLit,NegL,Clause,Ground,Index),
			 unify_with_occurs_check(NegL,NegLit)
		     ; 
			 mode_lit(Advisor_In,Advisor_Out,Cla,Path,PathLength,PathLim,Lem,NegLit,Clause,Ground,Index)
		      		      
		 )
              ; (NegLit,NegL,Clause,Ground,Index)=(NegLit1,NegL1,Clause1,Ground1,Index1),
                 unify_with_occurs_check(NegL,NegLit)
            ))
))

;
M=scalable_with_first_advise(PreCounting_Threshold,Mode_After_Threshold),
assert_mode(Mode_After_Threshold,mode_lit),
assert((
best_lit(Advisor_In,Advisor_Out,Cla,Path,PathLength,PathLim,Lem,NegLit,Clause,Ground,Index) :-
        %length(Path,L), 
        copy_term(NegLit,NegLit1),
        L is PathLength + 1000000, % to ensure that this flag is not in collision with flag prove/9!
        flag(L,_,PreCounting_Threshold),!,
        ( 
		 advised_lit(NegLit1,NegL1,Clause1,Ground1,Index1),
		 
		 flag(L,I,I-1),
		 %write(user_error,(try(L,I))),nl(user_error),
		 ((I =< 1) ,!, %write(user_error,(go(L,I))),nl(user_error),
%	    ;
                   mode_lit(Advisor_In,Advisor_Out,Cla,Path,PathLength,PathLim,Lem,NegLit,Clause,Ground,Index)
		      
		      
		 
                 ; (NegLit,NegL,Clause,Ground,Index)=(NegLit1,NegL1,Clause1,Ground1,Index1),
                    unify_with_occurs_check(NegL,NegLit)
                 )
        )
))

).

tail_list(0,Ls,Ls).
tail_list(N,[_|Ls],Rs) :- M is N-1, !, tail_list(M,Ls,Rs).

%%% collect nonvar symbols from term

collect_symbols_top(Xs,Ps,Ls):-
        maplist(collect_predicate_symbols,Xs,Qs,LRs),
        sort(Qs,Ps),
        append(LRs,Rs),
	maplist(collect_symbols,Rs,L1),!,
	append(L1,L2),
	flatten(L2,L3),
	sort(L3,Ls).

collect_predicate_symbols(-X,P,As) :- X=..[P|As].
collect_predicate_symbols(X,P,As)  :- X=..[P|As].

collect_predicate_symbols([],[]).

collect_symbols(X,[]):- var(X),!.
collect_symbols(X,[X]):- atomic(X),!.
collect_symbols(X1,T2):-
	X1 =.. [H1|T1],
	maplist(collect_symbols,T1,T3),
	append(T3,T4),
	flatten(T4,T5),
	sort([H1|T5],T2).

%%% prove matrix M / formula F

prove(F,Advisor_In,Advisor_Out,Proof) :- prove2(F,[cut,comp(7)],Advisor_In,Advisor_Out,Proof).

prove2(M,Set,Advisor_In,Advisor_Out,Proof) :-
    retractall(lit(_,_,_,_,_)), (member(conj,Set) -> S=conj;S=pos)/*(member(dnf(_,_,[-(#)],_),M) -> S=conj ; S=pos)*/,
    assert_clauses(M,lit,S),
    best_lit_mode(Mode),
    ( member(Mode,[scalable_with_first_advise(_,_,_),
                   scalable_with_first_advise(_,_),
                   limited_smart_with_first_advise(_),
                   limited_smart_and_complete_with_first_advise(_),
                   original_leancop_with_first_advise]) ->
         retractall(advised_lit(_,_,_,_,_)),
         %findall(C,(dnf(_,_,X,_),select(#,X,C)),Cs),
         findall(C,dnf(_,conjecture,C,_),Cs),
         append(Cs,Qs),
         collect_symbols_top(Qs,Ps,Fs),
         append(Ps,Fs,Ss),            
         write(Advisor_Out,Ss),nl(Advisor_Out),flush_output(Advisor_Out),
         read(Advisor_In,Indexes),!,
         findall(advised_lit(L1,L2,Cla,G,Index),(
               ( member(Index,Indexes), 
                 lit(L1,L2,Cla,G,Index)
               ; lit(L1,L2,Cla,G,Index), 
                 \+ member(Index,Indexes)
               )
         ),Rs),
         forall(member(Q,Rs),assert(Q))
      ;
      true
    ),
    prove(1,Set,Advisor_In,Advisor_Out,Proof).

prove(PathLim,Set,Advisor_In,Advisor_Out,Proof) :-
    \+member(scut,Set) -> prove([-(#)],[],0,PathLim,[],Set,Advisor_In,Advisor_Out,[Proof]) ;
    lit(#,_,C,_,_) -> prove(C,[-(#)],0,PathLim,[],Set,Advisor_In,Advisor_Out,Proof1),
    Proof=[C|Proof1].
prove(PathLim,Set,Advisor_In,Advisor_Out,Proof) :-
    member(comp(Limit),Set), PathLim=Limit -> prove(1,[],Advisor_In,Advisor_Out,Proof) ;
    (member(comp(_),Set);retract(pathlim)) ->
    PathLim1 is PathLim+1, prove(PathLim1,Set,Advisor_In,Advisor_Out,Proof).

%%% leanCoP core prover

prove([],_,_,_,_,_,_,_,[]).

prove([Lit|Cla],Path,PathLength,PathLim,Lem,Set,Advisor_In,Advisor_Out,Proof) :-
    Proof=[[[NegLit|Cla1]|Proof1]|Proof2],
    \+ (member(LitC,[Lit|Cla]), member(LitP,Path), LitC==LitP),
    (-NegLit=Lit;-Lit=NegLit) ->
       ( member(LitL,Lem), Lit==LitL, Cla1=[], Proof1=[]
         ;
         member(NegL,Path), unify_with_occurs_check(NegL,NegLit),
         Cla1=[], Proof1=[]
         ;
          writeln('@'),
          (machine_learning_of_subtrees -> copy_term([NegLit|Path],Real_Input),flag(best_lit_attempts,PAST,PAST) ; true),
          best_lit(Advisor_In,Advisor_Out,Cla,Path,PathLength,PathLim,Lem,NegLit,Cla1,Grnd1,IDX),
          flag(best_lit_attempts,TRY,TRY+1),
%         lit(NegLit,NegL,Cla1,Grnd1,_IDX),
%         unify_with_occurs_check(NegL,NegLit),
         ( Grnd1=g -> true ; /*length(Path,K), K*/ PathLength<PathLim -> true ;
           \+ pathlim -> assert(pathlim), fail ),
         New_PathLength is PathLength+1,  
         prove(Cla1,[Lit|Path],New_PathLength,PathLim,Lem,Set,Advisor_In,Advisor_Out,Proof1),
         (machine_learning_of_subtrees -> 
              write('& '),
              flag(best_lit_attempts,NOW,NOW),
              ATT is NOW-PAST,
              UATT is TRY-PAST,
              collect_symbols_top(Real_Input,Ps,Fs),
              append(Ps,Fs,Ss),            
              write(best_lit_attempts=ATT),write('; '),
              write(best_lit_unsuccessful_attempts=UATT),write('; '),
              write(Ss), write(' >>> '), writeln(IDX)
            ; true)
       ),
       ( member(cut,Set) -> ! ; true ),
       prove(Cla,Path,PathLength,PathLim,[Lit|Lem],Set,Advisor_In,Advisor_Out,Proof2).


%%% write clauses into Prolog's database

assert_clauses([],_,_).
assert_clauses([dnf(Index,_,Cla,G)|M],Functor,Set) :-
    (Set\=conj, select(#,Cla,C) -> true ; Cla=C),
    (Set\=conj, \+member(-_,C) -> C1=[#|C] ; C1=C),
%    copy_term(C1,X),numbervars(X,1,_), print(X), nl,
    assert_clauses2(C1,[],G,Index,Functor),
    assert_clauses(M,Functor,Set).

assert_clauses2([],_,_,_,_).
assert_clauses2([L|C],C1,G,Index,Functor) :-
    assert_renvar([L],[L2]), append(C1,C,C2), append(C1,[L],C3),
    X=..[Functor,L2,L,C2,G,Index],assert(X), 
    assert_clauses2(C,C3,G,Index,Functor).

assert_renvar([],[]).
assert_renvar([F|FunL],[F1|FunL1]) :-
    ( var(F) -> true ; F=..[Fu|Arg], assert_renvar(Arg,Arg1),
      F1=..[Fu|Arg1] ), assert_renvar(FunL,FunL1).

%%% output of leanCoP proof

leancop_proof(Mat,Proof) :-
    proof(compact) -> leancop_compact_proof(Proof) ;
    proof(connect) -> leancop_connect_proof(Mat,Proof) ;
    proof(readable_with_global_index) -> leancop_readable_proof_with_global_index(Mat,Proof) ;
%    proof(machine_learning_with_global_index) -> leancop_machine_learning_proof_with_global_index(Mat,Proof) ;
    leancop_readable_proof(Mat,Proof).
/*
%%% print machine learning proof with global index of clauses

leancop_machine_learning_proof_with_global_index(Mat,Proof) :-
    print('%------------------------------------------------------'),
    nl,
    print_explanations,
    print('%Proof:'), nl, print('%------'), nl, nl,
    print('%Translation into (disjunctive) clausal form:'), nl,
    print_dnf(Mat,Mat1),
    print_introduction,
    calc_proof_with_global_index(Mat1,Mat,Proof,Proof1),
    print_proof(Proof1), nl,
    print_ending,
    print('%------------------------------------------------------'),
    nl, print(Proof), nl,
    nl, print(Proof1), nl,
    nl, print_ML_proof(Proof1),
    nl.
    
%%% print machine learning proof and sub-proofs with global index of clauses

print_ML_proof([(Cla,Num,Sub)|Proof]) :-
%    print_ML_proof_step([],Cla,Num,Sub),
    print_ML_proof(Proof,[1],[]).

print_ML_proof([],_,_).

print_ML_proof([[(Cla,Num,Sub)|Proof]|Proof2],[I|J],Path) :-
%   print_ML_proof_step([I|J],Cla,Num,Sub),
    Cla=[Lit|_],
    print(Path), nl, print(' >>> '), nl, print(Lit), nl, print(' ::: '), print(Num), nl,
    print_ML_proof(Proof,[1,I|J],[Lit|Path]), I1 is I+1,
    print_ML_proof(Proof2,[I1|J],Path).

    
print_ML_proof_step(I,Cla,Num,Sub) :-
    append(I,[1],I1), print_step(I1), print('  '), print(Cla),
    ( Num=(R:N) -> append(_,[H|T],I1), N1 is N+1, length([H|T],N1),
      ( R=r -> print('   (reduction:'), print_step(T) ;
               print('   (lemmata:'), print_step(T) ) ;
      print('   ('), print(Num) ), print(')  '),
    ( Sub=[[],_] -> true ; print('substitution:'), print(Sub) ), nl.
*/

    
%%% print readable proof with global index of clauses

leancop_readable_proof_with_global_index(Mat,Proof) :-
    print('------------------------------------------------------'),
    nl,
    print_explanations,
    print('Proof:'), nl, print('------'), nl, nl,
    print('Translation into (disjunctive) clausal form:'), nl,
    print_dnf(Mat,Mat1),
    print_introduction,
    calc_proof_with_global_index(Mat1,Mat,Proof,Proof1),
    print_proof(Proof1), nl,
    print_ending,
    print('------------------------------------------------------'),
    nl.

%%% print dnf clauses, print index number, print spaces

print_dnf([],[]) :- nl.
print_dnf([[dnf(_,_,-(#),_)]|Mat],Mat1) :- !, print_dnf(Mat,Mat1).
print_dnf([dnf(I,Type,Cla,G)|Mat],Mat1) :-
    append(Cla2,[#|Cla3],Cla),append(Cla2,Cla3,Cla1),
    print_dnf([dnf(I,Type,Cla1,G)|Mat],Mat1).
print_dnf([dnf(I,_Type,Cla,_G)|Mat],[Cla|Mat1]) :-
    print(' ('), print(I), print(')  '),
    print(Cla), nl, print_dnf(Mat,Mat1).

%%% calculate leanCoP proof

calc_proof_with_global_index(Mat,DNF_Mat,[Cla|Proof],[(Cla1,Num,Sub)|Proof1]) :-
    ((Cla=[#|Cla1];Cla=[-!|Cla1]) -> true ; Cla1=Cla),
    clause_num_sub_with_global_index(Cla1,[],[],Mat,DNF_Mat,1,Num,Sub),
    calc_proof_with_global_index(Cla1,[],[],Mat,DNF_Mat,Proof,Proof1).

calc_proof_with_global_index(_,_,_,_,_,[],[]).

calc_proof_with_global_index(Cla,Path,Lem,Mat,DNF_Mat,[[Cla1|Proof]|Proof2],Proof1) :-
    append(Cla2,[#|Cla3],Cla1), !, append(Cla2,Cla3,Cla4),
    append(Pro1,[[[-(#)]]|Pro2],Proof), append(Pro1,Pro2,Proof3),
    calc_proof_with_global_index(Cla,Path,Lem,Mat,DNF_Mat,[[Cla4|Proof3]|Proof2],Proof1).

calc_proof_with_global_index([Lit|Cla],Path,Lem,Mat,DNF_Mat,[[Cla1|Proof]|Proof2],Proof1) :-
    (-NegLit=Lit;-Lit=NegLit), append(Cla2,[NegL|Cla3],Cla1),
    NegLit==NegL, append(Cla2,Cla3,Cla4), length([_|Path],I) ->
      clause_num_sub_with_global_index(Cla1,Path,Lem,Mat,DNF_Mat,1,Num,Sub),
      Proof1=[[([NegLit|Cla4],Num,Sub)|Proof3]|Proof4],
      calc_proof_with_global_index(Cla4,[I:Lit|Path],Lem,Mat,DNF_Mat,Proof,Proof3),
      (Lem=[I:J:_|_] -> J1 is J+1 ; J1=1),
      calc_proof_with_global_index(Cla,Path,[I:J1:Lit|Lem],Mat,DNF_Mat,Proof2,Proof4).

%%% determine clause number and substitution

clause_num_sub_with_global_index([NegLit],Path,Lem,[],DNF_Mat,_,R:Num,[[],[]]) :-
    (-NegLit=Lit;-Lit=NegLit), member(Num:J:LitL,Lem), LitL==Lit ->
    R=J ; member(Num:NegL,Path), NegL==NegLit -> R=r.

clause_num_sub_with_global_index(Cla,Path,Lem,[Cla1|Mat],DNF_Mat,I,Num,Sub) :-
    append(Cla2,[L|Cla3],Cla1), append([L|Cla2],Cla3,Cla4),
    instance1(Cla,Cla4) ->
      nth_element(I,DNF_Mat,dnf(Num,_,_,_)), term_variables(Cla4,Var), copy_term(Cla4,Cla5),
      term_variables(Cla5,Var1), Cla=Cla5, Sub=[Var,Var1] ;
      I1 is I+1, clause_num_sub_with_global_index(Cla,Path,Lem,Mat,DNF_Mat,I1,Num,Sub).

nth_element(1,[E|_],E) :- !.
nth_element(I,[_|Ls],E) :- I1 is I - 1, nth_element(I1,Ls,E).

/*
append([],[]).
append([Ls],Ls).
append([As,Bs|Cs],Ls) :-
	append(As,Bs,Ds),
	append([Ds|Cs],Ls).
*/	
