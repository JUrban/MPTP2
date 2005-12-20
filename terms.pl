%%- -*-Mode: Prolog;-*--------------------------------------------------
%%
%% File  : terms.pl
%%
%% Author: Josef Urban
%%
%%  MPTP2 term utilities, tested only with SWI Prolog 5.2 now.
%%------------------------------------------------------------------------

:- ensure_loaded([utils]).


%% get_var_type(+,+,-) get type of a bound variable, 
%% must be in the context
get_var_type(Var, [], _):- throw(get_var_type(Var)).

get_var_type(Var, [(X:Type)|_], Type):-
	var(Var), X == Var, !.

get_var_type(Var, [_|T], Type):- !, get_var_type(Var, T, Type).

%% get_func_type(+,+,-) get the type of a complex term 
%% or a local constant, no checking of argument types
%% options for Info:
%% [mptp_info(_,_,_,_,[ctype])|_]
%% [mptp_info(_,_,constant,_,[_,type])|_]
%% [mptp_info(_,_,functor,_,[scheme,type])|_]
get_func_type(Term, Info, Type):-
	ensure(not(var(Term)), get_func_type(Term)),
	Term =.. [Constr | _],
	fof_section(Constr,Id),
	clause(fof(_,sort,Decl,file(_,Constr), Info),_,Id),!,
	(
	  Decl = ( ! _ : sort(Term, Type)) 
	;

	  Decl = sort(Term, Type)
	), !.

get_func_type(Term, _, _):- throw(get_func_type(Term)).

%% fraenkel yields just "set"
get_term_type(all(_,_,_), _, $true):- !.



get_trm_types_qlist([],C,C,[]).
get_trm_types_qlist([(X:S)|T],Context,NewContext,Types):-
	get_trm_types(S,Context,Types0),
	get_trm_types_qlist(T,[(X:S)|Context],NewContext,Types_l),
	append(Types0,Types_l,Types).


%% get_trm_types(+,+,-) get types of all terms in Expr
get_trm_types(Var, Context, [(Var:Type)]):-
	var(Var), !, get_var_type(Var, Context, Type).

get_trm_types(! Svars : Y, Context, Types):- !,
	get_trm_types_qlist(Svars, Context, NewContext, T1),
	get_trm_types(Y, NewContext, T2),
	append(T1,T2,Types).

get_trm_types(? Svars : Y, Context, Types):- !,
	get_trm_types_qlist(Svars, Context, NewContext, T1),
	get_trm_types(Y, NewContext, T2),
	append(T1,T2,Types).

get_trm_types(all(Svars,Frm,Trm), Context,
	      [(all(Svars,Frm,Trm):Typ)|Types]):- !,
	get_trm_types_qlist(Svars, Context, NewContext, T1),
	get_trm_types([Frm,Trm], NewContext, T2),
	get_term_type(all(Svars,Frm,Trm), Context, Typ),
	append(T1,T2,Types).

get_trm_types([], _, []):- !.
get_trm_types([H|T], Context, Types):- !,
	get_trm_types(H, Context, T1),
	get_trm_types(T, Context, T2),
	append(T1,T2,Types), ! .

get_trm_types(Expr, _, (Expr : $true)):- number(Expr), !.
get_trm_types(Expr, Context, Types):- !,
	Expr =.. [H | Args],
	get_trm_types(Args, Context, Types1), !,
	(
	  atom_chars(H, [C1,C2 | _]), C2 @=< '9', C2 @>= '0',
	  member(C1, [c,f,g,k,u]), !,
	  (
	    C1 = c,
	    Info = [mptp_info(_,_,constant,_,[_,type])|_]
	  ;
	    C1 = f,
	    Info = [mptp_info(_,_,functor,_,[scheme,type])|_]
	  ;
	    member(C1, [g,k,u]),
	    Info = [mptp_info(_,_,_,_,[ctype])|_]
	  ),
	  get_func_type(Expr, Info, Typ),
	  Types = [ (Expr : Typ) | Types1 ]
	;
	  Types = Types1
	).
	
