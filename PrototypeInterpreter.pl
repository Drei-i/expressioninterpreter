:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, chars).

% ------------------------------
% REPL
% ------------------------------
main :-
    writeln('=== Advanced Expression Interpreter (Prolog) ==='),
    writeln('Type expressions or assignments. Use ; to chain statements.'),
    writeln('Type exit. to quit.'),
    repl([]).

repl(Env) :-
    write('>>> '), flush_output,
    read_line_to_string(user_input, Line),
    ( Line = "exit." -> true
    ; string_chars(Line, Chars),
      ( phrase(program(ASTs), Chars) ->
          eval_program(ASTs, Env, _Val, NewEnv),
          repl(NewEnv)
      ; writeln('Syntax error.'), repl(Env)
      )
    ).

% ------------------------------
% TOKENIZATION & PARSING (DCG)
% ------------------------------
program([Stmt|Rest]) --> statement(Stmt), whites, ";", whites, program(Rest).
program([Stmt]) --> statement(Stmt).

statement(assign(Var, Expr)) --> identifier(Var), whites, "=", whites, expr(Expr).
statement(Expr) --> expr(Expr).

expr(Expr) --> or_expr(Expr).

or_expr(Expr) --> and_expr(L), whites, "or", whites, or_expr(R), {Expr = binop(or,L,R)}.
or_expr(Expr) --> and_expr(Expr).

and_expr(Expr) --> equality_expr(L), whites, "and", whites, and_expr(R), {Expr = binop(and,L,R)}.
and_expr(Expr) --> equality_expr(Expr).

equality_expr(Expr) --> comparison_expr(L), whites, "==", whites, equality_expr(R), {Expr = binop('==',L,R)}.
equality_expr(Expr) --> comparison_expr(L), whites, "!=", whites, equality_expr(R), {Expr = binop('!=',L,R)}.
equality_expr(Expr) --> comparison_expr(Expr).

comparison_expr(Expr) --> term(L), whites, "<=", whites, comparison_expr(R), {Expr = binop('<=',L,R)}.
comparison_expr(Expr) --> term(L), whites, ">=", whites, comparison_expr(R), {Expr = binop('>=',L,R)}.
comparison_expr(Expr) --> term(L), whites, "<", whites, comparison_expr(R), {Expr = binop('<',L,R)}.
comparison_expr(Expr) --> term(L), whites, ">", whites, comparison_expr(R), {Expr = binop('>',L,R)}.
comparison_expr(Expr) --> term(Expr).

term(Expr) --> factor(L), whites, "+", whites, term(R), {Expr = binop('+',L,R)}.
term(Expr) --> factor(L), whites, "-", whites, term(R), {Expr = binop('-',L,R)}.
term(Expr) --> factor(Expr).

factor(Expr) --> unary(L), whites, "*", whites, factor(R), {Expr = binop('*',L,R)}.
factor(Expr) --> unary(L), whites, "/", whites, factor(R), {Expr = binop('/',L,R)}.
factor(Expr) --> unary(L), whites, "%", whites, factor(R), {Expr = binop('%',L,R)}.
factor(Expr) --> unary(Expr).

unary(Expr) --> "not", whites, unary(E), {Expr = unary(not,E)}.
unary(Expr) --> primary(Expr).

primary(num(N)) --> number(N).
primary(var(X)) --> identifier(X).
primary(Expr) --> "(", whites, expr(Expr), whites, ")".

identifier(Var) --> letter(L), id_chars(Cs), { atom_chars(Var,[L|Cs]) }.
id_chars([C|Cs]) --> alpha_num(C), !, id_chars(Cs).
id_chars([]) --> [].

number(N) --> integer(I), ".", integer(F), { number_chars(NChars,[I,'.'|F]), number_chars(N,NChars) }.
number(N) --> integer(I), { number_chars(NChars,I), number_chars(N,NChars) }.

letter(C) --> [C], { char_type(C, alpha) }.
alpha_num(C) --> [C], { char_type(C, alnum) }.

whites --> [C], { char_type(C, space) }, !, whites.
whites --> [].

% ------------------------------
% EVALUATOR
% ------------------------------
% env is a list of Var=Value pairs

eval_program([], Env, 0, Env).
eval_program([Stmt|Rest], EnvIn, Val, EnvOut) :-
    eval_stmt(Stmt, EnvIn, Val1, Env1),
    eval_program(Rest, Env1, Val, EnvOut).

eval_stmt(num(N), Env, N, Env).
eval_stmt(var(X), Env, Val, Env) :-
    member(X=Val, Env), !.
eval_stmt(assign(X,Expr), Env, Val, NewEnv) :-
    eval_stmt(Expr, Env, Val, Env1),
    update_env(X, Val, Env1, NewEnv).
eval_stmt(unary(not,E), Env, Val, EnvOut) :-
    eval_stmt(E, Env, V, EnvOut),
    (V =:= 0 -> Val = 1 ; Val = 0).
eval_stmt(binop(Op,L,R), Env, Val, EnvOut) :-
    eval_stmt(L, Env, LV, Env1),
    eval_stmt(R, Env1, RV, Env2),
    eval_op(Op, LV, RV, Val),
    EnvOut = Env2.

% binary operators
eval_op('+', A,B,V) :- V is A + B.
eval_op('-', A,B,V) :- V is A - B.
eval_op('*', A,B,V) :- V is A * B.
eval_op('/', A,B,V) :- V is A / B.
eval_op('%', A,B,V) :- V is mod(A,B).
eval_op('==',A,B,V) :- (A =:= B -> V = 1 ; V = 0).
eval_op('!=',A,B,V) :- (A =\= B -> V = 1 ; V = 0).
eval_op('<', A,B,V) :- (A < B -> V = 1 ; V = 0).
eval_op('>', A,B,V) :- (A > B -> V = 1 ; V = 0).
eval_op('<=',A,B,V) :- (A =< B -> V = 1 ; V = 0).
eval_op('>=',A,B,V) :- (A >= B -> V = 1 ; V = 0).
eval_op(and,A,B,V) :- ((A =\= 0, B =\= 0) -> V = 1 ; V = 0).
eval_op(or,A,B,V) :- ((A =\= 0 ; B =\= 0) -> V = 1 ; V = 0).

% update environment
update_env(X, Val, [], [X=Val]).
update_env(X, Val, [X=_|T], [X=Val|T]).
update_env(X, Val, [H|T], [H|T1]) :- H = (K=_), K \= X, update_env(X, Val, T, T1).
