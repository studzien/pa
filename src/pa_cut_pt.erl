-module(pa_cut_pt).

-export([parse_transform/2]).

-ifndef(max_partial_arity).
-define(max_partial_arity, 10).
-endif.

-define(FUNCTION, cut).

parse_transform(Forms, _Opts) ->
    module(Forms, []).

module([{eof, _}=Eof], Acc) ->
    lists:reverse(Acc) ++ functions(?max_partial_arity) ++ [Eof];
module([{attribute, _, module, _}=Module|Tail], Acc) ->
    module(Tail, [exports(?max_partial_arity), Module | Acc]);
module([Head|Tail], Acc) ->
    module(Tail, [Head|Acc]).

exports(MaxArity) ->
    {attribute, 0, export,
     [{?FUNCTION, Arity+1} || Arity <- lists:seq(1, MaxArity)]}.

functions(MaxArity) ->
    [ function(Arity) || Arity <- lists:seq(1, MaxArity) ].

function(Arity) ->
    {function, 0, ?FUNCTION, Arity+1,
     [badarg_clause(Arity)|clauses(Arity)]}.

clauses(Arity) ->
    [ clause(Head, Args, Arity) || {Head, Args} <- head_args(Arity) ].

badarg_clause(Arity) ->
    {clause, 0,
     clause_header([ var('_') || _ <- lists:seq(1, Arity) ]),
     [[{op, 0, 'not',
        {call, 0, atom(is_function), [var('Fun'), {integer, 0, Arity}]}}]],
     [{call, 0,
       {remote, 0, atom(erlang), atom(error)}, [atom(badarg)]}]}.

clause(Head, Args, Arity) ->
    {clause, 0,
     clause_header(Head),
     [],
     clause_body(Args, Arity)}.

fun_args(Arity) ->
    [ var(arg(N)) || N <- lists:seq(1, Arity) ].

clause_header(Head) ->
    [var('Fun') | Head].

clause_body(Args, Arity) ->
    [{'fun', 0,
      {clauses,
       [{clause, 0,
         Args,
         [],
         [{call, 0,
           {var, 0, 'Fun'},
           fun_args(Arity)}]}]}}].

args(1) ->
    [ ['_'], ['A'] ];
args(Arity) ->
    BaseArgs = args(Arity-1),
    [ ['_' | Args] || Args <- BaseArgs ] ++
    [ ['A' | Args] || Args <- BaseArgs ].

head_args(Arity) ->
    [ head_args(Args, 1, [], []) || Args <- args(Arity) ].

head_args([], _, HeadAcc, ArgsAcc) ->
    {lists:reverse(HeadAcc), lists:reverse(ArgsAcc)};
head_args(['_'|Tail], N, HeadAcc, ArgsAcc) ->
    head_args(Tail, N+1, [atom('_')|HeadAcc], [var(arg(N))|ArgsAcc]);
head_args(['A'|Tail], N, HeadAcc, ArgsAcc) ->
    head_args(Tail, N+1, [var(arg(N))|HeadAcc], ArgsAcc).

arg(N) ->
    list_to_atom("A" ++ integer_to_list(N)).

atom(Atom) ->
    {atom, 0, Atom}.

var(Var) ->
    {var, 0, Var}.
