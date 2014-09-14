%% @author Rado Kozmer
%% @doc Unit tests for similarity_jw.

-module(similarity_jw_test).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================
-export([]).

proximity_exact_match_test() ->
	S1 = "Peter Black",
	?assert(1.0 =:= similarity_jw:proximity(S1, S1)).

proximity_both_strings_empty_test() ->
	?assert(1.0 =:= similarity_jw:proximity("", "")).

proximity_first_string_empty_test() ->
	?assert(0.0 =:= similarity_jw:proximity("", "Peter Black")).

proximity_second_string_empty_test() ->
	?assert(0.0 =:= similarity_jw:proximity("Peter Black", "")).

proximity_partial_match_test() ->
	?assert(0.8153679653679654 =:= similarity_jw:proximity("Peter Black", "Peter Müller")).
