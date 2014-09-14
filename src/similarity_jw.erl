%% @author Rado Kozmer
%% @doc Implementation of the Jaro-Winkler similarity algorithm.

-module(similarity_jw).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([proximity/2, distance/2]).

-define(NUM_CHARS, 4).

proximity(S1, S1) -> 1.0;

proximity("", _S2) -> 0.0;

proximity(_S1, "") -> 0.0;

proximity(S1T, S2T) ->	
	{S1, S2, Len1, Len2} = setup(S1T, S2T),
	MaxDist = trunc(Len1 / 2),
	{_PrevPos, CommonChars, Transpositions} = find_matching_chars(0, Len1, Len2, S1, S2, MaxDist, -1, 0, 0),
	if (CommonChars == 0) ->
		   0.0;
	   true ->
		   Score = (CommonChars/Len1 + CommonChars/Len2 + (CommonChars-Transpositions) / CommonChars) / 3.0,
		   PrefixLen = get_prefix_len(0, min(?NUM_CHARS, Len1), S1, S2),
		   Score + (PrefixLen * (1 - Score)) / 10
	end.	

distance(S1, S2) ->
	1.0 - proximity(S1, S2).

%% ====================================================================
%% Internal functions
%% ====================================================================

find_matching_chars(_L1, _L1, _L2, _S1, _S2, _MaxDist, PrevPos, C, T) ->
	{PrevPos, C, T};

find_matching_chars(I, L1, L2, [S1I|S1R], S2, MaxDist, PrevPos, C, T) ->
	{PrevPosNew, CNew, TNew} = compare_chars(max(0, I - MaxDist), min(L2, I + MaxDist), S1I, S2, PrevPos, C, T),
	find_matching_chars(I+1, L1, L2, S1R, S2, MaxDist, PrevPosNew, CNew, TNew).

compare_chars(_Max, _Max, _S1I, _S2, PrevPos, C, T) ->
	{PrevPos, C, T};

compare_chars(J, Max, S1I, [S2J|S2R], PrevPos, C, T) ->
	if (S1I == S2J) ->
		if (PrevPos /= -1) and (J < PrevPos) ->
			TNew = T + 1;
		true ->
			TNew = T
		end,
		{J, C+1, TNew};
	true ->
		compare_chars(J+1, Max, S1I, S2R, PrevPos, C, T)
	end.

setup(S1, S2) -> 
	L1 = length(S1),
	L2 = string:len(S2),
	if (L1 > L2) ->
		{S2, S1, L2, L1};
	true ->
		{S1, S2, L1, L2}
	end.

get_prefix_len(Last, Last, _S1, _S2) ->
	Last;

get_prefix_len(P, Last, [S1P|S1R], [S1P|S2R]) ->
	get_prefix_len(P+1, Last, S1R, S2R);
	
get_prefix_len(P, _Last, _S1, _S2) ->
	P.
