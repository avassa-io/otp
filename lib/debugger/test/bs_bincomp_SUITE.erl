%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2007-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
%% Originally based on Per Gustafsson's test suite.
%%

-module(bs_bincomp_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 byte_aligned/1,bit_aligned/1,extended_byte_aligned/1,
	 extended_bit_aligned/1,mixed/1,strict_generators/1,float_skip/1]).

-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [byte_aligned, bit_aligned, extended_byte_aligned,
     extended_bit_aligned, mixed, strict_generators, float_skip].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



byte_aligned(Config) when is_list(Config) ->
    <<"abcdefg">> = << <<(X+32)>> || <<X>> <= <<"ABCDEFG">> >>,
    <<"AxyzBxyzCxyz">> = << <<X, "xyz">> || <<X>> <= <<"ABC">> >>,
    <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	<< <<X:32/little>> || <<X:32>> <= <<1:32,2:32,3:32,4:32>> >>,
    <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	<< <<X:32/little>> || <<X:16>> <= <<1:16,2:16,3:16,4:16>> >>,
    ok.

bit_aligned(Config) when is_list(Config) ->
    <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
	<< <<(X+32):7>> || <<X>> <= <<"ABCDEFG">> >>,
    <<"ABCDEFG">> =
	<< <<(X-32)>> || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> >>,
    <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	<< <<X:31/little>> || <<X:31>> <= <<1:31,2:31,3:31,4:31>> >>,
    <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	<< <<X:31/little>> || <<X:15>> <= <<1:15,2:15,3:15,4:15>> >>,
    ok.

extended_byte_aligned(Config) when is_list(Config) ->
    <<"abcdefg">> = << <<(X+32)>> || X <- "ABCDEFG" >>,
    "abcdefg" = [(X+32) || <<X>> <= <<"ABCDEFG">>],
    <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	<< <<X:32/little>> || X <- [1,2,3,4] >>,
    [256,512,768,1024] =
	[X || <<X:16/little>> <= <<1:16,2:16,3:16,4:16>>],
    ok.

extended_bit_aligned(Config) when is_list(Config) ->
    <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
	<< <<(X+32):7>> || X <- "ABCDEFG" >>,
    "ABCDEFG" = [(X-32) || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>>],
    <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	<< <<X:31/little>> || X <- [1,2,3,4] >>,
    [256,512,768,1024] =
	[X || <<X:15/little>> <= <<1:15,2:15,3:15,4:15>>],
    ok.

mixed(Config) when is_list(Config) ->
    <<2,3,3,4,4,5,5,6>> =
	<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>> >>,
    <<2,3,3,4,4,5,5,6>> =
	<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, Y <- [1,2] >>,
    <<2,3,3,4,4,5,5,6>> =
	<< <<(X+Y)>> || X <- [1,2,3,4], Y <- [1,2] >>,
    [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>>],
    [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X>> <= <<1,2,3,4>>, Y <- [1,2]],
    <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	<< <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>> >>,
    <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	<< <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2] >>,
    <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	<< <<(X+Y):3>> || X <- [1,2,3,4], Y <- [1,2] >>,
    [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>>],
    [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2]],
    ok.

strict_generators(Config) when is_list(Config) ->
    %% Basic strict generators (each generator type)
    <<2,3,4>> = << <<(X+1)>> || X <:- [1,2,3]>>,
    <<2,3,4>> = << <<(X+1)>> || <<X>> <:= <<1,2,3>> >>,
    <<2,12>> = << <<(X*Y)>> || X := Y <:- #{1 => 2, 3 => 4} >>,

    %% A failing guard following a strict generator is ok
    <<3,4>> = << <<(X+1)>> || X <:- [1,2,3], X > 1>>,
    <<3,4>> = << <<(X+1)>> || <<X>> <:= <<1,2,3>>, X > 1 >>,
    <<12>> = << <<(X*Y)>> || X := Y <:- #{1 => 2, 3 => 4}, X > 1 >>,

    %% Non-matching elements cause a badmatch error for strict generators
    {'EXIT',{{badmatch,2},_}} = (catch << <<X>> || {ok, X} <:- [{ok,1},2,{ok,3}] >>),
    {'EXIT',{{badmatch,<<128,2>>},_}} = (catch << <<X>> || <<0:1, X:7>> <:= <<1,128,2>> >>),
    {'EXIT',{{badmatch,{2,error}},_}} = (catch << <<X>> || X := ok <:- #{1 => ok, 2 => error, 3 => ok} >>),

    %% Extra bits cannot be skipped at the end of the binary either
    {'EXIT',{{badmatch,<<0:2>>},_}} = (catch [X || <<X:3>> <:= <<0>>]),
    {'EXIT',{{badmatch,<<9,2>>},_}} = (catch [Y || <<X, Y:X>> <:= <<8,1,9,2>>]),

    ok.

float_skip(Config) when is_list(Config) ->
    BadFloat = <<-1:64>>,
    [1.0,1.5,200.0] = [X || <<X:64/float>> <= <<BadFloat/binary,
                        1:64/float, 1.5:64/float, 200:64/float>>],
    [24.0,+48.5,21.0] =[X || <<X:64/float>> <= <<24:64/float,
                        BadFloat/binary, 48.5:64/float, 21:64/float>>],
    [a,a] =[a || <<0:64/float>> <= <<0:64/float, BadFloat/binary,
                        0:64/float, 1.0:64/float>>].
