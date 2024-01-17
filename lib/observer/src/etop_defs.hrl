%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-define(SYSFORM,
	" ~-72w~10s~n"
	" Load:  cpu  ~8w               Memory:  total    ~11s    binary   ~11s~n"
	"        procs~8w                        processes~11s    code     ~11s~n"
	"        runq ~8w                        atom     ~11s    ets      ~11s~n").

-record(opts, {node=node(), port = 8415, accum = false, intv = 5000, lines = 10,
	       width = 700, height = 340, sort = runtime, tracing = on,
               human_readable = false,
	       %% Other state information
	       out_mod=etop_txt, out_proc, server, host, tracer, store,
	       accum_tab, remote}).
