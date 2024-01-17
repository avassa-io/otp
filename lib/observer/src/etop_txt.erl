%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2022. All Rights Reserved.
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
-module(etop_txt).
-author('siri@erix.ericsson.se').

%%-compile(export_all).
-export([init/1,stop/1]).
-export([do_update/4]).

-include("etop.hrl").
-include("etop_defs.hrl").

-define(DEFAULT_WIDTH, 89).

-record(field_widths, {cols      :: pos_integer(),
                       used_cols :: pos_integer(),
                       init_func :: pos_integer(),
                       reds      :: pos_integer(),
                       mem       :: pos_integer(),
                       msgq      :: pos_integer(),
                       curr_func :: pos_integer()}).

-import(etop,[loadinfo/2,meminfo/2]).

stop(Pid) -> Pid ! stop.

init(Config) ->
    loop(#etop_info{},Config).

loop(Prev,Config) ->
    Info = do_update(Prev,Config),
    receive 
	stop -> stopped;
	{dump,Fd} -> _ = do_update(Fd,Info,Prev,Config), loop(Info,Config);
	{config,_,Config1} -> loop(Info,Config1)
    after Config#opts.intv -> loop(Info,Config)
    end.

do_update(Prev,Config) ->
    Info = etop:update(Config),
    do_update(standard_io,Info,Prev,Config).

do_update(Fd,Info,Prev,Config) ->
    {Cpu,NProcs,RQ,Clock} = loadinfo(Info,Prev),
    HR = Config#opts.human_readable,
    FieldWidths = calc_field_widths(Info#etop_info.procinfo, HR),
    io:nl(Fd),
    writedoubleline(Fd, FieldWidths),
    case Info#etop_info.memi of
	undefined ->
	    io:fwrite(Fd, " ~-72w~10s~n"
		      " Load:  cpu  ~8w~n"
		      "        procs~8w~n"
		      "        runq ~8w~n",
		      [Config#opts.node,Clock,
		       Cpu,NProcs,RQ]);
	Memi ->
	    [Tot,Procs,Atom,Bin,Code,Ets] = 
		meminfo(Memi, [total,processes,atom,binary,code,ets]),
	    io:fwrite(Fd, ?SYSFORM,
		      [Config#opts.node,Clock,
		       Cpu,fmt_bytes(HR, Tot), fmt_bytes(HR, Bin),
		       NProcs,fmt_bytes(HR, Procs), fmt_bytes(HR, Code),
		       RQ,fmt_bytes(HR, Atom), fmt_bytes(HR, Ets)])
    end,
    io:nl(Fd),
    writepinfo_header(Fd, FieldWidths),
    writesingleline(Fd, FieldWidths),
    writepinfo(Fd, Info#etop_info.procinfo, modifier(Fd), HR, FieldWidths),
    writedoubleline(Fd, FieldWidths),
    io:nl(Fd),
    Info.


calc_field_widths(ProcInfoL, HR) ->
    Cols = case io:columns() of
               {ok, IoCols}     -> max(IoCols, ?DEFAULT_WIDTH);
               {error, enotsup} -> ?DEFAULT_WIDTH
           end,

    %% There is a priority with the field calculation since it's done
    %% sequential with all fields deducted with their default values from
    %% start. If any field eventually exceed the column budget, the rest of the
    %% columns will get their default widths.

    %% Calculate columns left. NOTE: The extra spaces between reds/mem/msgq.
    %% Subsequent calculations adds default width then deducts calculated width.
    %% See proc_format/2 for fields format.
    %% Columns: pid, init_func, time, reds, mem, msgq, curr_func.
    ColsLeft0 = Cols - 15 - 20 - 8 - 8 - 1 - 8 - 1 - 8 - 20,

    RedsWidth = get_width(reds, false, ProcInfoL, ColsLeft0),

    ColsLeft1 = ColsLeft0 + 8 - RedsWidth,

    MemWidth = get_width(mem, HR, ProcInfoL, ColsLeft1),

    ColsLeft2 = ColsLeft1 + 8 - MemWidth,

    MsgQWidth= get_width(msgq, false, ProcInfoL, ColsLeft2),

    ColsLeft3 = ColsLeft2 + 8 - MsgQWidth,

    %% Use the rest for initial and current function fields
    if ColsLeft3 > 0 ->
           %% compensate field start width for rounding
           FieldSize = 19 + round((ColsLeft3 - 1) / 2),
           InitFuncWidth = FieldSize,
           CurrFuncWidth = FieldSize;
       true ->
           InitFuncWidth = 20,
           CurrFuncWidth = 20
    end,

    %% Extra space between reds/mem, extra space due to rounding
    UsedCols =
        15 + InitFuncWidth + 8 + RedsWidth + 1 + MemWidth + 1
        + MsgQWidth + CurrFuncWidth + 1,

    #field_widths{cols = Cols,
                  used_cols = UsedCols,
                  init_func = InitFuncWidth,
                  reds = RedsWidth,
                  mem = MemWidth,
                  msgq = MsgQWidth,
                  curr_func = CurrFuncWidth}.


get_width(reds, HR, ProcInfoL, ColsLeft) ->
    get_width(4, HR, ProcInfoL, ColsLeft);
get_width(mem, HR, ProcInfoL, ColsLeft) ->
    get_width(3, HR, ProcInfoL, ColsLeft);
get_width(msgq, HR, ProcInfoL, ColsLeft) ->
    get_width(8, HR, ProcInfoL, ColsLeft);

%% Get the maximum width of the field at place N in #sysinfo{}.
%% Calculate the maximum width by taking the maximum width (by taking common
%% logarithm of largest number) then check if is larger than 8 and that it
%% fits the column budget, if so return that value; otherwise return 8.
get_width(_, true, _ProcInfoL, _ColsLeft) ->
    11;
get_width(N, false, ProcInfoL, ColsLeft) ->
    MaxNum = lists:foldr(fun(Info, Acc)
                               when element(N, Info) > Acc ->
                                 element(N, Info);
                            (_, Acc) ->
                                 Acc
                         end,
                         0,
                         ProcInfoL),

    MaxWidth =
        if MaxNum > 0 -> round(math:log10(MaxNum)) + 1;
           true       -> 1  %% logarithm defined in R_n, n > 0.
        end,

    if MaxWidth > 8 andalso ColsLeft - MaxWidth > 0 ->
           MaxWidth;
       true ->
           8
    end.


writepinfo_header(Fd, #field_widths{init_func = InitFunc, reds = Reds,
                                    mem = Mem, msgq = MsgQ}) ->
    %% Add spaces between variable width columns.
    Header =
        "Pid            Name or Initial Func"
        ++ lists:duplicate(max(InitFunc - 16, 4), $\s) ++
        "Time"
        ++ lists:duplicate(max(Reds - 4, 4), $\s) ++
        "Reds"
        ++ lists:duplicate(max(Mem - 5, 3), $\s) ++
        "Memory"
        ++ lists:duplicate(max(MsgQ - 3, 5), $\s) ++
        "MsgQ Current Function\n",

    io:fwrite(Fd, Header, []).

writesingleline(Fd, FieldWidths) -> writedupline(Fd, $-, FieldWidths).
writedoubleline(Fd, FieldWidths) -> writedupline(Fd, $=, FieldWidths).

writedupline(Fd, Char, #field_widths{used_cols = UsedCols}) ->
    Line = lists:duplicate(UsedCols, Char) ++ "\n",
    io:fwrite(Fd, Line, []).

writepinfo(Fd,[#etop_proc_info{pid=Pid,
			       mem=Mem,
			       reds=Reds,
			       name=Name,
			       runtime=Time,
			       cf=MFA,
			       mq=MQ}
	       |T],
           Modifier, HR, FieldWidths) ->
    io:fwrite(Fd,proc_format(Modifier, FieldWidths),
              [Pid,to_string(Name,Modifier),Time,Reds,fmt_bytes(HR, Mem),MQ,
               to_string(MFA,Modifier)]),
    writepinfo(Fd,T,Modifier, HR, FieldWidths);
writepinfo(_Fd,[],_,_,_) ->
    ok.

proc_format(Modifier, #field_widths{init_func = InitFunc, reds = Reds,
                                    mem = Mem, msgq = MsgQ,
                                    curr_func = CurrFunc}) ->
    "~-15w"
    "~-" ++ i2l(InitFunc) ++ Modifier ++ "s"
    "~8w"
    "~" ++ i2l(Reds) ++ "w "
    "~" ++ i2l(Mem) ++"s "
    "~" ++ i2l(MsgQ) ++ "w "
    "~-" ++ i2l(CurrFunc) ++ Modifier ++ "s~n".


to_string({M,F,A},Modifier) ->
    io_lib:format("~w:~"++Modifier++"w/~w",[M,F,A]);
to_string(Other,Modifier) ->
    io_lib:format("~"++Modifier++"w",[Other]).

i2l(I) -> integer_to_list(I).

modifier(Device) ->
    case encoding(Device) of
        latin1 -> "";
        _ -> "t"
    end.

encoding(Device) ->
    case io:getopts(Device) of
        List when is_list(List) ->
            proplists:get_value(encoding,List,latin1);
        _ ->
            latin1
    end.

-define(Ki, 1024).
-define(Mi, (1024*?Ki)).
-define(Gi, (1024*?Mi)).
-define(Ti, (1024*?Gi)).


fmt_bytes(false, B) ->
    integer_to_binary(B);
fmt_bytes(true, B) ->
    fmt_bytes(B).

fmt_bytes(B) ->
    fmt_bytes1(B, 2).
fmt_bytes1(B, FractionDigits) ->
    if B < ?Ki ->
            [integer_to_binary(B), <<" B  ">>];
       B < ?Mi ->
            [fmt_decimal(B / ?Ki, FractionDigits), " KiB"];
       B < ?Gi ->
            [fmt_decimal(B / ?Mi, FractionDigits), " MiB"];
       B < ?Ti ->
            [fmt_decimal(B / ?Gi, FractionDigits), " GiB"];
       true ->
            [fmt_decimal(B / ?Ti, FractionDigits), " TiB"]
    end.

fmt_decimal(D, FractionDigits) ->
    io_lib:format("~.*f", [FractionDigits, D]).
