%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: TimeBase_UtcT
%% Source: /build/buildd/erlang-13.b.3-dfsg/lib/cosTime/src/TimeBase.idl
%% IC vsn: 4.2.23
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('TimeBase_UtcT').
-ic_compiled("4_2_23").


-include("TimeBase.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:omg.org/TimeBase/UtcT:1.0","UtcT",
                   [{"time",tk_ulonglong},
                    {"inacclo",tk_ulong},
                    {"inacchi",tk_ushort},
                    {"tdf",tk_short}]}.

%% returns id
id() -> "IDL:omg.org/TimeBase/UtcT:1.0".

%% returns name
name() -> "TimeBase_UtcT".



