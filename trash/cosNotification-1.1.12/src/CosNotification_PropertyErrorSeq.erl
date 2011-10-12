%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNotification_PropertyErrorSeq
%% Source: /build/buildd/erlang-13.b.3-dfsg/lib/cosNotification/src/CosNotification.idl
%% IC vsn: 4.2.23
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNotification_PropertyErrorSeq').
-ic_compiled("4_2_23").


-include("CosNotification.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_sequence,
            {tk_struct,"IDL:omg.org/CosNotification/PropertyError:1.0",
                "PropertyError",
                [{"code",
                  {tk_enum,"IDL:omg.org/CosNotification/QoSError_code:1.0",
                      "QoSError_code",
                      ["UNSUPPORTED_PROPERTY","UNAVAILABLE_PROPERTY",
                       "UNSUPPORTED_VALUE","UNAVAILABLE_VALUE","BAD_PROPERTY",
                       "BAD_TYPE","BAD_VALUE"]}},
                 {"name",{tk_string,0}},
                 {"available_range",
                  {tk_struct,"IDL:omg.org/CosNotification/PropertyRange:1.0",
                      "PropertyRange",
                      [{"low_val",tk_any},{"high_val",tk_any}]}}]},
            0}.

%% returns id
id() -> "IDL:omg.org/CosNotification/PropertyErrorSeq:1.0".

%% returns name
name() -> "CosNotification_PropertyErrorSeq".



