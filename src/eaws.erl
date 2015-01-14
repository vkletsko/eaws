-module(eaws).

-author('Vitali Kletsko <v.kletsko@gmail.com>').

%%================================
%% API
%%================================
-export([send_formatted/1, send_raw/1]).

-export([start/0, stop/0]).

-include("eaws.hrl").

start() ->
    F = fun({App, _, _}) -> App end,
    RunningApps = lists:map(F, application:which_applications()),
    LoadedApps = lists:map(F, application:loaded_applications()),
    case lists:member(?MODULE, LoadedApps) of
        true -> true;
        false -> ok = application:load(?MODULE)
    end,
    {ok, Apps} = application:get_key(?MODULE, applications),
    [application:start(A)|| A <- Apps ++ [?MODULE], not lists:member(A, RunningApps)],
    ok.
            
stop() ->
    application:stop(?MODULE).

%%================================
%% API
%%================================
send_formatted(Par) ->
  {_, AccessKey} = lists:keyfind(<<"access_key">>, 1, Par),
  {_, AccessId} = lists:keyfind(<<"access_id">>, 1, Par),
  EndP = proplists:get_value(<<"end_point">>, Par),
  {Date, Auth}= eaws_util:calc_sign(AccessId, AccessKey),
  Body = eaws_util:qs(eaws_util:formatted_params(Par)),
  AuthHdrs = eaws_util:auth_headers(Auth, Date),
  eaws_http:req(?Detect_EndPoint(EndP), AuthHdrs, ?CONT_TYPE_XWWW, Body).

send_raw(Par) ->
  {_, SecretKey} = lists:keyfind(<<"access_key">>, 1, Par),
  {_, AccessId} = lists:keyfind(<<"access_id">>, 1, Par),
  {_, From} = lists:keyfind(<<"from">>, 1, Par),
  {_, To} = lists:keyfind(<<"to">>, 1, Par),
  Subj = proplists:get_value(<<"subject">>, Par, <<>>),
  Txt = proplists:get_value(<<"txt">>, Par, <<>>),
  Attchs = proplists:get_value(<<"attachments">>, Par, []),
  EndP = proplists:get_value(<<"end_point">>, Par),
  {Date, Auth}= eaws_util:calc_sign(AccessId, SecretKey),
  AuthHdrs = eaws_util:auth_headers(Auth, Date),
  MultPrtBody = eaws_util:build_multipart_body(From, To, Subj, Date, Txt, Attchs),
  eaws_http:req(?Detect_EndPoint(EndP), AuthHdrs, ?CONT_TYPE_XWWW, MultPrtBody).




