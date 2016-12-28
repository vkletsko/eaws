-module(eaws).

-author('Vitali Kletsko <v.kletsko@gmail.com>').

%% avialable aws endpoints
-define(US_East_EndPoint, "https://email.us-east-1.amazonaws.com/").
-define(US_West_EndPoint, "https://email.us-west-2.amazonaws.com/").
-define(EU_EndPoint,      "https://email.eu-west-1.amazonaws.com/").

-define(XWWW_CONT_TYPE, "application/x-www-form-urlencoded").

-define(TIMEOUT, 60000).
-define(HEADER, [{"connection", "close"}]).


-export([send_formatted/1, send_raw/1]).
%% FOR INTERNAL USAGE
-export([start/0, stop/0]).

%%================================
%% API
%%================================
-spec send_formatted(list() | map()) ->
  {ok, binary()} | {error, atom()}.
send_formatted(Par) when is_list(Par) ->
  send_formatted(maps:from_list(Par));

send_formatted(#{ <<"access_id">>     := AccessId,
                  <<"access_key">>    := AccessKey} = Par) ->
  AwsURL = maps:get(<<"aws_url">>, Par, ?EU_EndPoint),
  {_Date, AuthHeaders} = eaws_util:calc_sign(AccessId, AccessKey),
  {ok, QsBody} = eaws_util:formatted_params(Par),
  Result = http_req(AwsURL, AuthHeaders, QsBody),
  short_resp(Result).

-spec send_raw(list() | map()) ->
  {ok, binary()} | {error, atom()}.
send_raw(Par) when is_list(Par) ->
  send_raw(maps:from_list(Par));

send_raw(#{ <<"access_id">>     := AccessId,
            <<"access_key">>    := AccessKey} = Par) ->
  AwsURL = maps:get(<<"aws_url">>, Par, ?EU_EndPoint),
  {Date, AuthHeaders}= eaws_util:calc_sign(AccessId, AccessKey),
  {ok, MultiPartBody} = eaws_util:build_multipart_body(Date, Par),
  Result = http_req(AwsURL, AuthHeaders, MultiPartBody),
  short_resp(Result).

%% FOR INTERNAL USAGE
http_req(Url, Headers, Body) ->
  HttpClOpts = [{sync, true},{body_format,binary}],
  Request = {check_url(Url), to_list_headers(Headers), ?XWWW_CONT_TYPE, Body},
  httpc:request(post, Request, [{timeout, ?TIMEOUT}], HttpClOpts).

short_resp({ok, {{_NewVrsn, 200, _}, _Headers, RespBody}}) ->
  {ok, RespBody};
short_resp({ok, {{_NewVrsn, _HttpCode, _}, _Headers, RespBody}}) ->
  {error, RespBody};
short_resp(AnyError) ->
  AnyError.

check_url(Url) when is_binary(Url) ->
  eaws_util:to_list(Url);
check_url([]) -> %% set default EU_Endpoint
  ?EU_EndPoint;
check_url(Url) when is_list(Url) -> Url.

to_list_headers(L) ->
  [{eaws_util:to_list(K), eaws_util:to_list(V)} || {K, V} <- L].

%% stop start apps
-spec start() -> ok.
start() ->
  F = fun({App, _, _}) -> App end,
  RunningApps = lists:map(F, application:which_applications()),
  LoadedApps = lists:map(F, application:loaded_applications()),
  case lists:member(?MODULE, LoadedApps) of
    true -> true;
    false -> ok = application:load(?MODULE)
  end,
  {ok, Apps} = application:get_key(?MODULE, applications),
  Fun = fun (A) ->
          case not lists:member(A, RunningApps) of
            true ->
              _Ignore = application:start(A);
            false -> ok
          end
        end,
  ok = lists:foreach(Fun, Apps ++ [?MODULE]).

-spec stop() -> ok.
stop() ->
  application:stop(?MODULE).


%%================================
%% Tests
%%================================
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

%% @Todo

-endif.

