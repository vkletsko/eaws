-module(eaws_http).

-export([req/4]).

-define(HEADER, [{"connection", "close"}]).

-include("eaws.hrl").

req(Url, Headers, ContType, Body) ->
  HttpClOpts = [{sync, true},{body_format,binary}],
  Resp = httpc:request(post, {Url, to_list_headers(Headers), ContType, Body}, [{timeout, 60000}], HttpClOpts),
  minimize_resp(Resp).

minimize_resp(Resp) ->
  case Resp of
    {ok, {{_NewVrsn, 200, _}, _Headers, RespBody}} ->
      {ok, RespBody};
    {ok, {{_NewVrsn, _HttpCode, _}, _Headers, RespBody}} ->
      {error, RespBody};
    Any -> Any
  end.

to_list(X) when is_list(X) -> X;
to_list(X) when is_binary(X) -> binary_to_list(X).

to_list_headers(L) ->
  [{to_list(K), to_list(V)}  || {K, V} <- L].

