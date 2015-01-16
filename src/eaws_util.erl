%% -*- coding: utf-8 -*-
-module(eaws_util).
-export([   calc_sign/2,
            formatted_params/1,
            auth_headers/2,
            smtp_headers/5,
            boundary/0,
            build_multipart_body/6,
            qs/1
  ]).

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

-include("eaws.hrl").

%%%===================================================================
%%% API
%%%===================================================================
build_multipart_body(From, To, Subj, Date, Txt, Attchs) ->
  Bndry = boundary(),
  SmtpH = smtp_headers(From, To, Subj, Date, Bndry),
  SubtTxt = subtype_txt(Txt, Bndry),
  F = fun({FileName, Content}, Acc) ->
            <<Acc/binary, (subtype_attach(to_binary(filename:basename(to_list(FileName))), Content, Bndry, <<"none">>))/binary>>;
          ({FileName, Content, Encoded}, Acc) ->
            <<Acc/binary, (subtype_attach(to_binary(filename:basename(to_list(FileName))), Content, Bndry, Encoded))/binary>>;
          (FileName, Acc) ->
            case file:read_file(to_list(FileName)) of
              {ok, Content} ->
                <<Acc/binary, (subtype_attach(to_binary(filename:basename(to_list(FileName))), Content, Bndry, <<"none">>))/binary>>;
              _Error -> Acc
            end
  end,
  RawMData = <<SmtpH/binary, SubtTxt/binary, (lists:foldl(F, <<>>, Attchs))/binary, (end_boundary(Bndry))/binary>>,
  qs([{<<"Action">>,          <<"SendRawEmail">>},
      {<<"RawMessage.Data">>, base64:encode(RawMData)}
  ]).

%% Boundary
boundary() ->
  << <<"_WebKitBoundary_">>/binary, (unique(32))/binary>>.

end_boundary(Boundary) ->
  <<"\r\n--", Boundary/binary, "--\r\n">>.

unique(Size) -> unique(Size, <<>>).
unique(Size, Acc) when size(Acc) == Size -> Acc;
unique(Size, Acc) ->
  Random = $a + random:uniform($z - $a),
  unique(Size, <<Acc/binary, Random>>).

subtype_txt(Txt, Boundary) ->
  %% Set Email Body Description
  <<"--", Boundary/binary, "\r\n",
  "Content-Type: text/plain; charset=\"UTF-8\"\r\n",
  "Content-Transfer-Encoding: quoted-printable\r\n\r\n",
  (to_utf8(Txt))/binary,
  "\r\n\r\n">>.

subtype_attach(FileName, Content, Boundary, Encoded) ->
  <<
  %% Set Content Description
  "--", Boundary/binary, "\r\n",
  "Content-Type: ", (mime_type(FileName))/binary, "; name=\"", FileName/binary, "\"\r\n",
  "Content-Description: ", FileName/binary, "\r\n",
  "Content-Disposition: attachment; filename=\"", FileName/binary, "\"; size=", (to_binary(byte_size(Content)))/binary, "\r\n",
  "Content-Transfer-Encoding: base64\r\n\r\n",
  %% Set Content
  (b64encode(Encoded, Content))/binary,
  "\r\n"
  >>.

b64encode(<<"base64">>, Content) -> Content;
b64encode(_, Content) -> base64:encode(Content).

gen_message_id() ->
  <<"<", (unique(8))/binary,
    "-", (unique(4))/binary,
    "-", (unique(4))/binary,
    "-", (unique(4))/binary,
    "-", (unique(12))/binary,
    "@amazon.com>">>.

calc_sign(AccessId, SecretKey) ->
    {Today, Time} = erlang:universaltime(),
    {{Year, Month, Day}, {Hour, Minute, Second}} = {Today, Time},
    Day_Name = httpd_util:day(calendar:day_of_the_week(Today)),
    Month_Name = httpd_util:month(Month),
    Date = list_to_binary(lists:flatten(io_lib:format("~s, ~p ~s ~p ~2.10.0B:~2.10.0B:~2.10.0B +0000",
        [Day_Name, Day, Month_Name, Year, Hour, Minute, Second]))
    ),
    Signature = base64:encode(crypto:hmac(sha, SecretKey, Date)),
    Auth = <<"AWS3-HTTPS AWSAccessKeyId=", AccessId/binary, ", Algorithm=HMACSHA1, Signature=", Signature/binary >>,
    {Date, Auth}.

%% create post params string
qs(Params) ->
    qs(Params, <<>>).

qs([], Acc) -> Acc;
qs([{K, V}|[]], Acc) ->
    V1 = urlencode(V), 
    <<Acc/binary, "&", K/binary, "=", V1/binary>>;
qs([{K,V}|T], <<>>) ->
    V1 = urlencode(V),
    qs(T, <<K/binary, "=", V1/binary>>);
qs([{K, V}|T], Acc) ->
    V1 = urlencode(V), 
    qs(T, <<Acc/binary, "&", K/binary, "=", V1/binary >>).

formatted_params(Par) ->
  From = proplists:get_value(<<"from">>, Par, <<>>),
  Subj = proplists:get_value(<<"subject">>, Par, <<>>),
  Txt = proplists:get_value(<<"txt">>, Par, <<>>),
  Html = proplists:get_value(<<"html">>, Par, <<>>),
  To = proplists:get_value(<<"to">>, Par, <<>>),
  [ {<<"Action">>,                          <<"SendEmail">>},
    {<<"Source">>,                          From},
    {<<"Message.Subject.Data">>,            Subj},
    {<<"Message.Body.Text.Data">>,          Txt},
    {<<"Message.Body.Html.Data">>,          Html},
    {<<"Destination.ToAddresses.member.1">>,To}
  ].

auth_headers(Auth, Date) ->
  [ {<<"X-Amzn-Authorization">>, Auth},
    {<<"Date">>, Date}
  ].

smtp_headers(From, To, Subject, Date, PartSep) ->
  <<
  "From: ", From/binary, "\r\n",
  "To: ", To/binary, "\r\n",
  "Date: ", Date/binary, "\r\n",
  "Subject: ", Subject/binary, "\r\n",
  "Message-ID: ", (gen_message_id())/binary, "\r\n",
  "Accept-Language: en-US\r\n",
  "Content-Language: en-US\r\n",
  "Content-Type: multipart/mixed; boundary=\"", PartSep/binary, "\"\r\n",
  "MIME-Version: 1.0", "\r\n\r\n"
  >>.

%% urlencode 
urlencode(Bin) ->
        urlencode(Bin, []).

urlencode(Bin, Opts) ->
        Plus = not lists:member(noplus, Opts),
        Upper = lists:member(upper, Opts),
        urlencode(Bin, <<>>, Plus, Upper).

urlencode(<<C, Rest/binary>>, Acc, P=Plus, U=Upper) ->
        if      C >= $0, C =< $9 -> urlencode(Rest, <<Acc/binary, C>>, P, U);
                C >= $A, C =< $Z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
                C >= $a, C =< $z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
                C =:= $.; C =:= $-; C =:= $~; C =:= $_ ->
                urlencode(Rest, <<Acc/binary, C>>, P, U);
                C =:= $ , Plus ->
                urlencode(Rest, <<Acc/binary, $+>>, P, U);
                true ->
                H = C band 16#F0 bsr 4, L = C band 16#0F,
                H1 = if Upper -> tohexu(H); true -> tohexl(H) end,
                L1 = if Upper -> tohexu(L); true -> tohexl(L) end,
                urlencode(Rest, <<Acc/binary, $%, H1, L1>>, P, U)
        end;
urlencode(<<>>, Acc, _Plus, _Upper) ->
        Acc.

tohexu(C) when C < 10 -> $0 + C;
tohexu(C) when C < 17 -> $A + C - 10.

tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 17 -> $a + C - 10.

%% @Todo More
mime_type(FileName) ->
  case filename:extension(to_list(FileName)) of
    Ext when Ext =:= ".pdf" -> <<"application/pdf">>;
    Ext when Ext =:= ".jpeg" -> <<"image/jpeg">>;
    Ext when Ext =:= ".jpg" -> <<"image/jpg">>;
    Ext when Ext =:= ".gif" -> <<"image/gif">>;
    Ext when Ext =:= ".png" -> <<"image/png">>;
    _ -> <<"text/plain">>
  end.

to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_integer(X) -> integer_to_binary(X).

to_list(X) when is_list(X) -> X;
to_list(X) when is_binary(X) -> binary_to_list(X).

%% Unicode lib
%% author detect unicode S.Kostyshkin
is_unicode_string(BinaryString) when is_binary(BinaryString) ->
  Latin1List = binary_to_list(BinaryString),
  UTF8List = unicode:characters_to_list(BinaryString),
  Latin1List =/= UTF8List;
is_unicode_string(String) when is_list(String) ->
  is_unicode_string(list_to_binary(String)).

to_utf8(Txt) ->
  case is_unicode_string(Txt) of
    true ->
      unicode:characters_to_binary(unicode:characters_to_list(Txt));
    false -> Txt
  end.

