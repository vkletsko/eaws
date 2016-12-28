%% -*- coding: utf-8 -*-
-module(eaws_util).
-export([   calc_sign/2,
            formatted_params/1,
            build_multipart_body/2,
            to_list/1
  ]).

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

%%%===================================================================
%%% API
%%%===================================================================
-spec formatted_params(Par :: map()) ->
  {ok, binary()}.
formatted_params(#{ <<"from">>    := From,
                    <<"html">>    := Html,
                    <<"to">>      := To} = Par) ->
  Txt = maps:get(<<"txt">>, Par, <<>>),
  Subj = maps:get(<<"subject">>, Par, <<>>),
  qs(#{ <<"Action">> => <<"SendEmail">>,
        <<"Source">> => From,
        <<"Message.Subject.Data">> => Subj,
        <<"Message.Body.Text.Data">> => Txt,
        <<"Message.Body.Html.Data">> => Html,
        <<"Destination.ToAddresses.member.1">> => To
  }).

-spec build_multipart_body(Date :: binary(), Par :: map()) ->
  {ok, binary()}.
build_multipart_body(Date, #{ <<"from">>    := From,
                              <<"to">>      := To,
                              <<"attachments">> := Attchs} = Par) ->
  Boundary = boundary(),
  Txt = maps:get(<<"txt">>, Par, <<>>),
  Subj = maps:get(<<"subject">>, Par, <<>>),
  SmtpHeader = smtp_headers(From, To, Subj, Date, Boundary),
  SubtTxt = subtype_txt(Txt, Boundary),
  {_Boundary, AttachmentsPack} = lists:foldl(fun build_attach_package/2, {Boundary, <<>>}, Attchs),
  RawMData = base64:encode(<<SmtpHeader/binary, SubtTxt/binary, AttachmentsPack/binary, (boundary(ending, Boundary))/binary>>),
  qs(#{ <<"Action">> => <<"SendRawEmail">>,
        <<"RawMessage.Data">> => RawMData
  }).

-spec calc_sign(binary(), binary()) ->
  {Date :: binary(), Headers :: list()}.
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
  AuthHeaders = [
    {<<"X-Amzn-Authorization">>, Auth},
    {<<"Date">>, Date}
  ],
  {Date, AuthHeaders}.

%% Utils
build_attach_package({FileName, Content}, {Boundary, Acc}) when is_binary(Content) ->
  FileName1 = to_binary_filename(filename:basename(FileName)),
  {Boundary, <<Acc/binary, (subtype_attach(FileName1, Content, Boundary, <<"none">>))/binary>>};
build_attach_package({FileName, Content, Encoded}, {Boundary, Acc}) when is_binary(Content) ->
  FileName1 = to_binary_filename(filename:basename(FileName)),
  {Boundary, <<Acc/binary, (subtype_attach(FileName1, Content, Boundary, Encoded))/binary>>};
build_attach_package(FileName, {Boundary, Acc}) ->
  FileName1 = to_binary_filename(filename:basename(FileName)),
  case file:read_file(FileName) of
    {ok, Content} ->
      {Boundary, <<Acc/binary, (subtype_attach(FileName1, Content, Boundary, <<"none">>))/binary>>};
    _Error ->
      {Boundary, Acc}
  end.

boundary() ->
  << <<"_WebKitBoundary_">>/binary, (unique(32))/binary>>.

boundary(ending, Boundary) ->
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
  "Content-Disposition: attachment; filename=\"", FileName/binary, "\"; size=", (integer_to_binary(byte_size(Content)))/binary, "\r\n",
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

qs(Params) ->
  {ok, maps:fold(fun bin_kv/3, <<>>,Params)}.

bin_kv(K, V, <<>>) ->
  <<K/binary, "=", (urlencode(V))/binary>>;
bin_kv(K, V, AccIn) ->
  <<AccIn/binary, "&", K/binary, "=", (urlencode(V))/binary >>.

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

urlencode(<<C, Rest/binary>>, Acc, Plus, Upper) ->
  if  C >= $0, C =< $9 -> urlencode(Rest, <<Acc/binary, C>>, Plus, Upper);
      C >= $A, C =< $Z -> urlencode(Rest, <<Acc/binary, C>>, Plus, Upper);
      C >= $a, C =< $z -> urlencode(Rest, <<Acc/binary, C>>, Plus, Upper);
      C =:= $.; C =:= $-; C =:= $~; C =:= $_ ->
        urlencode(Rest, <<Acc/binary, C>>, Plus, Upper);
      C =:= $ , Plus ->
        urlencode(Rest, <<Acc/binary, $+>>, Plus, Upper);
      true ->
        H = C band 16#F0 bsr 4, L = C band 16#0F,
        H1 = tohexu(H),
        L1 = tohexu(L),
        urlencode(Rest, <<Acc/binary, $%, H1, L1>>, Plus, Upper)
  end;
urlencode(<<>>, Acc, _Plus, _Upper) ->
  Acc.

tohexu(C) when C < 10 -> $0 + C;
tohexu(C) when C < 17 -> $A + C - 10.

%%tohexl(C) when C < 10 -> $0 + C;
%%tohexl(C) when C < 17 -> $a + C - 10.

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

-spec to_list(list() | binary()) -> list().
to_list(X) when is_list(X) -> X;
to_list(X) when is_binary(X) -> binary_to_list(X).

to_binary_filename(FName) when is_binary(FName) -> FName;
to_binary_filename(FName) when is_list(FName) -> list_to_binary(FName).

%% unicode detection
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

