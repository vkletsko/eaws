Eaws
======

Eaws is a small Email sender written in Erlang.

Link Doc API: http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-api.html

Usage:
--------------------
Sending Formatted Email (Include Html)
--------

```
Par = [
    {<<"access_key">>, <<"">>}, %% Amazon Pass
    {<<"access_id">>, <<"">>},  %% Amazon Id
    {<<"from">>, <<"Test <simple_from@mail.com>">>},
    {<<"subject">>, <<"Test Subject">>},
    {<<"txt">>, <<"test txt">>},
    {<<"html">>, <<"<!DOCTYPE html>"
                    "<html><head lang=\"ru\">"
                    "<meta charset=\"UTF-8\"><title>Обработка</title></head>"
                    "<style>.message {margin: -16px 0 0 -50px;position: absolute;top: 50%;left: 40%;font:bold 14px Arial;text-align: center;}body{background: #DCDCDC;}</style>"
                    "<body><div class=\"message\">...Подождите, <br> Ваша заявка обрабатывается ... </div>"
                    "</body></html>"/utf8>>},
    {<<"to">>, <<"simple_to@mail.com">>}
],
eaws:send_formatted(Par).
``` 

Sending Raw Email
--------

```
Par = [
    {<<"access_key">>, <<"">>}, %% Amazon Pass
    {<<"access_id">>, <<"">>},  %% Amazon Id
    {<<"from">>, <<"Test <simple_from@mail.com>">>},
    {<<"subject">>, <<"Test Subject">>},
    {<<"txt">>, <<"test txt">>},
    {<<"attachments">>, [
      {"test.txt", <<"Maibe Binary Content">>},
      filename:absname_join(code:priv_dir(eaws), "Blank_zakaza_primer.pdf"),
      filename:absname_join(code:priv_dir(eaws), "duck.jpg")
    ]},
    {<<"to">>, <<"simple_to@mail.com">>}
],
eaws:send_raw(Par).
```