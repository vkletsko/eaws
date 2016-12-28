Eaws
======

Eaws is a small Email sender written in Erlang.

Amazon SES Description: https://aws.amazon.com/ses/?nc1=h_ls

Usage:
--------------------
Sending Formatted Email (Including Html)
--------

```
Par = #{
    <<"access_id">> =>  <<>>,
    <<"access_key">> => <<>>,

    <<"aws_url">> => <<"https://email.eu-west-1.amazonaws.com/">>,
    <<"from">> => <<"success@simulator.amazonses.com">>,
    <<"to">> => <<"some_addr@mail.com">>,
    <<"subject">> => <<"test subject">>,
    <<"txt">> => <<"test txt">>,
    <<"html">> => <<"<!DOCTYPE html><html><body>"
    "<h1>You have successfully sent an email using Amazon SES!</h1>"
    "<p>For more information about Amazon SES, see the <a href=\"http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html\">Amazon SES Developer Guide</a>.</p>"
    "</body></html>"/utf8>>
  },
eaws:send_formatted(Par).
``` 

Sending Raw Email (With Attachments)
--------

```
Par = #{  <<"access_key">> => <<>>,  %% Amazon Pass
          <<"access_id">> => <<>>,   %% Amazon Id

          <<"aws_url">> => <<"https://email.eu-west-1.amazonaws.com/">>,
          <<"from">> => <<"success@simulator.amazonses.com">>,
          <<"to">> => <<"some_addr@mail.com">>,
          <<"subject">> => <<"test subject">>,
          <<"txt">> => <<"test txt">>,
          <<"attachments">> => [
            {<<"test.txt">>, <<"binary Content">>},
            list_to_binary(filename:absname_join(code:priv_dir(eaws), "simple.pdf")),
            filename:absname_join(code:priv_dir(eaws), "duck.jpg")
          ]
  },
eaws:send_raw(Par).
```

##### parameter "aws_url" need to be contained one of the available end point:
* https://email.us-east-1.amazonaws.com
* https://email.us-west-2.amazonaws.com
* https://email.eu-west-1.amazonaws.com

by default will be set:  
* https://email.eu-west-1.amazonaws.com
