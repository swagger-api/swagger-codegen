-module(petstore_utils).

-export([request/7]).

request(_Ctx, Method, Path, QS, Headers, Body, Opts) ->
    Url = hackney_url:make_url(application:get_env(petstore, host, "localhost"), Path, QS),

    Body1 = case lists:keyfind(<<"Content-Type">>, 1, Headers) of
                {_, <<"application/json", _/binary>>} ->
                    jsx:encode(Body);
                _ ->
                    Body
            end,

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, Status, RespHeaders, ClientRef} when Status >= 200,
                                                  Status =< 299 ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            Resp = decode_response(RespHeaders, ResponseBody),
            {ok, Resp, #{status => Status,
                         headers => RespHeaders}};
        {ok, Status, RespHeaders, ClientRef} when Status >= 300 ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            Resp = decode_response(RespHeaders, ResponseBody),
            {error, Resp, #{status => Status,
                            headers => RespHeaders}}
    end.

decode_response(Headers, Body) ->
    case lists:keyfind(<<"Content-Type">>, 1, Headers) of
        {_, <<"application/json", _/binary>>} ->
            jsx:decode(Body, [return_maps, {labels, atom}]);
        %% TODO: yml, protobuf, user defined function
        _ ->
            Body
    end.
