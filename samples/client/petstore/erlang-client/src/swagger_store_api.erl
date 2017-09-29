-module(swagger_store_api).

-export([delete_order/1,
         get_inventory/0,
         get_order_by_id/1,
         place_order/1]).

-define(BASE_URL, <<"http://petstore.swagger.io/v2">>).

%% @doc Delete purchase order by ID
%% For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
-spec delete_order(binary()) -> ok.
delete_order(OrderId) ->
    Method = delete,
    Path = ["/store/order/", OrderId, ""],
    QS = lists:flatten([]),
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),


    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 400, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end; 
        {ok, 404, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end
    end.

%% @doc Returns pet inventories by status
%% Returns a map of status codes to quantities
-spec get_inventory() -> maps:map().
get_inventory() ->
    Method = get,
    Path = ["/store/inventory"],
    QS = lists:flatten([]),
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),


    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end
    end.

%% @doc Find purchase order by ID
%% For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
-spec get_order_by_id(integer()) -> swagger_order:swagger_order().
get_order_by_id(OrderId) ->
    Method = get,
    Path = ["/store/order/", OrderId, ""],
    QS = lists:flatten([]),
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),


    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end; 
        {ok, 400, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end; 
        {ok, 404, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end
    end.

%% @doc Place an order for a pet
-spec place_order(swagger_order:swagger_order()) -> swagger_order:swagger_order().
place_order(Body) ->
    Method = post,
    Path = ["/store/order"],
    QS = lists:flatten([]),
    Headers = [],
    Body1 = Body,
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),


    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end; 
        {ok, 400, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end
    end.


