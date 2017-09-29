-module(swagger_pet_api).

-export([add_pet/1,
         delete_pet/2,
         find_pets_by_status/1,
         find_pets_by_tags/1,
         get_pet_by_id/1,
         update_pet/1,
         update_pet_with_form/3,
         upload_file/3]).

-define(BASE_URL, <<"http://petstore.swagger.io/v2">>).

%% @doc Add a new pet to the store
-spec add_pet(swagger_pet:swagger_pet()) -> ok.
add_pet(Body) ->
    Method = post,
    Path = ["/pet"],
    QS = lists:flatten([]),
    Headers = [],
    Body1 = Body,
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),


    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 405, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end
    end.

%% @doc Deletes a pet
-spec delete_pet(integer(), binary()) -> ok.
delete_pet(PetId, ApiKey) ->
    Method = delete,
    Path = ["/pet/", PetId, ""],
    QS = lists:flatten([]),
    Headers = [{<<"api_key">>, ApiKey}],
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
            end
    end.

%% @doc Finds Pets by status
%% Multiple status values can be provided with comma separated strings
-spec find_pets_by_status(list()) -> [swagger_pet:swagger_pet()].
find_pets_by_status(Status) ->
    Method = get,
    Path = ["/pet/findByStatus"],
    QS = lists:flatten([[{<<"status">>, X} || X <- Status]]),
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
            end
    end.

%% @doc Finds Pets by tags
%% Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-spec find_pets_by_tags(list()) -> [swagger_pet:swagger_pet()].
find_pets_by_tags(Tags) ->
    Method = get,
    Path = ["/pet/findByTags"],
    QS = lists:flatten([[{<<"tags">>, X} || X <- Tags]]),
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
            end
    end.

%% @doc Find pet by ID
%% Returns a single pet
-spec get_pet_by_id(integer()) -> swagger_pet:swagger_pet().
get_pet_by_id(PetId) ->
    Method = get,
    Path = ["/pet/", PetId, ""],
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

%% @doc Update an existing pet
-spec update_pet(swagger_pet:swagger_pet()) -> ok.
update_pet(Body) ->
    Method = put,
    Path = ["/pet"],
    QS = lists:flatten([]),
    Headers = [],
    Body1 = Body,
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
            end; 
        {ok, 405, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end
    end.

%% @doc Updates a pet in the store with form data
-spec update_pet_with_form(integer(), binary(), binary()) -> ok.
update_pet_with_form(PetId, Name, Status) ->
    Method = post,
    Path = ["/pet/", PetId, ""],
    QS = lists:flatten([]),
    Headers = [],
    Body1 = {form, [{<<"name">>, Name}, {<<"status">>, Status}]},
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),


    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 405, RespHeaders, ClientRef} ->
            case lists:keyfind(<<"Content-Type">>, 1, RespHeaders) of
            
              <<"application/xml", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}]); 
              <<"application/json", _/binary>> ->
                  {ok, Body} = hackney:body(ClientRef),

                  jsx:decode(Body, [returns_maps, {labels, attempt_atom}])
            end
    end.

%% @doc uploads an image
-spec upload_file(integer(), binary(), binary()) -> swagger_api_response:swagger_api_response().
upload_file(PetId, AdditionalMetadata, File) ->
    Method = post,
    Path = ["/pet/", PetId, "/uploadImage"],
    QS = lists:flatten([]),
    Headers = [],
    Body1 = {form, [{<<"additionalMetadata">>, AdditionalMetadata}, {<<"file">>, File}]},
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


