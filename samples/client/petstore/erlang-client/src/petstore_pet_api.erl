-module(petstore_pet_api).

-export([add_pet/2, add_pet/3,
         delete_pet/2, delete_pet/3,
         find_pets_by_status/2, find_pets_by_status/3,
         find_pets_by_tags/2, find_pets_by_tags/3,
         get_pet_by_id/2, get_pet_by_id/3,
         update_pet/2, update_pet/3,
         update_pet_with_form/2, update_pet_with_form/3,
         upload_file/2, upload_file/3]).

-define(BASE_URL, <<"/v2">>).

%% @doc Add a new pet to the store
-spec add_pet(ctx:ctx(), petstore_pet:petstore_pet()) -> ok | {error, integer()}.
add_pet(Ctx, Body) ->
    add_pet(Ctx, Body, #{}).

-spec add_pet(ctx:ctx(), petstore_pet:petstore_pet(), maps:map()) -> ok | {error, integer()}.
add_pet(Ctx, Body, _Optional) ->
    Method = post,
    Path = ["/pet"],
    QS = [],
    Headers = [],
    Body1 = Body,
    Opts = [],

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, Headers, Body1, Opts).

%% @doc Deletes a pet
-spec delete_pet(ctx:ctx(), integer()) -> ok | {error, integer()}.
delete_pet(Ctx, PetId) ->
    delete_pet(Ctx, PetId, #{}).

-spec delete_pet(ctx:ctx(), integer(), maps:map()) -> ok | {error, integer()}.
delete_pet(Ctx, PetId, _Optional) ->
    Method = delete,
    Path = ["/pet/", PetId, ""],
    QS = [],
    Headers = []++[{X, maps:get(X, _Optional)} || X <- ['api_key'], maps:is_key(X, _Optional)],
    Body1 = [],
    Opts = [],

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, Headers, Body1, Opts).

%% @doc Finds Pets by status
%% Multiple status values can be provided with comma separated strings
-spec find_pets_by_status(ctx:ctx(), list()) -> {ok, list(), [petstore_pet:petstore_pet()]} | {error, string()}.
find_pets_by_status(Ctx, Status) ->
    find_pets_by_status(Ctx, Status, #{}).

-spec find_pets_by_status(ctx:ctx(), list(), maps:map()) -> {ok, list(), [petstore_pet:petstore_pet()]} | {error, string()}.
find_pets_by_status(Ctx, Status, _Optional) ->
    Method = get,
    Path = ["/pet/findByStatus"],
    QS = lists:flatten([[{<<"status">>, X} || X <- Status]])++[{X, maps:get(X, _Optional)} || X <- [], maps:is_key(X, _Optional)],
    Headers = [],
    Body1 = [],
    Opts = [],

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, Headers, Body1, Opts).

%% @doc Finds Pets by tags
%% Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-spec find_pets_by_tags(ctx:ctx(), list()) -> {ok, list(), [petstore_pet:petstore_pet()]} | {error, string()}.
find_pets_by_tags(Ctx, Tags) ->
    find_pets_by_tags(Ctx, Tags, #{}).

-spec find_pets_by_tags(ctx:ctx(), list(), maps:map()) -> {ok, list(), [petstore_pet:petstore_pet()]} | {error, string()}.
find_pets_by_tags(Ctx, Tags, _Optional) ->
    Method = get,
    Path = ["/pet/findByTags"],
    QS = lists:flatten([[{<<"tags">>, X} || X <- Tags]])++[{X, maps:get(X, _Optional)} || X <- [], maps:is_key(X, _Optional)],
    Headers = [],
    Body1 = [],
    Opts = [],

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, Headers, Body1, Opts).

%% @doc Find pet by ID
%% Returns a single pet
-spec get_pet_by_id(ctx:ctx(), integer()) -> {ok, list(), petstore_pet:petstore_pet()} | {error, string()}.
get_pet_by_id(Ctx, PetId) ->
    get_pet_by_id(Ctx, PetId, #{}).

-spec get_pet_by_id(ctx:ctx(), integer(), maps:map()) -> {ok, list(), petstore_pet:petstore_pet()} | {error, string()}.
get_pet_by_id(Ctx, PetId, _Optional) ->
    Method = get,
    Path = ["/pet/", PetId, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    Opts = [],

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, Headers, Body1, Opts).

%% @doc Update an existing pet
-spec update_pet(ctx:ctx(), petstore_pet:petstore_pet()) -> ok | {error, integer()}.
update_pet(Ctx, Body) ->
    update_pet(Ctx, Body, #{}).

-spec update_pet(ctx:ctx(), petstore_pet:petstore_pet(), maps:map()) -> ok | {error, integer()}.
update_pet(Ctx, Body, _Optional) ->
    Method = put,
    Path = ["/pet"],
    QS = [],
    Headers = [],
    Body1 = Body,
    Opts = [],

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, Headers, Body1, Opts).

%% @doc Updates a pet in the store with form data
-spec update_pet_with_form(ctx:ctx(), integer()) -> ok | {error, integer()}.
update_pet_with_form(Ctx, PetId) ->
    update_pet_with_form(Ctx, PetId, #{}).

-spec update_pet_with_form(ctx:ctx(), integer(), maps:map()) -> ok | {error, integer()}.
update_pet_with_form(Ctx, PetId, _Optional) ->
    Method = post,
    Path = ["/pet/", PetId, ""],
    QS = [],
    Headers = [],
    Body1 = {form, []++[{X, maps:get(X, _Optional)} || X <- ['name', 'status'], maps:is_key(X, _Optional)]},
    Opts = [],

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, Headers, Body1, Opts).

%% @doc uploads an image
-spec upload_file(ctx:ctx(), integer()) -> {ok, list(), petstore_api_response:petstore_api_response()} | {error, string()}.
upload_file(Ctx, PetId) ->
    upload_file(Ctx, PetId, #{}).

-spec upload_file(ctx:ctx(), integer(), maps:map()) -> {ok, list(), petstore_api_response:petstore_api_response()} | {error, string()}.
upload_file(Ctx, PetId, _Optional) ->
    Method = post,
    Path = ["/pet/", PetId, "/uploadImage"],
    QS = [],
    Headers = [],
    Body1 = {form, []++[{X, maps:get(X, _Optional)} || X <- ['additionalMetadata', 'file'], maps:is_key(X, _Optional)]},
    Opts = [],

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, Headers, Body1, Opts).


