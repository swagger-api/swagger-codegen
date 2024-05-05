-module(petstore_currency).

-export([encode/1]).

-export_type([petstore_currency/0]).

-type petstore_currency() ::
    #{ 
     }.

encode(#{ 
        }) ->
    #{ 
     }.
