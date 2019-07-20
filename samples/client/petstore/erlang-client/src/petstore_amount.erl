-module(petstore_amount).

-export([encode/1]).

-export_type([petstore_amount/0]).

-type petstore_amount() ::
    #{ 'value' := float(),
       'currency' := petstore_currency:petstore_currency()
     }.

encode(#{ 'value' := Value,
          'currency' := Currency
        }) ->
    #{ 'value' => Value,
       'currency' => Currency
     }.
