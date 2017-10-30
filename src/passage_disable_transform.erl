%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc This removes all `passage_trace' attributes
-module(passage_disable_transform).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([parse_transform/2]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @doc Performs transformations
-spec parse_transform(AbstractForms, list()) -> AbstractForms when
      AbstractForms :: [term()].
parse_transform(AbstractForms, _CompileOptions) ->
    lists:filter(fun ({attribute, _, passage_trace, _}) -> false;
                     (_) -> true
                 end,
                 AbstractForms).
