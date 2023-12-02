-module(aspike_server_processor).
-include("../include/aspike_protocol.hrl").
-include("../include/aspike_node_test.hrl").
%%-include_lib("eunit/include/eunit.hrl").

%%
%% Aerospike server emulator
%%

%% API
-export([
  process/1
]).

process({login_request, {_Command, Fields}}) ->
  case proplists:get_value(?USER, Fields) of
    ?TEST_USER1 ->
      case proplists:get_value(?CREDENTIAL, Fields) of
        ?TEST_PASSWORD1_CRYPT ->
          Response = {login_response,
            #{?SESSION_TTL => ?TEST_SESSION_TTL,
              ?SESSION_TOKEN => ?TEST_SESSION_TOKEN1}},
          Response;
        undefined -> {login_response, no_password};
        _ -> {login_response, wrong_password}
      end;
    undefined -> {login_response, no_user};
    _ -> {login_response, unknown_user}
  end;

process({put_request, {Namespace_str, Set_name, Key_digest, Bins}}) ->
  Bvs = [{B, V} || {_Op, B, V} <- Bins],
  {Namespace_str, Set_name, Key_digest, Bvs};

process({get_request, {Namespace_str, Set_name, Key_digest, Bins}}) ->
  {Namespace_str, Set_name, Key_digest, Bins};

process({remove_request, {Namespace_str, Set_name, Key_digest}}) ->
  {Namespace_str, Set_name, Key_digest};

process({exists_request, {Namespace_str, Set_name, Key_digest}}) ->
  {Namespace_str, Set_name, Key_digest};

process(X) ->
  {unknown_response, X}.
