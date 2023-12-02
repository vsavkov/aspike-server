-module(aspike_receive).
-include("../include/aspike_protocol.hrl").

%% API
-export([
  receive_response_admin/2,
  receive_response_operation/2
]).

receive_response_admin(Socket, Timeout) ->
  receive_proto(Socket, 8, Timeout, ?AS_ADMIN_MESSAGE_TYPE).

receive_response_operation(Socket, Timeout) ->
  receive_proto(Socket, 8, Timeout, ?AS_MESSAGE_TYPE).

receive_proto(Socket, Header_size, Timeout, Type) ->
  Ret_header = gen_tcp:recv(Socket, Header_size, Timeout),
  case Ret_header of
    {ok, Header} ->
      <<?AS_PROTO_VERSION:8, Type:8, Size:48/big-unsigned-integer>>
        = Header,
      Ret_data = gen_tcp:recv(Socket, Size, Timeout),
      case Ret_data of
        {ok, Data} -> <<Header/binary, Data/binary>>;
        Other_data -> Other_data
      end;
    Other_header -> Other_header
  end.
