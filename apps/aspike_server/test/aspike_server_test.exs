defmodule Aspike.ServerTest do
  use ExUnit.Case
  doctest Aspike.Server

  setup do
    Application.stop(:aspike_storage)
    :ok = Application.start(:aspike_storage)
#    Application.ensure_all_started(:aspike_storage)
  end

  setup do
    opts = [:binary, packet: :raw, active: false]
    {:ok, socket} = :gen_tcp.connect('localhost', 4041, opts)
    {:ok, socket: socket}
  end

  setup do
    opts = [:binary, packet: :line, active: false]
    {:ok, socket_text} = :gen_tcp.connect('localhost', 4040, opts)
    {:ok, socket_text: socket_text}
  end

  setup do
    {:ok,
      namespace: "namespace-gateway",
      set: "set-gateway",
      bin: 'aspike-bin-1',
      kp: "key-aspike-",
      vp: "value-aspike-"
    }
  end

  setup do
    {:ok,
      test_user: <<"User1">>,
      credential: 3,
      test_password_crypt: <<"$2a$10$7EqJtq98hPqEX7fNZaFWoOOY1Ba9.gZNwHJkrSKJl7mXQyPCsCrQa">>
    }
  end

  setup do
    {:ok,
      session_token: <<"test_session_token1">>,
      session_ttl: 12345
    }
  end

  test "login", %{
        socket: socket,
        test_user: test_user,
        credential: credential,
        test_password_crypt: test_password_crypt,
        session_token: session_token,
        session_ttl: session_ttl} do

    pkt_login = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    send = :gen_tcp.send(socket, pkt_login)
    assert send == :ok

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl
  end

  test "put into not existing namespace", %{
      socket: socket,
      test_user: test_user,
      credential: credential,
      test_password_crypt: test_password_crypt,
      session_token: session_token,
      session_ttl: session_ttl,
      namespace: namespace,
      set: set,
      bin: bin,
      kp: kp,
      vp: vp} do

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)
    put_request = :aspike_protocol.enc_put_request(namespace, set, key_digest, [{bin, value}])
    :ok = :gen_tcp.send(socket, put_request)
    put_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, put_decoded, rest} = :aspike_protocol.dec_put_response(put_response)
    assert put_decoded == Aspike.Status.err_NAMESPACE_NOT_FOUND
    assert rest == <<>>
  end

  test "put", %{
      socket: socket,
      socket_text: socket_text,
      test_user: test_user,
      credential: credential,
      test_password_crypt: test_password_crypt,
      session_token: session_token,
      session_ttl: session_ttl,
      namespace: namespace,
      set: set,
      bin: bin,
      kp: kp,
      vp: vp} do

    assert send_and_recv(socket_text, "CREATE #{namespace}\r\n") == "OK\r\n"

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)
    put_request = :aspike_protocol.enc_put_request(namespace, set, key_digest, [{bin, value}])
    :ok = :gen_tcp.send(socket, put_request)
    put_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, put_decoded, rest} = :aspike_protocol.dec_put_response(put_response)
    assert put_decoded == Aspike.Status.ok
    assert rest == <<>>
  end

  test "get from not existing namespace", %{
    socket: socket,
    test_user: test_user,
    credential: credential,
    test_password_crypt: test_password_crypt,
    session_token: session_token,
    session_ttl: session_ttl,
    namespace: namespace,
    set: set,
    bin: _bin,
    kp: kp,
    vp: vp} do

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, _value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)
    get_request = :aspike_protocol.enc_get_request(namespace, set, key_digest, [])
    :ok = :gen_tcp.send(socket, get_request)
    get_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, get_decoded, rest} = :aspike_protocol.dec_get_response(get_response)
    assert elem(get_decoded, 0) == Aspike.Status.err_NAMESPACE_NOT_FOUND
    assert rest == <<>>
  end

  test "get non-existing record", %{
    socket: socket,
    socket_text: socket_text,
    test_user: test_user,
    credential: credential,
    test_password_crypt: test_password_crypt,
    session_token: session_token,
    session_ttl: session_ttl,
    namespace: namespace,
    set: set,
    bin: _bin,
    kp: kp,
    vp: vp} do

    assert send_and_recv(socket_text, "CREATE #{namespace}\r\n") == "OK\r\n"

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, _value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)
    get_request = :aspike_protocol.enc_get_request(namespace, set, key_digest, [])
    :ok = :gen_tcp.send(socket, get_request)
    get_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, get_decoded, rest} = :aspike_protocol.dec_get_response(get_response)
    assert elem(get_decoded, 0) == Aspike.Status.err_RECORD_NOT_FOUND
    assert rest == <<>>
  end

  test "get existing record", %{
    socket: socket,
    socket_text: socket_text,
    test_user: test_user,
    credential: credential,
    test_password_crypt: test_password_crypt,
    session_token: session_token,
    session_ttl: session_ttl,
    namespace: namespace,
    set: set,
    bin: bin,
    kp: kp,
    vp: vp} do

    assert send_and_recv(socket_text, "CREATE #{namespace}\r\n") == "OK\r\n"

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)

    put_request = :aspike_protocol.enc_put_request(namespace, set, key_digest, [{bin, value}])
    :ok = :gen_tcp.send(socket, put_request)
    put_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, put_decoded, rest} = :aspike_protocol.dec_put_response(put_response)
    assert put_decoded == Aspike.Status.ok
    assert rest == <<>>

    get_request = :aspike_protocol.enc_get_request(namespace, set, key_digest, [])
    :ok = :gen_tcp.send(socket, get_request)
    get_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, get_decoded, rest} = :aspike_protocol.dec_get_response(get_response)
    assert elem(get_decoded, 0) == Aspike.Status.ok
    assert elem(get_decoded, 1) == []
    assert elem(get_decoded, 2) == [{to_string(bin), value}]
    assert rest == <<>>
  end

  test "remove from not existing namespace", %{
    socket: socket,
    test_user: test_user,
    credential: credential,
    test_password_crypt: test_password_crypt,
    session_token: session_token,
    session_ttl: session_ttl,
    namespace: namespace,
    set: set,
    bin: _bin,
    kp: kp,
    vp: vp} do

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, _value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)
    remove_request = :aspike_protocol.enc_remove_request(namespace, set, key_digest)
    :ok = :gen_tcp.send(socket, remove_request)
    remove_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, remove_decoded, rest} = :aspike_protocol.dec_remove_response(remove_response)
    assert remove_decoded == Aspike.Status.err_NAMESPACE_NOT_FOUND
    assert rest == <<>>
  end

  test "remove non-existing record", %{
    socket: socket,
    socket_text: socket_text,
    test_user: test_user,
    credential: credential,
    test_password_crypt: test_password_crypt,
    session_token: session_token,
    session_ttl: session_ttl,
    namespace: namespace,
    set: set,
    bin: _bin,
    kp: kp,
    vp: vp} do

    assert send_and_recv(socket_text, "CREATE #{namespace}\r\n") == "OK\r\n"

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, _value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)
    remove_request = :aspike_protocol.enc_remove_request(namespace, set, key_digest)
    :ok = :gen_tcp.send(socket, remove_request)
    remove_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, remove_decoded, rest} = :aspike_protocol.dec_remove_response(remove_response)
    assert remove_decoded == Aspike.Status.err_RECORD_NOT_FOUND
    assert rest == <<>>
  end

  test "remove existing record", %{
    socket: socket,
    socket_text: socket_text,
    test_user: test_user,
    credential: credential,
    test_password_crypt: test_password_crypt,
    session_token: session_token,
    session_ttl: session_ttl,
    namespace: namespace,
    set: set,
    bin: bin,
    kp: kp,
    vp: vp} do

    assert send_and_recv(socket_text, "CREATE #{namespace}\r\n") == "OK\r\n"

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)

    put_request = :aspike_protocol.enc_put_request(namespace, set, key_digest, [{bin, value}])
    :ok = :gen_tcp.send(socket, put_request)
    put_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, put_decoded, rest} = :aspike_protocol.dec_put_response(put_response)
    assert put_decoded == Aspike.Status.ok
    assert rest == <<>>

    get_request = :aspike_protocol.enc_get_request(namespace, set, key_digest, [])
    :ok = :gen_tcp.send(socket, get_request)
    get_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, get_decoded, rest} = :aspike_protocol.dec_get_response(get_response)
    assert elem(get_decoded, 0) == Aspike.Status.ok
    assert elem(get_decoded, 1) == []
    assert elem(get_decoded, 2) == [{to_string(bin), value}]
    assert rest == <<>>

    remove_request = :aspike_protocol.enc_remove_request(namespace, set, key_digest)
    :ok = :gen_tcp.send(socket, remove_request)
    remove_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, remove_decoded, rest} = :aspike_protocol.dec_remove_response(remove_response)
    assert remove_decoded == Aspike.Status.ok
    assert rest == <<>>

    :ok = :gen_tcp.send(socket, get_request)
    get_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, get_decoded, rest} = :aspike_protocol.dec_get_response(get_response)
    assert elem(get_decoded, 0) == Aspike.Status.err_RECORD_NOT_FOUND
    assert rest == <<>>
  end

  test "exists in not existing namespace", %{
    socket: socket,
    test_user: test_user,
    credential: credential,
    test_password_crypt: test_password_crypt,
    session_token: session_token,
    session_ttl: session_ttl,
    namespace: namespace,
    set: set,
    bin: _bin,
    kp: kp,
    vp: vp} do

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, _value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)
    exists_request = :aspike_protocol.enc_exists_request(namespace, set, key_digest)
    :ok = :gen_tcp.send(socket, exists_request)
    exists_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, exists_decoded, rest} = :aspike_protocol.dec_exists_response(exists_response)
    assert exists_decoded == Aspike.Status.err_NAMESPACE_NOT_FOUND
    assert rest == <<>>
  end

  test "exists record", %{
    socket: socket,
    socket_text: socket_text,
    test_user: test_user,
    credential: credential,
    test_password_crypt: test_password_crypt,
    session_token: session_token,
    session_ttl: session_ttl,
    namespace: namespace,
    set: set,
    bin: bin,
    kp: kp,
    vp: vp} do

    assert send_and_recv(socket_text, "CREATE #{namespace}\r\n") == "OK\r\n"

    login_request = :aspike_protocol.enc_login_request(test_user, {credential, test_password_crypt})
    :ok = :gen_tcp.send(socket, login_request)

    response = :aspike_receive.receive_response_admin(socket, 1000)
    {:ok, {status, fields}, <<>>} = :aspike_protocol.dec_login_response(response)
    assert Aspike.Status.ok == status
    assert length(fields) == 2
    assert fields[:session_token] == session_token
    assert fields[:session_ttl] == session_ttl

    {key, value} = mk_kv(kp, vp)
    key_digest = :aspike_protocol.digest(set, key)

    exists_request = :aspike_protocol.enc_exists_request(namespace, set, key_digest)
    :ok = :gen_tcp.send(socket, exists_request)
    exists_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, exists_decoded, rest} = :aspike_protocol.dec_exists_response(exists_response)
    assert exists_decoded == Aspike.Status.err_RECORD_NOT_FOUND
    assert rest == <<>>

    put_request = :aspike_protocol.enc_put_request(namespace, set, key_digest, [{bin, value}])
    :ok = :gen_tcp.send(socket, put_request)
    put_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, put_decoded, rest} = :aspike_protocol.dec_put_response(put_response)
    assert put_decoded == Aspike.Status.ok
    assert rest == <<>>

    :ok = :gen_tcp.send(socket, exists_request)
    exists_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, exists_decoded, rest} = :aspike_protocol.dec_exists_response(exists_response)
    assert exists_decoded == Aspike.Status.ok
    assert rest == <<>>

    remove_request = :aspike_protocol.enc_remove_request(namespace, set, key_digest)
    :ok = :gen_tcp.send(socket, remove_request)
    remove_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, remove_decoded, rest} = :aspike_protocol.dec_remove_response(remove_response)
    assert remove_decoded == Aspike.Status.ok
    assert rest == <<>>

    :ok = :gen_tcp.send(socket, exists_request)
    exists_response = :aspike_receive.receive_response_operation(socket, 1000)
    {:ok, exists_decoded, rest} = :aspike_protocol.dec_exists_response(exists_response)
    assert exists_decoded == Aspike.Status.err_RECORD_NOT_FOUND
    assert rest == <<>>
  end

  ## Key-Value
  defp mk_kv(key_prefix, value_prefix) do
    ts = to_string(:erlang.system_time(:millisecond))
    k = key_prefix <> ts
    v = value_prefix <> ts
    {k, v}
  end

  defp send_and_recv(socket, command) do
    :ok = :gen_tcp.send(socket, command)
    {:ok, data} = :gen_tcp.recv(socket, 0, 1000)
    data
  end

end
