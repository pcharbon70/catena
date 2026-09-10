defmodule Catena.Distribution.TLS do
  @moduledoc "Mutually authenticated TLS 1.3 carrier for Catena distribution frames."

  alias Catena.Distribution.{Contract, Wire}

  @timeout 5_000

  def listen(contract, options) when is_list(options) do
    with :ok <- Contract.validate(contract),
         {:ok, tls} <- tls_options(options, :server),
         {:ok, _} <- Application.ensure_all_started(:ssl),
         {:ok, socket} <-
           :ssl.listen(0, [:binary, packet: 4, active: false, reuseaddr: true] ++ tls),
         {:ok, {_address, port}} <- :ssl.sockname(socket) do
      {:ok, %{socket: socket, port: port, contract: contract, timeout: timeout(options)}}
    else
      _ -> {:error, :tls_listen_failed}
    end
  end

  def listen(_, _), do: {:error, :tls_listen_failed}

  def accept(%{socket: listener, contract: contract, timeout: timeout}) do
    with {:ok, transport} <- :ssl.transport_accept(listener, timeout),
         {:ok, socket} <- :ssl.handshake(transport, timeout),
         {:ok, connection} <- authenticate(socket, contract, timeout) do
      {:ok, connection}
    else
      _ -> {:error, :tls_accept_failed}
    end
  end

  def connect(host, port, contract, options) when is_integer(port) and is_list(options) do
    with :ok <- Contract.validate(contract),
         {:ok, tls} <- tls_options(options, :client),
         {:ok, _} <- Application.ensure_all_started(:ssl),
         {:ok, socket} <-
           :ssl.connect(host, port, [:binary, packet: 4, active: false] ++ tls, timeout(options)),
         {:ok, connection} <- authenticate(socket, contract, timeout(options)) do
      {:ok, connection}
    else
      _ -> {:error, :tls_connect_failed}
    end
  end

  def send(%{socket: socket}, bytes) when is_binary(bytes), do: :ssl.send(socket, bytes)

  def receive(%{socket: socket, timeout: timeout}), do: :ssl.recv(socket, 0, timeout)

  def close(%{socket: socket}), do: :ssl.close(socket)
  def close_listener(%{socket: socket}), do: :ssl.close(socket)

  defp authenticate(socket, contract, timeout) do
    with :ok <- :ssl.send(socket, Wire.hello(contract)),
         {:ok, bytes} <- :ssl.recv(socket, 0, timeout),
         {:ok, peer} <- Wire.decode_hello(bytes),
         {:ok, certificate} <- :ssl.peercert(socket),
         true <- peer["protocol"] == contract.local.digest,
         true <-
           peer["endpoint"] ==
             Contract.endpoint_digest(
               contract.version,
               peer["node"],
               peer["service"],
               peer["package"],
               peer["protocol"]
             ),
         :ok <-
           Contract.authorize(
             contract,
             peer["node"],
             peer["service"],
             peer["package"],
             certificate
           ),
         :ok <- authenticated_ready(socket, timeout) do
      {:ok,
       %{
         socket: socket,
         peer: peer["node"],
         peer_service: peer["service"],
         peer_package: peer["package"],
         timeout: timeout,
         authenticated: true
       }}
    else
      _ ->
        :ssl.close(socket)
        {:error, :remote_handshake_refused}
    end
  end

  defp authenticated_ready(socket, timeout) do
    with :ok <- :ssl.send(socket, "catena-distribution-ready"),
         {:ok, "catena-distribution-ready"} <- :ssl.recv(socket, 0, timeout),
         do: :ok
  end

  defp tls_options(options, role) do
    allowed = [:certfile, :keyfile, :cacertfile, :timeout]

    with [] <- Keyword.keys(options) -- allowed,
         certfile when is_binary(certfile) <- Keyword.get(options, :certfile),
         keyfile when is_binary(keyfile) <- Keyword.get(options, :keyfile),
         cacertfile when is_binary(cacertfile) <- Keyword.get(options, :cacertfile),
         configured_timeout
         when is_integer(configured_timeout) and configured_timeout in 1..60_000 <-
           Keyword.get(options, :timeout, @timeout),
         true <- Enum.all?([certfile, keyfile, cacertfile], &File.regular?/1) do
      common = [
        versions: [:"tlsv1.3"],
        verify: :verify_peer,
        certfile: String.to_charlist(certfile),
        keyfile: String.to_charlist(keyfile),
        cacertfile: String.to_charlist(cacertfile)
      ]

      role_options =
        if role == :server,
          do: [fail_if_no_peer_cert: true],
          else: [server_name_indication: :disable]

      {:ok, common ++ role_options}
    else
      _ -> {:error, :invalid_tls_configuration}
    end
  end

  defp timeout(options) do
    Keyword.get(options, :timeout, @timeout)
  end
end
