defmodule ChatRoom do

  use GenServer.Behaviour

  defrecord ClientState, id: 0, nick: nil, host: nil, last_action: nil

  defrecord State, clients: []

  def init(_args) do
    {:ok, State.new}
  end
 
  def start_link() do
    :gen_server.start_link({:local, :chatroom}, ChatRoom, [], [])
  end

  def join(nick, host) do
    :gen_server.call(:chatroom, {:join, {nick, host}}, :infinity)
  end

  def handle_call({:join, {nick, host}}, _from, state) when is_list nick do
    case validate_nick(nick, state) do
      {:error, reason} -> 
        {:reply, {:error, reason}, state}
      {:ok, valid_nick} ->
        session = get_unique_session state
        case ChatPostOffice.create_mailbox Session do
          :ok -> 
            ChatPostOffice.broadcast_mail({:msg, {:user_joined_room, valid_nick}}, [session])
            new_client = ClientState.new id: session, nick: valid_nick, host: host, last_action: :erlang.now()
            {:reply, {:ok, session}, State.new clients: [new_client | state.clients]}
          {:error, _} -> 
            {:reply, {:error, :not_available}, state}
        end
    end
  end

  defp get_unique_session(state) do
    "dummy"
  end

  def validate_nick([], _) do
    {:error, :bad_format}
  end

  def validate_nick(nick, state) do
    shortened = nick |> String.strip |> String.slice 0, 16 
    case {Regex.run(%r/^([A-Za-z0-9]+)$/, shortened), Enum.filter(state.clients, fn(client) -> client.nick == shortened end)} do
      {[shortened, shortened], []} -> {:ok, shortened}
      {[shortened, shortened], _} -> {:error, :not_available}
      {nil, []} -> {:error, :bad_format}
       _ -> {:error, :not_available}
    end
  end

end

