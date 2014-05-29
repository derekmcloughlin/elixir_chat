defmodule ChatRoom do

  use GenServer.Behaviour

  def max_idle_time, do: 1200    # seconds
  def check_idle_time, do: 1000 # Milliseconds

  defmodule ClientState do
    defstruct id: 0, nick: nil, host: nil, last_action: nil
  end

  defmodule State do
    defstruct clients: []
  end

  def init(_args) do
	:erlang.process_flag(:trap_exit, true)
    :timer.apply_after(check_idle_time, ChatRoom, :find_idle_clients, [])
    {:ok, %State{}}
  end
 
  def start_link() do
    :gen_server.start_link {:local, :chatroom}, ChatRoom, [], []
  end

  def stop() do
    :gen_server.cast :chatroom, {:stop, {}}
  end

  def join(nick, host) do
    :gen_server.call :chatroom, {:join, {nick, host}}, :infinity
  end

  def leave(session, reason) do
    :gen_server.cast :chatroom, {:leave, {session, reason}}
  end

  def chat_message(session, message) do
    :gen_server.cast :chatroom, {:chat_message, {session, message}}
  end

  def get_state(session) do
    :gen_server.call :chatroom, {:get_state, session}, :infinity
  end

  def get_users(session) do
    :gen_server.call :chatroom, {:get_users, {session}}, :infinity
  end

  def get_msg_id(session, pid) do
    :gen_server.cast :chatroom, {:get_msg_id, {session, pid}}
  end

  def wait(session, message_id, pid) do
    :gen_server.cast :chatroom, {:wait, {session, message_id, pid}}
  end

  def wait_finish(session, pid) do
    :gen_server.cast :chatroom, {:wait_finish, {session, pid}}
  end

  def find_idle_clients() do
    :gen_server.cast :chatroom, {:find_idle_clients, {}}
  end

 
  def handle_call({:join, {nick, host}}, _from, state) do
    case validate_nick(nick, state) do
      {:error, reason} -> 
        {:reply, {:error, reason}, state}
      {:ok, valid_nick} ->
        session = get_unique_session state
        case ChatPostOffice.create_mailbox session do
          :ok -> 
            ChatPostOffice.broadcast_mail({:msg, {:user_joined_room, valid_nick}}, [session])
            new_client = %ClientState{ id: session, nick: valid_nick, host: host, last_action: :erlang.now()}
            # IO.puts "State: #{inspect(state)}"
            {:reply, {:ok, session}, %State{state | clients: [new_client | state.clients]}}
          {:error, _} -> 
            {:reply, {:error, :not_available}, state}
        end
    end
  end

  def handle_call({:get_state, _session}, _from, state) do
    {:reply, {:ok, {:state, state}}, state}
  end

  def handle_call({:get_users, {session}}, _from, state) do
    case get_session(session, state) do
      {:error, :not_found} -> 
        {:reply, {:error, :not_found}, state}
      {:ok, client} -> 
        new_state = update_client(client, state)
        {:reply, {:ok, Enum.map(state.clients, fn(c) -> c.nick end)}, new_state}
    end
  end

  def handle_cast({:get_msg_id, {session, pid}}, state) do
    case get_session(session, state) do
      {:error, :not_found} -> 
        send pid, {:error, :bad_session}
        {:noreply, state}
      {:ok, client} -> 
        new_state = update_client(client, state)
        ChatPostOffice.send_mail session, {:get_msg_id, pid} 
        {:noreply, new_state}
    end
  end

  def handle_cast({:wait, {session, message_id, pid}}, state) do
    case get_session(session, state) do
      {:error, :not_found} -> 
        send pid, {:error, :bad_session}
        {:noreply, state}
      {:ok, client} -> 
        new_state = update_client(client, state)
        ChatPostOffice.send_mail session, {:add_listener, {message_id, pid}}
        {:noreply, new_state}
    end
  end

  def handle_cast({:wait_finish, {session, pid}}, state) do
    case get_session(session, state) do
      {:error, :not_found} -> 
        {:noreply, state}
      {:ok, _client} -> 
        ChatPostOffice.send_mail session, {:remove_listener, pid}
        {:noreply, state}
    end
  end

  def handle_cast({:find_idle_clients, {}}, state) do
    Enum.each(state.clients,
      fn(client) ->
        last_action = :calendar.now_to_datetime client.last_action
        now = :erlang.now |> :calendar.now_to_datetime
        idle_seconds =  :calendar.datetime_to_gregorian_seconds(now) - :calendar.datetime_to_gregorian_seconds(last_action)
        case idle_seconds > max_idle_time do
          true -> 
            IO.puts "User timed out: #{client.nick}, secs: #{idle_seconds}"
            :timer.apply_after(0, __MODULE__, :leave, [client.id, "timeout"])
          _ -> :noop
        end
      end)
    :timer.apply_after(check_idle_time, __MODULE__, :find_idle_clients, [])
    {:noreply, state}
  end

  def handle_cast({:stop, {}}, state) do
    {:stop, :normal, state}
  end

  def handle_cast({:leave, {session, reason}}, state) do
    case get_session(session, state) do
      {:error, :not_found} -> 
        {:noreply, state}
      {:ok, client} ->
        ChatPostOffice.delete_mailbox client.id
        clean_reason =  reason |> String.slice 0, 32 
        ChatPostOffice.broadcast_mail {:msg, {:user_left_room, {client.nick, clean_reason}}}, [client.id]
        other_clients = Enum.filter(state.clients, fn(c) -> c.id != client.id end)
        {:noreply, %State{clients: other_clients}}
    end
  end

  def handle_cast({:chat_message, {session, message}}, state) do
    case get_session(session, state) do
      {:error, :not_found} -> 
        {:noreply, state}
      {:ok, client} ->
        clean_message =  message |> String.slice 0, 256 
        ChatPostOffice.broadcast_mail {:msg, {:chat_msg, {client.nick, clean_message}}}, [client.id]
        ChatPostOffice.send_mail client.id, {:msg, {:sent_chat_msg, {client.nick, clean_message}}}
        new_state = update_client(client, state)
        {:noreply, new_state}
    end
  end  
 
  def update_client(client, state) do
    new_client = %ClientState{ client | last_action: :erlang.now() }
    others = Enum.filter(state.clients, fn(c) -> c.id != client.id end)
    %State{ clients: [new_client | others]}
  end

  def get_unique_session(state) do
    hash = ChatUtil.generate_hash
    case Enum.filter(state.clients, fn(client_state) -> client_state.id == hash end) do
      [] -> hash
      _ -> get_unique_session state
    end
  end

  def get_session(session, state) do 
    case Enum.filter(state.clients, fn(c) -> c.id == session end) do
      [] -> {:error, :not_found}
      [x|_] -> {:ok, x}
    end
  end

  def validate_nick([], _) do
    {:error, :bad_format}
  end

  def validate_nick(nick, state) do
    shortened = nick |> String.strip |> String.slice 0, 16 
    case {Regex.run(~r/^([A-Za-z0-9]+)$/, shortened), Enum.filter(state.clients, fn(client) -> client.nick == shortened end)} do
      {[shortened, shortened], []} -> {:ok, shortened}
      {[shortened, shortened], _} -> {:error, :not_available}
      {nil, []} -> {:error, :bad_format}
       _ -> {:error, :not_available}
    end
  end

end

