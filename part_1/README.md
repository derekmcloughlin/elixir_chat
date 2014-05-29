# ChatTutorial - Part 1

This is an Elixir Chat Server based on Chris Moos's
[Building an Erlang Chat Server with Comet](http://www.chrismoos.com/2009/09/28/building-an-erlang-chat-server-with-comet-part-1)
the source of which can be found on [Github](https://github.com/chrismoos/erl_chat_tutorial).

Creating the project
--------------------

You can create a new project using mix:

```sh
> mix new elixir_chat
* creating README.md
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/elixir_chat.ex
* creating test
* creating test/test_helper.exs
* creating test/elixir_chat_test.exs

Your mix project was created with success.
You can use mix to compile it, test it, and more:

    cd elixir_chat
    mix compile
    mix test

Run `mix help` for more information.
```

Note that in the code on github I changed things around a bit to split the tutorial into part_1, part_2 etc.

The MailBox
-----------

Each connected user has a mailbox that stores messages and notifies listeners of new messages.

Messages are stored using a struct within a module:

```elixir
defmodule Message do
  defstruct id: 0, data: nil
end
```

The state of the mailbox is defined by another struct:

```elixir
defmodule State do
  defstruct id: 0, cur_id: 0, listeners: [], messages: []
end
```

`listeners` is a list of `{message_id, process_id}` pairs. A listener is an Erlang
process plus an integer id corresponding the last message that the process got.

The mailbox is a plain old Erlang process (not a gen_server).

```elixir
def loop(state) do

  receive do
    
    {:add_listener, listener = {_msg_id, _pid}} ->
      new_state = %State{state | listeners: [listener | state.listeners]}
      new_state = notify_listeners(new_state)
      loop(new_state)

    {:remove_listener, pid} ->
      new_listeners = Enum.filter(state.listeners, fn({_id, p}) -> p != pid end)
      new_state = %State{ state | listeners: new_listeners }
      loop(new_state)

    {:get_state} ->
      IO.puts "State: #{inspect(state)}"
      loop(state)

    {:get_msg_id, pid} ->
      send pid, {:cur_msg_id, state.cur_id}
      loop(state)

    {:msg, data} ->
      msg = %Message{ id: state.cur_id, data: data}
      new_state = %State { state | messages: [msg | state.messages], cur_id: state.cur_id + 1 }
      new_state = notify_listeners(new_state)
      loop(new_state)
  end
  
end
```

I've added in an extra message to print out the state - it's useful for debugging.

Also, compared to Chris' original code, I'm not using proc:hibernate yet.

Listeners are notififed when a new listener is added and when a new message comes in:

```elixir
def notify_listeners(state) do
  new_listeners = Enum.filter(state.listeners, 
    fn({msg_id, pid}) ->
      case msg_id >= state.cur_id do
          true -> 
            true
          _ ->
            # Select messages that are greater than or equal to the requested ID
            case Enum.filter(state.messages, fn(msg) -> msg.id >= msg_id end) do
                [] -> 
                  true # no messages were found for this listener, keep it in the list
                m -> 
                  messages_to_send = Enum.map(m, fn(msg) -> {msg.id, msg.data} end) 
                  send pid, messages_to_send
                  false # remove it
            end
      end
    end)
  %State{ state | listeners: new_listeners}
end
```

Note that as soon as a listener has been notified of the list of messages it is 
removed from the list of listeners. This seems a bit odd until you look at the 
web code, in particular the AJAX call `getServiceMsg` in client.js. It issues a request
for the `/chat/wait/?msg_id=` page, and when it gets a reply, it handles it and then 
calls itself again recursively. 

A helper function kicks it off:

```elixir
def start(id) do
  loop(id)
end
```

Testing the Mailbox
-------------------

We can do some ad-hoc manual tests of the Mailbox: 

```elixir
~/proj/elixir/elixir_chat/part_1(master)$ iex -S mix
iex(1)> p = spawn(ChatMailbox, :start, [1])
#PID<0.51.0>
iex(2)> send p, {:get_state}
{:get_state}
State: %ChatMailbox.State{id: 1, cur_id: 0, listeners: [], messages: []}
iex(3)> send p, {:add_listener, {0, self}}
{:add_listener, {0, #PID<0.26.0>}}
iex(4)> send p, {:get_state}
{:get_state}
State: %ChatMailbox.State{id: 1, cur_id: 0, listeners: [{0, #PID<0.26.0>}], messages: []}
iex(5)>
```

However, use can also add some unit tests. In the file `chat_tutorial_test.exs`:

```elixir
  test "Sending a message to a mailbox" do
    p = spawn(ChatMailbox, :start, [0])
    send p, {:add_listener, {0, self}}
    send p, {:msg, "Hello world"}
    receive do
      m when is_list(m) ->
        [{id, message} | _ ] = m
        assert message == "Hello world"
      _ -> 
        assert(false)
    end
  end
```

The PostOffice
--------------

The post office manages multiple mailboxes, including their creation and
destruction, and the sending of messages to the mailboxes, either indiviually
or broadcast to all. 

The ChatPostOffice module is a gen_server. Its state is kept as an array
of mailboxes, each of which is a tuple containin the ID and a pid.

```elixir
defmodule ChatPostOffice do

  use GenServer.Behaviour

  defmodule State do
    defstruct mailboxes: []
  end

  def init(_args) do
    {:ok, %State{}}
  end
  ...
```

The initial API to the server has `start_link` and `create_mailbox` functions:

```elixir
  def start_link() do
    :gen_server.start_link {:local, :postoffice}, ChatPostOffice, [], []
  end

  def create_mailbox(id) do
    :gen_server.call :postoffice, {:create_mailbox, id}
  end
```

When creating a mailbox we need to check if one exists already:

```elixir
def get_mailbox(mailbox_id, state) do
  case Enum.filter(state.mailboxes, fn({id, _}) -> id == mailbox_id end) do
      [] -> 
        {:error, :notfound}
      [m|_] -> 
        {:ok, m}
  end
end
```

We handle the call for the `create_mailbox` API thus:

```elixir
def handle_call({:create_mailbox, id}, _from, state) do
  case get_mailbox(id, state) do
    {:ok, _} -> 
      {:reply, {:error, :already_exists}, state}
    {:error, :notfound} ->
      pid = spawn_link(ChatMailbox, :start, [id])
      new_mailbox = {id, pid}
      {:reply, :ok, %State{state | mailboxes: [new_mailbox | state.mailboxes] } } 
  end
end
```


Testing the PostOffice
----------------------

We can make a test to see if creating duplicate mailboxes gives an error:

```elixir
test "Create a mailbox" do
  ChatPostOffice.start_link()
  :ok = ChatPostOffice.create_mailbox(42)
  # Try creating it again
  {:error, status } = ChatPostOffice.create_mailbox(42)
  assert status == :already_exists
end
```

More PostOffice Functionality
-----------------------------

Deleting a mailbox is done using a `:gen_server.cast` instead of a `:gen_server.call`.

```elixir
def delete_mailbox(id) do
  :gen_server.cast :postoffice, {:delete_mailbox, id}
end

def handle_cast({:delete_mailbox, mailbox_id}, state) do
  new_boxes = Enum.filter(state.mailboxes, fn({id, pid}) ->
    case id != mailbox_id do
      false -> 
        # tell the mailbox process to quit
        send pid, :quit
        false
      _ -> 
        true
    end
  end)
  {:noreply, %State{ state | mailboxes: new_boxes}}
end
```

The corresponding test is:
```elixir
test "Delete a mailbox" do
  ChatPostOffice.start_link()
  :ok = ChatPostOffice.create_mailbox 43
  # Delete it
  ChatPostOffice.delete_mailbox 43
  # Delete it again - doesn't cause an error
  ChatPostOffice.delete_mailbox 43
end
```

Sending mail is also done using a cast:

```elixir
def send_mail(id, message) do
  :gen_server.cast(:postoffice, {:send_mail, {id, message}})
end

def handle_cast({:send_mail, {id, message}}, state) do
  case get_mailbox(id, state) do
    {:ok, {_id, pid}} -> 
      pid <- {:mail, message}
    _ -> 
      :ok
  end
  {:noreply, state}
end
```

The `send_mail` function in the post office is used in the chat room as a 
way of the room sending messages to the mailbox. The `message` isn't just a 
string - it's meant to be a tuple of {command, command_data}, and the 
command_data itself will be made up of other tuples with commands such 
as leave_room, join_room etc.

Here's a short test:

```elixir
test "Send some mail" do
  ChatPostOffice.start_link()
  :ok = ChatPostOffice.create_mailbox 44
  :ok = ChatPostOffice.send_mail 44, {:add_listener, {0, self}}
  :ok = ChatPostOffice.send_mail 44, {:msg, "Hello world"}
  receive do
    m when is_list m ->
      [{id, message} | _ ] = m
      assert message == "Hello world"
    _ -> 
      assert false
  end
  :ok = ChatPostOffice.send_mail 44, {:remove_listener, self}
end
```

Broadcasting a mail sends a message to all mailboxes in the post office:

```elixir
def broadcast_mail(message, except) do 
  :gen_server.cast(:postoffice, {:broadcast_mail, {message, except}})
end

def handle_cast({:broadcast_mail, {message, except}}, state) when is_list except do
  lc {id, pid} inlist state.mailboxes, Enum.member?(except, id) == false, do: pid <- message
  {:noreply, state};
end
```

However, I don't like the use of list comprehensions used in the original
Erlang code to perform an action on each member of the list of mailboxes. That
list is then discarded.

A more Elixir-like way to do this might look list this:

```elixir
def handle_cast({:broadcast_mail, {message, except}}, state) when is_list except do
  state.mailboxes 
  |> Enum.filter(fn({id, _}) -> Enum.member?(except, id) == false end)
  |> Enum.each(fn({_, pid}) -> pid <- message end)
  {:noreply, state};
end
```

NOTE: I originally didn't have parentheses for the Enum.filter and Enum.each calls:

```elixir
  |> Enum.filter fn({id, _}) -> Enum.member?(except, id) == false end
  |> Enum.each fn({_, pid}) -> pid <- message end
```

It compiled, but gave run-time errors about `:badaridy`.

We can test the broadcast:

```elixir
test "Broadcast some mail" do
  ChatPostOffice.start_link()
  :ok = ChatPostOffice.create_mailbox 45
  :ok = ChatPostOffice.send_mail 45, {:add_listener, {0, self}}
  :ok = ChatPostOffice.broadcast_mail {:msg, {:user_joined_room, "delboy"}}, []
  receive do
    m when is_list m ->
      [{_message_id, {:user_joined_room, "delboy"}} | _] = m
      assert true
    _ -> 
      assert false
  end
  :ok = ChatPostOffice.send_mail 45, {:remove_listener, self}
end
```

The Chat Room
-------------

The (for now, global) chat room is also a gen_server:

```elixir
defmodule ChatRoom do

  use GenServer.Behaviour

  defmodule ClientState do
    defstruct id: 0, nick: nil, host: nil, last_action: nil
  end

  defmodule State do
    defstruct clients: []
  end

  def init(_args) do
    {:ok, %State{}}
  end
 
  def start_link() do
    :gen_server.start_link({:local, :chatroom}, ChatRoom, [], [])
  end

end
```

The state is a list of clients defined by an id, a nickname, a hostname and 
the datetime of the last action.

The chat room allows users to join the room, leave the room, send a message, 
and allows the web front end to get a list of users, and wait for messages.

When joining a room we need to validate the nickname:

```elixir
defp validate_nick([], _) do
  {:error, :bad_format}
end

defp validate_nick(nick, state) do
  shortened = nick |> String.strip |> String.slice 0, 16 
  case {Regex.run(~r/^([A-Za-z0-9]+)$/, shortened), Enum.filter(state.clients, fn(client) -> client.nick == shortened end)} do
    {[shortened, shortened], []} -> {:ok, shortened}
    {[shortened, shortened], _} -> {:error, :not_available}
    {nil, []} -> {:error, :bad_format}
     _ -> {:error, :not_available}
  end
end
```

We can test this validation:

```elixir
test "Validate a nickname" do
  valid_nick = "granddad"

  delboy = %ChatRoom.ClientState{nick: "delboy"}
  rodney = %ChatRoom.ClientState{nick: "rodney"}

  clients = [delboy, rodney]
  state = %ChatRoom.State{clients: clients}

  # "grandad" is OK
  {:ok, valid_nick} = ChatRoom.validate_nick valid_nick, state

  # "delboy" is not available
  {:error, :not_available} = ChatRoom.validate_nick "delboy", state

  # Neither is "rodney"
  {:error, :not_available} = ChatRoom.validate_nick "rodney", state

  # "D@ve" is invalid
  {:error, :bad_format} = ChatRoom.validate_nick "D@ve", state

end
```

The `join` function looks like this:

```elixir
def join(nick, host) do
  :gen_server.call(:chatroom, {:join, {nick, host}}, :infinity)
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

def get_unique_session(state) do
  hash = ChatUtil.generate_hash
  case Enum.filter(state.clients, fn(client_state) -> client_state.id == hash end) do
    [] -> hash
    _ -> get_unique_session state
  end
end
```

There are a few utility functions in ChatUtil.ex that are used to generate SHA hashes.

To test the room functionality:

```elixir
test "User Joins Room" do
  ChatPostOffice.start_link()
  ChatRoom.start_link()
  {:ok, _session_id} = ChatRoom.join "delboy", "localhost"
  # You can't do it again
  {:error, :not_available} = ChatRoom.join "delboy", "localhost"
end
```

Note that I have to start both gen_servers manually. For normal operations, this is the 
responsibility of the supervisor - see later.

Leaving a room is a gen_server cast:

```elixir
def leave(session, reason) do
  :gen_server.cast :chatroom, {:leave, {session, reason}}
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
```

This works fine but it's difficult to unit-test. Genserver casts
are asynchronous, so the only value you'll get back is :ok.

```elixir
test "User Leaves Room" do
  ChatPostOffice.start_link()
  ChatRoom.start_link()
  {:ok, session_id} = ChatRoom.join "rodney", "localhost"
  :ok = ChatRoom.leave session_id, "Didn't like the language"
  # Try it again with the same session id
  :ok = ChatRoom.leave session_id, "Didn't like the language"
  # Try with an invalid session id
  :ok = ChatRoom.leave "invalid session", "Didn't like the language"
end
```

Sending a chat message is similar:


```elixir
def chat_message(session, message) do
  :gen_server.cast :chatroom, {:chat_message, {session, message}}
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

```

The test is as follows:

```elixir
test "Send a chat message" do
  ChatPostOffice.start_link()
  ChatRoom.start_link()
  {:ok, session_id} = ChatRoom.join "granddad", "localhost"
  ChatRoom.chat_message session_id, "How's it going Delboy?"
  :ok = ChatPostOffice.send_mail session_id, {:add_listener, {0, self}}
  receive do
    m when is_list m -> 
      [{_message_id, {:sent_chat_msg, {"granddad", "How's it going Delboy?"}}} | _] = m
    _ ->
      assert false
  end
end
```

More Chat Room Functionality
----------------------------

There are still a few more APIs to implement for the chat room, especially for the 
web client.

* Getting a list of users in the room
* Letting the web client get the current message id
* Allowing the web client to wait for a message
* Letting the web client finish this waiting
* Detecting idle users and disconnecting them.

The user list is straightforward enough:

```elixir
def get_users(session) do
  :gen_server.call :chatroom, {:get_users, {session}}, :infinity
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

test "Get the list of users" do
  ChatPostOffice.start_link()
  ChatRoom.start_link()
  {:ok, _session_id} = ChatRoom.join "dave", "localhost"
  {:ok, session_id} = ChatRoom.join "trigger", "localhost"
  case ChatRoom.get_users session_id do
    {:ok, m} when is_list m -> 
      assert length(m) == 2
      assert m |> Enum.sort |> Enum.at(0) == "dave"
      assert m |> Enum.sort |> Enum.at(1) == "trigger"
    _ ->
      assert false
  end
end
```

Getting the current message ID for the user's mailbox is similar:

```elixir
def get_msg_id(session, pid) do
  :gen_server.cast :chatroom, {:get_msg_id, {session, pid}}
end

def handle_cast({:get_msg_id, {session, pid}}, state) do
  case get_session(session, state) do
    {:error, :not_found} -> 
      pid <- {:error, :bad_session}
      {:noreply, state}
    {:ok, client} -> 
      new_state = update_client(client, state)
      ChatPostOffice.send_mail session, {:get_msg_id, pid} 
      {:noreply, new_state}
  end
end

test "Get the message ID for the current user" do
  ChatPostOffice.start_link()
  ChatRoom.start_link()
  {:ok, session_id} = ChatRoom.join "albert", "localhost"
  :ok = ChatRoom.get_msg_id 'badsession', self 
  receive do
    {:error, :bad_session} ->
      true
  end
  :ok = ChatRoom.get_msg_id session_id, self 
  receive do
    {:cur_msg_id, x} when is_integer x ->
      true
  end
end
```

Waiting for a chat message involves having a process able to receive a chat 
message for a particular user.

```elixir
def wait(session, message_id, pid) do
  :gen_server.cast :chatroom, {:wait, {session, message_id, pid}}
end

def handle_cast({:wait, {session, message_id, pid}}, state) do
  case get_session(session, state) do
    {:error, :not_found} -> 
      pid <- {:error, :bad_session}
      {:noreply, state}
    {:ok, client} -> 
      new_state = update_client(client, state)
      ChatPostOffice.send_mail session, {:add_listener, {message_id, pid}}
      {:noreply, new_state}
  end
end

test "Wait for a chat message" do
  ChatPostOffice.start_link()
  ChatRoom.start_link()
  {:ok, boice_session_id} = ChatRoom.join "boice", "localhost"
  {:ok, denzil_session_id} = ChatRoom.join "denzil", "localhost"
  ChatRoom.chat_message boice_session_id, "How's it going Denzil?"
  ChatRoom.wait denzil_session_id, 0, self
  receive do
    m when is_list m -> 
      [{_message_id, {:chat_msg, {"boice", "How's it going Denzil?"}}} | _] = m
    _ ->
      assert false
  end
  ChatRoom.chat_message denzil_session_id, "Not bad. Have you seen Delboy?"
  ChatRoom.wait boice_session_id, 0, self
  receive do
    m when is_list m -> 
      [{_message_id, {:chat_msg, {"denzil", "Not bad. Have you seen Delboy?"}}} | _] = m
    _ ->
      assert false
  end
end  
```

Because of the way the web client works, we need to let the chat room know we're not
waiting any more once we get a message. This effectively removes the client from the
listeners in the mailbox.

```elixir
def wait_finish(session, pid) do
  :gen_server.cast :chatroom, {:wait, {session, pid}}
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

test "Stop waiting for a chat message" do
  ChatPostOffice.start_link()
  ChatRoom.start_link()
  {:ok, raquel_session_id} = ChatRoom.join "raquel", "localhost"
  {:ok, cassandra_session_id} = ChatRoom.join "cassandra", "localhost"
  ChatRoom.chat_message raquel_session_id, "Where is Rodney that plonker?"
  ChatRoom.wait cassandra_session_id, 0, self
  receive do
    m when is_list m -> 
      [{_message_id, {:chat_msg, {"raquel", "Where is Rodney that plonker?"}}} | _] = m
    _ ->
      assert false
  end
  ChatRoom.wait_finish cassandra_session_id, self
  # See if there are any more messages - timeout after 1 second
  receive do
    _ -> 
      assert false
    after 1000 ->
      true
  end
end
```

Note that in the test after the call to `wait_finish` we see if there are any more
messages and timeout after 1000 milliseconds, which should be good enough.

The last bit is to find any idle clients and disconnect them.

```elixir
def find_idle_clients() do
  :gen_server.cast :chatroom, {:find_idle_clients, {}}
end

def handle_cast(:find_idle_clients, state) do
  {:noreply, state}
end

def handle_cast({:find_idle_clients, {}}, state) do
  Enum.each(state.clients,
    fn(client) ->
      last_action = :calendar.now_to_datetime client.last_action
      now = :erlang.now |> :calendar.now_to_datetime
      idle_seconds =  :calendar.datetime_to_gregorian_seconds(now) - :calendar.datetime_to_gregorian_seconds(last_action)
      case idle_seconds > max_idle_time do
        true -> 
          #IO.puts "User timed out: #{client.nick}, secs: #{idle_seconds}"
          :timer.apply_after(0, __MODULE__, :leave, [client.id, "timeout"])
        _ -> :noop
      end
    end)
  :timer.apply_after(check_idle_time, __MODULE__, :find_idle_clients, [])
  {:noreply, state}
end
```

This is called in the ChatRoom `init` function:

```elixir
def max_idle_time, do: 2    # seconds
def check_idle_time, do: 1000 # Milliseconds

def init(_args) do
  :erlang.process_flag(:trap_exit, true)
  :timer.apply_after(check_idle_time, ChatRoom, :find_idle_clients, [])
  {:ok, State.new}
end
```

Note for testing I've set the max idle time to only 2 seconds. It might be better
to have the max time configurable - i.e. create a new API to set the time and 
store it in the state.

A Note on Unit Test Setup and Teardown
--------------------------------------

I have a major problem in my tests which I've skirted till now. You might notice
that whenever I add someone to a room I'm using unique names.

Sometimes all the tests pass. Running `mix test` again and then some fail.

The reason is to do with the way the tests that use any of the gen servers are
written. I've been sloppy and put an explicit `start_link` in each test. 
However, if test A does this and then test B does it, the gen_servers are
already started. What we really want to do is to start and stop the servers
for each test.

The way this is done in ExUnit is to use the `setup` and `teardown` callbacks:

```elixir
setup do
  ChatPostOffice.start_link()
  ChatRoom.start_link()
  :ok
end

teardown _meta do
  ChatPostOffice.stop()
  ChatRoom.stop()
  :ok
end
```

We also need to handle stop messages in each gen_server:

```elixir
# In ChatRoom
def stop() do
  :gen_server.cast :chatroom, {:stop, {}}
end

def handle_cast({:stop, {}}, state) do
  {:stop, :normal, state}
end

# In ChatPostOffice
def stop() do
  :gen_server.cast :postoffice, {:stop, {}}
end

def handle_cast({:stop, {}}, state) do
  {:stop, :normal, state}
end
```

And we need to remove all explicit `start_link` calls in the tests.


What's Next
-----------

That's it for part 1. In part 2 we'll add the web client.
