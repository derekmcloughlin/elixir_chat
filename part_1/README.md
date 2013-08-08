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

Messages are stored using a record:

```elixir
defrecord Message, id: 0, data: nil
```

The state of the mailbox is defined by another record:

```elixir
defrecord State, id: 0, cur_id: 0, listeners: [], messages: []
```

`listeners` is a list of `{message_id, process_id}` pairs. A listener is an Erlang
process plus an integer id corresponding the last message that the process got.

The mailbox is a plain old Erlang process (not a gen_server).

```elixir
def loop(state) do
  receive do
    {:add_listener, listener = {_msg_id, _pid}} ->
      new_state = state.update_listeners(fn(old_listeners) -> [listener | old_listeners] end)
      new_state = notify_listeners(new_state)
      loop(new_state)
    {:remove_listener, pid} ->
      new_listeners = Enum.filter(state.listeners, fn({_id, p}) -> p != pid end)
      new_state = state.update_listeners(fn(_old_listeners) -> new_listeners end)
      loop(new_state)
    {:get_state} ->
      IO.puts "State: #{inspect(state)}"
      loop(state)
    {:get_msg_id, pid} ->
      pid <- {:cur_msg_id, state.cur_id}
      loop(state)
    {:msg, data} ->
      msg = Message.new id: state.cur_id, data: data
      new_state = state.update_messages(fn(old_messages) -> [msg | old_messages] end)
      new_state = new_state.update_cur_id(fn(old_cur_id) -> old_cur_id + 1 end)
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
                  pid <- messages_to_send
                  false # remove it
            end
      end
    end)
  state.update_listeners(fn(_old_listeners) -> new_listeners end)
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
iex(2)> p <- {:get_state}
{:get_state}
State: ChatMailbox.State[id: 1, cur_id: 0, listeners: [], messages: []]
iex(3)> p <- {:add_listener, {0, self}}
{:add_listener, {0, #PID<0.26.0>}}
iex(4)> p <- {:get_state}
{:get_state}
State: ChatMailbox.State[id: 1, cur_id: 0, listeners: [{0, #PID<0.26.0>}], messages: []]
iex(5)>
```

However, use can also add some unit tests. In the file `chat_tutorial_test.exs`:

```elixir
  test "Sending a message to a mailbox" do
    p = spawn(ChatMailbox, :start, [0])
    p <- {:add_listener, {0, self}}
    p <- {:msg, "Hello world"}
    receive do
      m when is_list(m) ->
        [{id, message} | _ ] = m
        assert(message == "Hello world")
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

  defrecord State, mailboxes: []

  def init(_args) do
    {:ok, State.new}
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
        {:reply, :ok, State.new mailboxes: [new_mailbox | state.mailboxes]}
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
  {:error, :already_exists} = ChatPostOffice.create_mailbox(42)
  assert(true)
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
  #state{mailboxes=MBoxes} = State) ->
  new_boxes = Enum.filter(state.mailboxes, fn({id, pid}) ->
      case id != mailbox_id do
          false -> 
            # tell the mailbox process to quit
            pid <- :quit
            false
          _ -> 
            true
      end
  end)
  {:noreply, State.new mailboxes: new_boxes}
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
  assert(true)
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
      assert(message == "Hello world")
    _ -> 
      assert false
  end
  :ok = ChatPostOffice.send_mail 44, {:remove_listener, self}
  assert(true)
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
  assert(true)
end
```

The Chat Room
-------------

The (for now, global) chat room is also a gen_server:

```elixir
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
  case {Regex.run(%r/^([A-Za-z0-9]+)$/, shortened), Enum.filter(state.clients, fn(client) -> client.nick == shortened end)} do
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

  delboy = ChatRoom.ClientState.new nick: "delboy"
  rodney = ChatRoom.ClientState.new nick: "rodney"

  clients = [delboy, rodney]
  state = ChatRoom.State.new clients: clients

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
