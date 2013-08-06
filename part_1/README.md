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




