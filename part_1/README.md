# ChatTutorial - Part 1

This is an Elixir Chat Server based on Chris Moos's
[Building an Erlang Chat Server with Comet](http://www.chrismoos.com/2009/09/28/building-an-erlang-chat-server-with-comet-part-1)
the source of which can be found on [Github](https://github.com/chrismoos/erl_chat_tutorial).

The MailBox
===========

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
    new_state = state.update_listeners(fn(old_listeners) -> new_listeners end)
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
    new_state = state.update_cur_id(fn(old_cur_id) -> old_cur_id + 1 end)
    new_state = notify_listeners(new_state)
    loop(new_state)
end
end
```

