defmodule ChatMailbox do

  defmodule Message do
    defstruct id: 0, data: nil
  end

  defmodule State do
    defstruct id: 0, cur_id: 0, listeners: [], messages: []
  end

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

  def start(id) do
    #:proc_lib.hibernate(__ENV__.module, :loop, [State.new id: id])
    #spawn(__ENV__.module, :loop, [State.new id: id])
    loop(%State{id: id})
  end

end
