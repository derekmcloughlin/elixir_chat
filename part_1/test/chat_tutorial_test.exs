defmodule ChatTutorialTest do
  use ExUnit.Case

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

end
