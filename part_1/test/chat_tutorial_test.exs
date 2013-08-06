defmodule ChatTutorialTest do
  use ExUnit.Case

  test "Sending a message to a mailbox" do
    p = spawn ChatMailbox, :start, [0] 
    p <- {:add_listener, {0, self}}
    p <- {:msg, "Hello world"}
    receive do
      m when is_list m ->
        [{id, message} | _ ] = m
        assert(message == "Hello world")
      _ -> 
        assert false
    end
  end

  test "Create a mailbox" do
    ChatPostOffice.start_link()
    :ok = ChatPostOffice.create_mailbox 42
    # Try creating it again
    {:error, :already_exists} = ChatPostOffice.create_mailbox(42)
    assert(true)
  end

  test "Delete a mailbox" do
    ChatPostOffice.start_link()
    :ok = ChatPostOffice.create_mailbox 43
    # Delete it
    ChatPostOffice.delete_mailbox 43
    # Delete it again - doesn't cause an error
    ChatPostOffice.delete_mailbox 43
    assert(true)
  end

  test "Send some mail" do

    assert(true)
  end
end
