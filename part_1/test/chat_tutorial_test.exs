defmodule ChatTutorialTest do
  use ExUnit.Case

  test "Sending a message to a mailbox" do
    p = spawn ChatMailbox, :start, [0] 
    p <- {:add_listener, {0, self}}
    p <- {:msg, "Hello world"}
    receive do
      m when is_list m ->
        [{_id, message} | _ ] = m
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
    ChatPostOffice.start_link()
    :ok = ChatPostOffice.create_mailbox 44
    :ok = ChatPostOffice.send_mail 44, {:add_listener, {0, self}}
    :ok = ChatPostOffice.send_mail 44, {:msg, "Hello world"}
    receive do
      m when is_list m ->
        [{_id, message} | _ ] = m
        assert(message == "Hello world")
      _ -> 
        assert false
    end
    :ok = ChatPostOffice.send_mail 44, {:remove_listener, self}
    assert(true)
  end

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
end
