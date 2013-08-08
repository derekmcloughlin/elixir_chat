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


  test "Validate a nickname" do
    delboy = ChatRoom.ClientState.new nick: "delboy"
    rodney = ChatRoom.ClientState.new nick: "rodney"

    clients = [delboy, rodney]
    state = ChatRoom.State.new clients: clients

    # "grandad" is OK
    {:ok, "granddad"} = ChatRoom.validate_nick "granddad", state

    # "delboy" is not available
    {:error, :not_available} = ChatRoom.validate_nick "delboy", state

    # Neither is "rodney"
    {:error, :not_available} = ChatRoom.validate_nick "rodney", state

    # "D@ve" is invalid
    {:error, :bad_format} = ChatRoom.validate_nick "D@ve", state
  end

  test "User Joins Room" do
    ChatPostOffice.start_link()
    ChatRoom.start_link()
    {:ok, _session_id} = ChatRoom.join "delboy", "localhost"
    # You can't do it again
    {:error, :not_available} = ChatRoom.join "delboy", "localhost"
  end

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

  test "Send a chat message" do
    ChatPostOffice.start_link()
    ChatRoom.start_link()
    {:ok, session_id} = ChatRoom.join "granddad", "localhost"
    ChatRoom.chat_message session_id, "How's it going Delboy?"
    :ok = ChatPostOffice.send_mail session_id, {:add_listener, {0, self}}
    receive do
      m when is_list m -> 
        [{_message_id, {:sent_chat_msg, {"granddad", "How's it going Delboy?"}}} | _] = m
        assert true
      _ ->
        assert false
    end
  end

end
