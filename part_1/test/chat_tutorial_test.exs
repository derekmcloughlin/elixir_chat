import :timer, only: [ sleep: 1 ]

defmodule ChatTutorialTest do
  use ExUnit.Case


  setup do
    IO.write "Starting servers..."
    ChatPostOffice.start_link()
    ChatRoom.start_link()
    IO.puts "... started"
    :ok
  end

  teardown _meta do
    IO.write "Stopping servers..."
    ChatPostOffice.stop()
    ChatRoom.stop()
    IO.puts "... stopped"
    :ok
  end


  test "Sending a message to a mailbox" do
    p = spawn ChatMailbox, :start, [0] 
    send p, {:add_listener, {0, self}}
    send p, {:msg, "Hello world"}
    receive do
      m when is_list m ->
        [{_id, message} | _ ] = m
        assert message == "Hello world"
      _ -> 
        assert false
    end
  end

  test "Create a mailbox" do
    :ok = ChatPostOffice.create_mailbox 42
    # Try creating it again
    {:error, status } = ChatPostOffice.create_mailbox(42)
    assert status == :already_exists
  end

  test "Delete a mailbox" do
    :ok = ChatPostOffice.create_mailbox 43
    # Delete it
    ChatPostOffice.delete_mailbox 43
    # Delete it again - doesn't cause an error
    ChatPostOffice.delete_mailbox 43
  end

  test "Send some mail" do
    :ok = ChatPostOffice.create_mailbox 44
    :ok = ChatPostOffice.send_mail 44, {:add_listener, {0, self}}
    :ok = ChatPostOffice.send_mail 44, {:msg, "Hello world"}
    receive do
      m when is_list m ->
        [{_id, message} | _ ] = m
        assert message == "Hello world"
      _ -> 
        assert false
    end
    :ok = ChatPostOffice.send_mail 44, {:remove_listener, self}
  end

  test "Broadcast some mail" do
    :ok = ChatPostOffice.create_mailbox 45
    :ok = ChatPostOffice.send_mail 45, {:add_listener, {0, self}}
    :ok = ChatPostOffice.broadcast_mail {:msg, {:user_joined_room, "delboy"}}, []
    receive do
      m when is_list m ->
        [{_message_id, {:user_joined_room, "delboy"}} | _] = m
      _ -> 
        assert false
    end
    :ok = ChatPostOffice.send_mail 45, {:remove_listener, self}
  end


  test "Validate a nickname" do
    delboy = %ChatRoom.ClientState{nick: "delboy"}
    rodney = %ChatRoom.ClientState{nick: "rodney"}

    clients = [delboy, rodney]
    state = %ChatRoom.State{clients: clients}

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
    {:ok, _session_id} = ChatRoom.join "delboy", "localhost"
    # You can't do it again
    {:error, :not_available} = ChatRoom.join "delboy", "localhost"
  end

  test "User Leaves Room" do
    {:ok, session_id} = ChatRoom.join "rodney", "localhost"
    :ok = ChatRoom.leave session_id, "Didn't like the language"
    # Try it again with the same session id
    :ok = ChatRoom.leave session_id, "Didn't like the language"
    # Try with an invalid session id
    :ok = ChatRoom.leave "invalid session", "Didn't like the language"
  end

  test "Send a chat message" do
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

  test "Get the list of users" do
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

  test "Get the message ID for the current user" do
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

  test "Wait for a chat message" do
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

  test "Stop waiting for a chat message" do
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


  # This is commented out for the moment
  #test "Find Idle Clients" do
    #{:ok, _micky_session_id} = ChatRoom.join "micky", "localhost"
    #sleep 8000
    ## Micky should have timed out after 2 seconds
    #{:ok, angela_session_id} = ChatRoom.join "angela", "localhost"
    #case ChatRoom.get_users angela_session_id do
      #{:ok, m} when is_list m -> 
        #assert length(m) == 1
        #assert m |> Enum.sort |> Enum.at(0) == "angela"
      #_ ->
        #assert false
    #end
  #end

end
