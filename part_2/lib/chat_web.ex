defmodule ChatWeb do


  def comet_timeout, do: 90000

  def start_link() do
    :mochiweb_http.start([{:port, 8000}, {:loop, {__MODULE__, :loop}}])
  end

  def loop(req) do
    case req.get(:version) do
      version when version >= {1, 0} ->
        path = to_string req.get(:path)
        IO.puts "Path = '#{path}'\n"
        handle_request req, path
      _ -> :ok
    end
  end

  def handle_request(req, "/") do
    IO.puts "Root"
    templ = ChatUtil.get_template("index", [])
    IO.puts "Template = '#{templ}'"
    html_ok req, templ
  end

  def handle_request(req, "/login/") do
    post = req.parse_post()
    case ChatRoom.join(ChatUtil.get_parameter('nick', post), req.get(:peer)) do
      {:ok, sess_id} -> 
        sess_cookie = :mochiweb_cookies.cookie('chat_sess', sess_id, [{:path, "/"}])
        req.respond({302, [sess_cookie, {'Location', '/chat'}], <<>>})
      {:error, :too_many_conns} -> 
        html_ok(req, ChatUtil.get_template("index", [{:error, "Your host has too many connections to the chat server."}]))
      {:error, :not_available} -> 
        html_ok(req, ChatUtil.get_template("index", [{:error, "The nickname is not available."}]))
      _ ->
        html_ok(req, ChatUtil.get_template("index", [{:error, "The nickname must be alphanumeric and not blank."}]))
    end
  end

  def handle_request(req, "/chat") do
    html_ok req, ChatUtil.get_template("chat", [])
  end

  def handle_request(req, "/chat/online/") do
    case ChatRoom.get_users(get_session(req)) do
      {:ok, users} -> 
        json_respond(json_client_ok(users), req)
      _ -> 
        bad_session(req)
    end
  end

  def handle_request(req, "/chat/start/") do
    ChatRoom.get_msg_id(get_session(req), self())
    :proc_lib.hibernate(__MODULE__, :wait_msg_id, [req])
  end

  def handle_request(req, "/chat/wait/") do
    msg_id = ChatUtil.get_parameter_int('msg_id', req.parse_qs())
    ChatRoom.wait(get_session(req), msg_id, self())
    :timer.apply_after(comet_timeout, __MODULE__, :timeout_wait, [self()])
    :proc_lib.hibernate(__MODULE__, :wait, [req])
  end

  def handle_request(req, "/chat/send_msg/") do
    ChatRoom.chat_message(get_session(req), ChatUtil.get_parameter('msg', req.parse_post()))
    json_respond(json_client_ok(<<"">>), req)
  end
 
  def handle_request(req, path) do
    {_, file_name} = String.slice(path, 1, 100) |> String.to_char_list
    req.serve_file(file_name, 'docroot', [])
  end

  def get_session(req) do
    ChatUtil.get_parameter('chat_sess', req.parse_cookie())
  end

  defp html_ok(req, data) do
    req.ok({"text/html;charset=UTF-8", data})
  end

  def json_respond(msg, req) do
    req.ok({"text/json", [], msg})
  end

  def bad_session(req) do
    json_respond(json_client_error(<<"bad_session">>), req)
  end

  def json_client_ok(msg) do
    List.flatten(:rfc4627.encode({:obj, [{"status", <<"ok">>}, {:response, msg}]}))
  end

  def json_client_error(msg) do
    Lists.flatten(:rfc4627.encode({:obj, [{"status", <<"error">>}, {:response, msg}]}))
  end

  def wait_msg_id(req) do
    receive do
      {:cur_msg_id, msg_id} -> 
        json_respond(json_client_ok(msg_id), req)
      _ -> 
        bad_session(req)
    end
  end

  def wait(req) do 
    receive do
      :timeout -> 
        json_respond(json_client_ok(<<"reconnect">>),Req)
      items when is_list(items) -> 
        msgs = List.flatten(Enum.map(items, fn(x) -> format_message(x) end))
        json_respond(json_client_ok(msgs), req)
      _ -> 
        bad_session(req)
    end
    ChatRoom.wait_finish(get_session(req), self())
  end

  def timeout_wait(pid) do
    send pid, :timeout    
  end

  def format_message({id, {type, data}}) do
    {:obj, [{"id", id}, 
            {"t", :erlang.list_to_binary(atom_to_list(type))}, 
            {"d", format_data(type, data)}]}
  end

  def format_message(_) do
    []
  end

  def format_data(:admin_logged_out, nick) do
    nick
  end
  def format_data(:admin_logged_in, nick) do
    nick
  end
  def format_data(:user_left_room, {nick, reason}) do
    {:obj, [{"nick", nick}, {"reason", reason}]}
  end
  def format_data(:user_joined_room, nick) do
    nick
  end
  def format_data(:system_msg, msg) do
    msg
  end
  def format_data(x, {nick, msg}) when (x == :chat_msg) or (x == :sent_chat_msg) do
    {:obj, [{"nick", nick}, {"msg", msg}]}
  end
  def format_data(_, _) do
    []
  end


end


