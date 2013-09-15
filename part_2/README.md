# ChatTutorial - Part 2

This is part 2 of my Elixir Chat Server based on Chris Moos's
[Building an Erlang Chat Server with Comet](http://www.chrismoos.com/2009/09/28/building-an-erlang-chat-server-with-comet-part-1)
the source of which can be found on [Github](https://github.com/chrismoos/erl_chat_tutorial).

In [Part 1](https://github.com/derekmcloughlin/elixir_chat/tree/master/part_2)
we ported the original Erlang code to Elixir. In this section
we'll add OTP items such as applications and supervisors, and then add
the web server and client using [Mochiweb](https://github.com/mochi/mochiweb).

OTP Applications
----------------

When we compile our chat server, a file called XXX.app is generated. This is 
the OTP application specification file. Part of the configuration is in
the `mix.exs` file, where we can specify to OTP the application entry point and
registered name.

~~~elixir
def application do
[
  mod: { ChatRoom, []},
  registered: [ :chat_room ]
]
end
~~~

If we now run `mix` we get:

~~~sh
lib/chat_postoffice.ex:59: clauses for the same def should be grouped together, def handle_cast/2 was previously defined (lib/chat_postoffice.ex:44)
Compiled lib/chat_postoffice.ex
Compiled lib/chat_mailbox.ex
Compiled lib/chat_tutorial.ex
Compiled lib/chat_util.ex
Compiled lib/chat_room.ex
Generated part_2.app

=INFO REPORT==== 25-Aug-2013::13:53:09 ===
    application: part_2
    exited: {bad_return,
                {{'Elixir.ChatRoom',start,[normal,[]]},
                 {'EXIT',
                     {undef,
                         [{'Elixir.ChatRoom',start,[normal,[]],[]},
                          {application_master,start_it_old,4,
                              [{file,"application_master.erl"},
                               {line,269}]}]}}}}
    type: temporary
** (Mix) Could not start application part_2: {:bad_return, {{ChatRoom, :start, [:normal, []]}, {:EXIT, {:undef, [{ChatRoom, :start, [:normal, []], []}, {:application_master, :start_it_old, 4, [file: 'application_master.erl', line: 269]}]}}}}
~~~

To fix this error, we need to add a `start` function to the ChatRoom to kick things off.

We could do this directly, but there's a better way: use OTP supervisors.

OTP Supervisors
---------------

We'll create a new file for the server: this is the main application entry point.

~~~elixir
defmodule ChatServer do

  use Application.Behaviour

  def start(_type, _args) do 
    ChatSupervisor.start_link()
  end

  def stop(_state) do
    :ok
  end

end
~~~

The supervisor starts the chat room and post office servers:


~~~elixir
defmodule ChatSupervisor do 
  use Supervisor.Behaviour

  def start_link() do 
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    child_processes = [worker(ChatRoom, []), worker(ChatPostOffice, [])] 
    supervise child_processes, strategy: :one_for_one
  end 
end
~~~

The configuration for the OTP application in `mix.exs` is as follows:

~~~elixir
# Configuration for the OTP application
def application do
[
  mod: { ChatServer, []},
  registered: [ :chat_server ]
]
end
~~~

Now running `iex -S mix` gives:

~~~elixir
~/proj/elixir/elixir_chat/part_2(master)$ iex -S mix
lib/chat_postoffice.ex:59: clauses for the same def should be grouped together, def handle_cast/2 was previously defined (lib/chat_postoffice.ex:44)
Compiled lib/chat_postoffice.ex
Compiled lib/chat_mailbox.ex
Compiled lib/chat_server.ex
Compiled lib/chat_supervisor.ex
Compiled lib/chat_tutorial.ex
Compiled lib/chat_util.ex
Compiled lib/chat_room.ex
Generated part_2.app
Erlang R16B01 (erts-5.10.2) [source-bdf5300] [64-bit] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]

Interactive Elixir (0.10.2-dev) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> ChatPostOffice.create_mailbox(42)
:ok
iex(2)> ChatPostOffice.create_mailbox(42)
{:error, :already_exists}
iex(3)>
~~~

Note that we don't have to start the post office or chat room servers explicitly.

Side Note
---------

There were warnings from the compiler output about handle_cast and handle_call 
being previously defined. The reason for this is that functions with the same 
name and arity must be grouped together, whereas I've added them as I came 
across them. They're fixed now.

MochiWeb
--------

To use MochiWeb we need to add it as a dependency of our application. This is
done in the `deps` section of the `mix.exs` file:

~~~elixir
defp deps do
[
  { :mochiweb, "2.7.0", git: "https://github.com/mochi/mochiweb" }
]
end
~~~

Running `mix deps.get` downloads this version and compiles it.

With that in place we can add a module to handle web requests. Here's a simple start:

~~~elixir
defmodule ChatWeb do

  def start_link() do
    :mochiweb_http.start([{:port, 8000}, {:loop, {__MODULE__, :loop}}])
  end

  def loop(req) do
    case req.get(:version) do
      version when version >= {1, 0} ->
        path = to_string req.get(:path)
        IO.puts "Path = #{path}\n"
        handle_request req, path
      _ -> :ok
    end
  end

  def handle_request(req, "/") do
    html_ok req, "<p>Getting index.html</p>"
  end

  def handle_request(req, "/test") do
    html_ok req, "<p>TESTING</p>"
  end

  def handle_request(req, path) do
    html_ok req, "<p>Got request for '#{path}'</p>"
  end
  defp html_ok(req, data) do
    req.ok({"text/html;charset=UTF-8", data})
  end

end
~~~

We need to modify the supervisor module to start the web server:

~~~elixir
def init([]) do
  child_processes = [
    worker(ChatRoom, []), 
    worker(ChatPostOffice, []),
    worker(ChatWeb, [])] 
  supervise child_processes, strategy: :one_for_one
end 
~~~

Running the server lets us browse to http://localhost:8000. We'll see the web request
in the browser output.

~~~
NOTE: I had to use `to_string` when getting the path, otherwise the pattern matching
in handle_request wouldn't work. I think this is because strings in Elixir are UTF-8
while the ones in Erlang aren't.
~~~

Using Erlydtl
-------------

The original code uses [Erlydtl](https://github.com/evanmiller/erlydtl) as the
templating language.

I couldn't get Erlydtl as a git dependency in the project, so I ended up 
manually cloning the Erlydtl repository and adding this to the deps section:

~~~elixir
  { :erlydtl, path: "deps/erlydtl"},
~~~

There's a helper function in the ChatUtil module to load templates

~~~elixir
def get_template(name, vars) do
  :ok = :erlydtl.compile('templates/#{name}.html', binary_to_atom(name))
  {:ok, tpl} = apply(binary_to_atom(name), :render, [vars])
  String.from_char_list!(tpl)
end
~~~

Note the use of the single-quoted string when passing the template name to the
`:erlydtl.compile` function. Also note that `name` is an Elixir string, not an Erlang
one, so we need to use `binary_to_atom` instead of `list_to_atom` as was in the original
Erlang code.


The root URL can be coded now as:

~~~elixir
def handle_request(req, "/") do
  html_ok req, ChatUtil.get_template("index", [])
end
~~~

Running this code you should now see the main login page.

Getting Request Parameters
--------------------------

The web server has to handle post requests and their data. There's a utility function
in ChatUtil that gets a parameter from the post for us:

~~~elixir
def get_parameter(n, [{k,v}|_]) when k == n do 
  v
end

def get_parameter(n, [_|t]) do
  get_parameter(n, t)
end

def get_parameter(_, _) do
  []
end
~~~

As an example we could have this in the login handler:

~~~elixir
def handle_request(req, "/login/") do
  post = req.parse_post()
  nick = ChatUtil.get_parameter('nick', post)
  html_ok req, "<p>Your nickname is #{nick}"
end
~~~

NOTE: You need to use the single-quoted string `'nick'` instead of a double-quoted
one `"nick"`. I've also used to_string to return the result as an Elixir string.

With that in place, the login handler now looks like:

~~~elixir
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
~~~

You should be able to login now. You won't see the list of users - that's next. In order
to do this I'll add the `rfc4627` library for JSON encoding into the deps file:

~~~elixir
  { :rfc4627_jsonrpc, "0.01", git: "https://github.com/tonyg/erlang-rfc4627"},
~~~ 

### Strings and things ###
However, there is a slight problem. Strings in Elixir are UTF-8 binary strings in Erlang.

~~~elixir
"Hello"     # Elixir string or Erlang binary string
'Hello'     # Erlang string or list of numbers
~~~

I've been a bit lax in mixing the two and it's going to cause problems. One area is
in the use of a string for the session id.

~~~elixir
def generate_hash() do
  :crypto.hash(:sha, generate_string(16))
  |> bin_to_hexstr
end
~~~

This produces an Erlang string, but the `get_parameter` function above returns
an Elixir string. Best to be consistent and use Elxir strings whenever we
can and Erlang strings when we have to communicate with Erlang functions.

~~~elixir
def generate_hash() do
  :crypto.hash(:sha, generate_string(16))
  |> bin_to_hexstr
  |> to_string
end
~~~

After Login
-----------

After logging in the server redirects to the `/chat` path:

~~~elixir
def handle_request(req, "/chat") do
  html_ok req, ChatUtil.get_template("chat", [])
end
~~~

The chat template loads up `client.js`. The client connects to the chat service
via the `/chat/start` path.  This involves getting the current message id 
and waiting for a response and sending it back to the client. 

~~~elixir
def handle_request(req, "/chat/start/") do
  ChatRoom.get_msg_id(get_session(req), self())
  wait_msg_id(req)
end

def wait_msg_id(req) do
  receive do
    {:cur_msg_id, msg_id} -> 
      json_respond(json_client_ok(msg_id), req)
    _ -> 
      bad_session(req)
  end
end
~~~

Once the client gets this, it asks for the list of online users:

~~~elixir
def handle_request(req, "/chat/online/") do
  case ChatRoom.get_users(get_session(req)) do
    {:ok, users} -> 
      json_respond(json_client_ok(users), req)
    _ -> 
      bad_session(req)
  end
end
~~~

The JSON helper functions are:

~~~elixir
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
~~~

Waiting for and Sending Chat Messages
-------------------------------------

The client issues a request to `/chat/wait/` to wait for messages.

~~~elixir
def handle_request(req, "/chat/wait/") do
  msg_id = ChatUtil.get_parameter_int('msg_id', req.parse_qs())
  ChatRoom.wait(get_session(req), msg_id, self())
  :timer.apply_after(comet_timeout, __MODULE__, :timeout_wait, [self()])
  :proc_lib.hibernate(__MODULE__, :wait, [req])
end
~~~

The `wait` function simply enters a receive block to get the message.

~~~elixir
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
~~~

Timeouts are handled after the period specified by `comet_timeout` where the
`timeout_wait` function is called:

~~~elixir
def timeout_wait(pid) do
  pid <- :timeout    
end
~~~

Various formatting functions are:

~~~elixir
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
~~~

Finally, to send a chat message, the client uses the `/chat/send_msg/` request:

~~~elixir
def handle_request(req, "/chat/send_msg/") do
  ChatRoom.chat_message(get_session(req), ChatUtil.get_parameter('msg', req.parse_post()))
  json_respond(json_client_ok(<<"">>), req)
end
~~~

What's Next
-----------

Now that we have a working chat server the next step is to add multiple rooms.




