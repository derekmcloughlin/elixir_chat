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


