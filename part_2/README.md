# ChatTutorial - Part 1

This is part 2 of my Elixir Chat Server based on Chris Moos's
[Building an Erlang Chat Server with Comet](http://www.chrismoos.com/2009/09/28/building-an-erlang-chat-server-with-comet-part-1)
the source of which can be found on [Github](https://github.com/chrismoos/erl_chat_tutorial).

In [Part 1] we ported the original Erlang code to Elixir. In this section
we'll add OTP items such as applications and supervisors, and then add
the web server and client using Mochiweb.

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


