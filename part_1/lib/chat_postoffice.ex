defmodule ChatPostOffice do

  use GenServer.Behaviour

  defrecord State, mailboxes: []

  def init(_args) do
    {:ok, State.new}
  end

  def start_link() do
    :gen_server.start_link {:local, :postoffice}, ChatPostOffice, [], []
  end

  def create_mailbox(id) do
    :gen_server.call :postoffice, {:create_mailbox, id}
  end

  def get_mailbox(mailbox_id, state) do
    case Enum.filter(state.mailboxes, fn({id, _}) -> id == mailbox_id end) do
        [] -> 
          {:error, :notfound}
        [m|_] -> 
          {:ok, m}
    end
  end

  def handle_call({:create_mailbox, id}, _from, state) do
    case get_mailbox(id, state) do
        {:ok, _} -> 
          {:reply, {:error, :already_exists}, state}
        {:error, :notfound} ->
          pid = spawn_link(ChatMailbox, :start, [id])
          new_mailbox = {id, pid}
          {:reply, :ok, State.new mailboxes: [new_mailbox | state.mailboxes]}
    end
  end

end
