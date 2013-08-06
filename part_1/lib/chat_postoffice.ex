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

  def delete_mailbox(id) do
    :gen_server.cast :postoffice, {:delete_mailbox, id}
  end

  def send_mail(id, message) do
    :gen_server.cast(:postoffice, {:send_mail, {id, message}})
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

  def handle_cast({:delete_mailbox, mailbox_id}, state) do
    #state{mailboxes=MBoxes} = State) ->
    new_boxes = Enum.filter(state.mailboxes, fn({id, pid}) ->
        case id != mailbox_id do
            false -> 
              # tell the mailbox process to quit
              pid <- :quit
              false
            _ -> 
              true
        end
    end)
    {:noreply, State.new mailboxes: new_boxes}
  end

  def handle_cast({:send_mail, {id, message}}, state) do
    case get_mailbox(id, state) do
      {:ok, {_id, pid}} -> 
        pid <- {:mail, message}
      _ -> 
        :ok
    end
    {:noreply, state}
  end

end

