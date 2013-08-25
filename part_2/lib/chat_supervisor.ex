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
