defmodule ChatServer do

  use Application.Behaviour

  def start(_type, _args) do 
    ChatSupervisor.start_link()
  end

  def stop(_state) do
    :ok
  end

end
