defmodule ChatWeb do

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

  def handle_request(req, "/test") do
    html_ok req, "<p>TESTING</p>"
  end

  def handle_request(req, "/chat") do
    html_ok req, ChatUtil.get_template("chat", [])
  end

  def handle_request(req, "/") do
    html_ok req, ChatUtil.get_template("index", [])
  end

  def handle_request(req, path) do
    html_ok req, "<p>Got request for '#{path}'</p>"
  end

  defp html_ok(req, data) do
    req.ok({"text/html;charset=UTF-8", data})
  end

end


