defmodule ChatUtil do
  def generate_hash() do
    :crypto.hash(:sha, generate_string(16))
    |> bin_to_hexstr
    |> to_string
  end

  def generate_string(size) do
    {a, b, c} = :erlang.now()
    :random.seed(a, b, c)
    Enum.sort(1..size) 
      |> List.foldl([], fn(_x, accin) -> [:random.uniform(90) + 32 | accin] end)
      |> List.flatten
  end

  def bin_to_hexstr(bin) do
    List.flatten(lc x inlist :erlang.binary_to_list(bin), 
      do: :io_lib.format("~2.16.0B", [x]))
  end

  def get_template(name, vars) do
    :ok = :erlydtl.compile('templates/#{name}.html', binary_to_atom(name))
    {:ok, tpl} = apply(binary_to_atom(name), :render, [vars])
    String.from_char_list!(tpl)
  end

  def get_parameter(n, [{k,v}|_]) when k == n do 
    to_string v
  end

  def get_parameter(n, [_|t]) do
    get_parameter(n, t)
  end

  def get_parameter(_, _) do
    []
  end


  #def unicode_clean(str) do
    #case :rfc4627.unicode_decode(:erlang.list_to_binary(str)) do
        #{'EXIT', _Reason} -> :erlang.list_to_binary(strip_unicode(str, []))
        #{'utf-8', _M} -> :erlang.list_to_binary(str)
        #_ -> <<>>
    #end
  #end

end

