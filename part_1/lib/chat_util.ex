defmodule ChatUtil do
  def generate_hash() do
    :crypto.hash(:sha, generate_string(16))
    |> bin_to_hexstr
  end

  def generate_string(size) do
    {a, b, c} = :erlang.now()
    :random.seed(a, b, c)
    Enum.sort(1..size) 
      |> List.foldl([], fn(_x, accin) -> [:random.uniform(90) + 32 | accin] end)
      |> List.flatten
  end

  def bin_to_hexstr(bin) do
    List.flatten(lc x inlist :erlang.binary_to_list(bin), do: :io_lib.format("~2.16.0B", [x]))
  end

end

