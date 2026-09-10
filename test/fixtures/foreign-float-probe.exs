[beam_directory, library] = System.argv()
Code.prepend_path(beam_directory)
:ok = :foreign_float_nif.load(String.to_charlist(library))
{:ok, codec} = Catena.Foreign.Codec.new({:data, :float})
limits = %{nodes: 8, bytes: 64, depth: 4}
patterns = [0, 0x8000000000000000, 0x7FEFFFFFFFFFFFFF, 1]

for {bits, index} <- Enum.with_index(patterns) do
  native = :foreign_float_nif.number(index)
  {:ok, semantic} = :foreign_float_nif.decode(codec, native, limits)
  <<^bits::64>> = <<semantic::float-64>>
end

for index <- 4..6 do
  result =
    try do
      :foreign_float_nif.number(index)
      :incorrectly_accepted
    rescue
      ArgumentError -> :rejected
    end

  :rejected = result
end

for bits <- [0x7FF0000000000000, 0xFFF0000000000000, 0x7FF8000000000001] do
  result =
    try do
      :erlang.binary_to_term(<<131, 70, bits::64>>, [:safe])
      :incorrectly_accepted
    rescue
      ArgumentError -> :rejected
    end

  :rejected = result
end

{:error, %{kind: :conversion_failure}} = :foreign_float_nif.decode(codec, 1, limits)

IO.puts(
  "foreign_float_probe: 4 finite patterns preserved; 3 NIF and 3 ETF nonfinite patterns refused"
)

{:ok, nested} =
  Catena.Foreign.Codec.new(
    {:data, {:record, %{"payload" => {:tuple, [:integer, :text, :bytes]}}}}
  )

native = %{payload: {123, "λ", <<255>>}}
{:ok, semantic} = :foreign_float_nif.decode(nested, native, %{nodes: 20, bytes: 100, depth: 10})
{:ok, ^native} = :foreign_float_nif.encode(nested, semantic, %{nodes: 20, bytes: 100, depth: 10})
IO.puts("independent Erlang nested codec roundtrip preserved")
