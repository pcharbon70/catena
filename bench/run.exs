alias Catena.Performance.Envelope

defmodule CatenaBenchWorkloads do
  def all do
    [
      w("direct-call", "direct-call", [0, 1_000, 10_000], &sum_direct/1, &sum_erlang/1),
      w("curried-call", "curried-call", [0, 1_000, 10_000], &curried/1, &sum_erlang/1),
      w("trait-map", "trait-operation", [0, 1_000, 10_000], &trait_map/1, &enum_map/1),
      w("adt-sequence", "adt", [0, 1_000, 10_000], &adt_sequence/1, &adt_baseline/1),
      w("pattern-fold", "pattern", [0, 1_000, 10_000], &pattern_fold/1, &sum_erlang/1),
      w("guard-filter", "guard", [0, 1_000, 10_000], &guard_filter/1, &guard_baseline/1),
      w("comprehension", "comprehension", [0, 1_000, 10_000], &comprehension/1, &enum_map/1),
      w("handler-cps", "handler", [0, 1_000, 10_000], &handler_cps/1, &sum_erlang/1),
      w(
        "process-roundtrip",
        "process-message",
        [0, 100, 1_000],
        &process_roundtrip/1,
        &mailbox_baseline/1
      ),
      w("resource-scope", "resource", [0, 100, 1_000], &resource_scope/1, &sum_erlang/1),
      w("foreign-codec", "foreign", [0, 1_000, 10_000], &foreign_codec/1, &values/1),
      w("erasure", "erasure", [0, 1_000, 10_000], &erasure/1, &erasure_baseline/1),
      w("beam-code-size", "code-size", [1, 8, 32], &code_size/1, &code_size/1),
      w("kernel-compile", "compile-time", [1, 8, 32], &compile_kernel/1, &compile_kernel/1),
      w("diagnostic-provenance", "diagnostic", [1, 8, 32], &diagnostics/1, &diagnostics/1)
    ]
  end

  defp w(id, family, sizes, run, baseline),
    do: Envelope.workload(id, family, sizes, run, baseline)

  defp values(n), do: if(n == 0, do: [], else: Enum.to_list(1..n))
  defp sum_direct(n), do: Enum.reduce(values(n), 0, fn x, acc -> acc + x end)
  defp sum_erlang(n), do: :lists.foldl(fn x, acc -> :erlang.+(acc, x) end, 0, values(n))

  defp curried(n) do
    add = fn acc -> fn value -> acc + value end end
    Enum.reduce(values(n), 0, fn value, acc -> add.(acc).(value) end)
  end

  defp trait_map(n), do: Catena.Standard.List.map(&(&1 + 1), values(n))
  defp enum_map(n), do: Enum.map(values(n), &(&1 + 1))
  defp adt_sequence(n), do: Catena.Standard.Outcomes.sequence(values(n))

  defp adt_baseline(n),
    do:
      Enum.reduce(
        :lists.reverse(values(n)),
        {:catena_adt, :"catena://outcome-contract/0.1.54::CatenaOutcomeRoles::Sequence", 0, {}},
        fn value, rest ->
          {:catena_adt, :"catena://outcome-contract/0.1.54::CatenaOutcomeRoles::Sequence", 1,
           {value, rest}}
        end
      )

  defp pattern_fold(n), do: do_pattern(values(n), 0)
  defp do_pattern([], acc), do: acc
  defp do_pattern([head | tail], acc), do: do_pattern(tail, acc + head)

  defp guard_filter(n),
    do: for(value <- values(n), is_integer(value) and rem(value, 2) == 0, do: value)

  defp guard_baseline(n), do: Enum.filter(values(n), &(rem(&1, 2) == 0))
  defp comprehension(n), do: for(value <- values(n), do: value + 1)

  defp handler_cps(n),
    do:
      Enum.reduce(values(n), 0, fn value, acc ->
        (fn resume -> resume.(value) end).(fn answer -> acc + answer end)
      end)

  defp process_roundtrip(n) do
    parent = self()

    pid =
      spawn(fn ->
        receive do
          {:values, values} -> send(parent, {:answer, Enum.sum(values)})
        end
      end)

    send(pid, {:values, values(n)})

    receive do
      {:answer, answer} -> answer
    end
  end

  defp mailbox_baseline(n) do
    send(self(), {:bench_values, values(n)})

    receive do
      {:bench_values, entries} -> Enum.sum(entries)
    end
  end

  defp resource_scope(n),
    do:
      Catena.Resource.Runtime.run(
        :bench,
        fn _ -> :ok end,
        fn _, _ -> sum_direct(n) end,
        1_000_000_000
      )

  defp foreign_codec(n) do
    {:ok, codec} = Catena.Foreign.Codec.new({:data, :integer})
    limits = %{depth: 8, nodes: 8, bytes: 1024}

    Enum.map(values(n), fn value ->
      {:ok, result} = Catena.Foreign.Codec.decode(codec, value, limits)
      result
    end)
  end

  defp erasure(n), do: Enum.map(values(n), &%{runtime: &1})
  defp erasure_baseline(n), do: for(value <- values(n), do: %{runtime: value})

  defp code_size(n) do
    {:ok, _, binary, _} = Catena.compile_kernel(source("BenchCode#{n}", nested(n)))
    byte_size(binary)
  end

  defp compile_kernel(n) do
    {:ok, core} = Catena.check_kernel(source("BenchCompile#{n}", nested(n)))
    {length(core.definitions), core.language_revision}
  end

  defp diagnostics(n) do
    {:error, diagnostic} =
      Catena.check_kernel(
        "(module BenchDiagnostic#{n} (edition 0.1) (revision 0.1.8) (origin \"bench://diagnostic\") (export value main) (def main (signature Int (uses)) (var missing#{n})))"
      )

    {diagnostic.id, diagnostic.path}
  end

  defp nested(n), do: Enum.reduce(1..n, "0", fn _, expression -> "(add 1 #{expression})" end)

  defp source(module, expression),
    do:
      "(module #{module} (edition 0.1) (revision 0.1.8) (origin \"bench://envelope\") (export value main) (def main (signature Int (uses)) #{expression}))"
end

{:ok, report} =
  Envelope.run(CatenaBenchWorkloads.all(),
    repetitions: 5,
    warmup: 1,
    seed: 13_138,
    timeout_ms: 30_000
  )

:ok = Envelope.verify(report)
path = Path.join(["bench", "results", "otp-29-linux-x86-64.json"])
File.write!(path, Catena.CanonicalJCS.encode(report))
IO.puts(path)
