defmodule Catena.Foreign.Lowering do
  @moduledoc false
  def entry(forms, name, prefix, annotation \\ 1, debug \\ false) do
    worker = String.to_existing_atom(prefix <> name)
    symbol = String.to_atom("__catena_foreign_" <> name)
    existing = for {:function, _, name, arity, _} <- forms, do: {name, arity}

    if {worker, 2} in existing and not Enum.any?(existing, &(elem(&1, 0) == symbol)) do
      handlers = {:var, annotation, :ForeignHandlers}
      value = {:var, annotation, :ForeignValue}
      continuation = {:fun, annotation, {:clauses, [{:clause, annotation, [value], [], [value]}]}}
      call = {:call, annotation, {:atom, annotation, worker}, [handlers, continuation]}

      call =
        if debug do
          {:call, annotation,
           {:remote, annotation, {:atom, annotation, Catena.Debugging.Runtime},
            {:atom, annotation, :identity}}, [call]}
        else
          call
        end

      wrapper =
        {:function, annotation, symbol, 1, [{:clause, annotation, [handlers], [], [call]}]}

      forms =
        Enum.map(forms, fn
          {:attribute, a, :export, entries} -> {:attribute, a, :export, [{symbol, 1} | entries]}
          form -> form
        end)

      {:ok, forms ++ [wrapper], symbol}
    else
      {:error, :unsupported_foreign_entry_lowering}
    end
  rescue
    _ -> {:error, :unsupported_foreign_entry_lowering}
  end
end
