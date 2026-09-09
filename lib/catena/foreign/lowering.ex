defmodule Catena.Foreign.Lowering do
  @moduledoc false
  def entry(forms, name, prefix) do
    worker = String.to_existing_atom(prefix <> name)
    symbol = String.to_atom("__catena_foreign_" <> name)
    existing = for {:function, _, name, arity, _} <- forms, do: {name, arity}

    if {worker, 2} in existing and not Enum.any?(existing, &(elem(&1, 0) == symbol)) do
      handlers = {:var, 1, :ForeignHandlers}
      value = {:var, 1, :ForeignValue}
      continuation = {:fun, 1, {:clauses, [{:clause, 1, [value], [], [value]}]}}
      call = {:call, 1, {:atom, 1, worker}, [handlers, continuation]}
      wrapper = {:function, 1, symbol, 1, [{:clause, 1, [handlers], [], [call]}]}

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
