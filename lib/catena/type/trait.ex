defmodule Catena.Type.Trait do
  @moduledoc "A terminating ground trait registry with coherence and ownership checks."

  alias Catena.Diagnostic

  defstruct traits: %{}, instances: []

  @type trait :: %{
          name: String.t(),
          arity: pos_integer(),
          fundeps: [{[non_neg_integer()], [non_neg_integer()]}]
        }
  @type instance :: %{
          trait: String.t(),
          arguments: [term()],
          owner: String.t(),
          context: [term()],
          associated_types: map()
        }
  @type t :: %__MODULE__{traits: map(), instances: [instance()]}

  @spec new() :: t()
  def new, do: %__MODULE__{}

  @spec add_trait(t(), String.t(), pos_integer(), keyword()) :: t()
  def add_trait(registry, name, arity, options \\ []) when arity > 0 do
    trait = %{name: name, arity: arity, fundeps: Keyword.get(options, :fundeps, [])}
    %{registry | traits: Map.put(registry.traits, name, trait)}
  end

  @spec add_instance(t(), instance()) :: t()
  def add_instance(registry, instance) do
    trait = Map.get(registry.traits, instance.trait) || fail("unknown trait #{instance.trait}")

    if length(instance.arguments) != trait.arity,
      do: fail("wrong instance arity for #{instance.trait}")

    head_owner = instance.arguments |> hd() |> constructor_owner()

    unless instance.owner in [instance.trait, head_owner] do
      fail("instance violates trait-or-type ownership")
    end

    if Enum.any?(registry.instances, &overlaps?(&1, instance)),
      do: fail("overlapping instances are forbidden")

    unless smaller_context?(instance.context, instance.arguments),
      do: fail("instance context does not decrease")

    %{registry | instances: [instance | registry.instances]}
  end

  @spec resolve(t(), String.t(), [term()]) :: {:ok, instance()} | {:error, Diagnostic.t()}
  def resolve(registry, trait_name, arguments) do
    matches =
      Enum.filter(registry.instances, &(&1.trait == trait_name and &1.arguments == arguments))

    case matches do
      [instance] ->
        {:ok, instance}

      [] ->
        {:error, Diagnostic.new("T007", "no instance for #{trait_name} #{inspect(arguments)}")}

      _ ->
        {:error,
         Diagnostic.new("T007", "incoherent instances for #{trait_name} #{inspect(arguments)}")}
    end
  end

  @spec associated_type(t(), String.t(), [term()], String.t()) ::
          {:ok, term()} | {:error, Diagnostic.t()}
  def associated_type(registry, trait, arguments, name) do
    with {:ok, instance} <- resolve(registry, trait, arguments),
         {:ok, type} <- Map.fetch(instance.associated_types, name) do
      {:ok, type}
    else
      :error -> {:error, Diagnostic.new("T007", "associated type #{name} is not defined")}
      {:error, _} = error -> error
    end
  end

  defp overlaps?(left, right), do: left.trait == right.trait and left.arguments == right.arguments
  defp smaller_context?([], _head), do: true
  defp smaller_context?(context, head), do: Enum.all?(context, &(term_size(&1) < term_size(head)))
  defp term_size(term) when is_list(term), do: 1 + Enum.sum(Enum.map(term, &term_size/1))
  defp term_size(term) when is_tuple(term), do: term |> Tuple.to_list() |> term_size()
  defp term_size(_term), do: 1
  defp constructor_owner({owner, _name}) when is_binary(owner), do: owner
  defp constructor_owner({owner, _name, _arguments}) when is_binary(owner), do: owner
  defp constructor_owner(_), do: nil
  defp fail(message), do: raise(Catena.TypeError, diagnostic: Diagnostic.new("T007", message))
end
