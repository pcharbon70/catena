defmodule Catena.KernelTransitiveEffectsTest do
  use ExUnit.Case, async: false

  alias Catena.Kernel.{Backend, Stepper}

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "ordinary effects cross several zero-argument global references" do
    source =
      program("KernelForwardedReferences", """
      (def requested (signature Int (uses Ask)) (request Ask ask 41))
      (def middle (signature Int (uses Ask)) (var requested))
      (def outer (signature Int (uses Ask)) (var middle))
      (def main (signature Int (uses)) (handle AddOne (var outer)))
      """)

    assert_agreement(source, 42)
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "curried workers forward checked latent effects through global calls" do
    source =
      program("KernelForwardedCurried", """
      (def requested (signature (Fn Int (effects) (Fn Int (effects Ask) Int)) (uses))
        (fn (left Int) (fn (right Int) (request Ask ask (add (var left) (var right))))))
      (def middle (signature (Fn Int (effects) (Fn Int (effects Ask) Int)) (uses))
        (fn (left Int) (fn (right Int) (call (var requested) (var left) (var right)))))
      (def outer (signature (Fn Int (effects) (Fn Int (effects Ask) Int)) (uses))
        (fn (left Int) (fn (right Int) (call (var middle) (var left) (var right)))))
      (def main (signature Int (uses))
        (handle AddOne (let partial (call (var outer) 20) (call (var partial) 21))))
      """)

    assert_agreement(source, 42)
  end

  for {name, expression, signature, request, invocation} <- [
        {"Alias", "(var requested)", "(Fn Int (effects Ask) Int)",
         "(fn (value Int) (request Ask ask (var value)))", "(call (var forwarded) 41)"},
        {"LetAlias", "(let selected (var requested) (var selected))",
         "(Fn Int (effects Ask) Int)", "(fn (value Int) (request Ask ask (var value)))",
         "(call (var forwarded) 41)"},
        {"Partial", "(call (var requested) 20)", "(Fn Int (effects Ask) Int)",
         "(fn (left Int) (fn (right Int) (request Ask ask (add (var left) (var right)))))",
         "(call (var forwarded) 21)"},
        {"LambdaResidual", "(fn (left Int) (call (var requested) (var left)))",
         "(Fn Int (effects) (Fn Int (effects Ask) Int))",
         "(fn (left Int) (fn (right Int) (request Ask ask (add (var left) (var right)))))",
         "(call (var forwarded) 20 21)"}
      ] do
    @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
    test "#{name} preserves latent effects when the definition body is not fully lambda-expanded" do
      request_signature =
        if unquote(name) in ["Partial", "LambdaResidual"],
          do: "(Fn Int (effects) (Fn Int (effects Ask) Int))",
          else: "(Fn Int (effects Ask) Int)"

      source =
        program("KernelForwarded#{unquote(name)}", """
        (def requested (signature #{request_signature} (uses)) #{unquote(request)})
        (def forwarded (signature #{unquote(signature)} (uses)) #{unquote(expression)})
        (def main (signature Int (uses)) (handle AddOne #{unquote(invocation)}))
        """)

      assert_agreement(source, 42)
    end
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "invoked higher-order effects are preserved without CPS-converting an unused parameter" do
    source =
      program("KernelHigherOrderForwarding", """
      (def requested (signature (Fn Int (effects Ask) Int) (uses))
        (fn (value Int) (request Ask ask (var value))))
      (def ignore (signature (Fn (Fn Int (effects Ask) Int) (effects) Int) (uses))
        (fn (function (Fn Int (effects Ask) Int)) 42))
      (def invoke (signature (Fn (Fn Int (effects Ask) Int) (effects) (Fn Int (effects Ask) Int)) (uses))
        (fn (function (Fn Int (effects Ask) Int))
          (fn (value Int) (call (var function) (var value)))))
      (def main (signature Int (uses))
        (sequence (call (var ignore) (var requested))
          (handle AddOne (call (var invoke) (var requested) 41))))
      """)

    metadata = assert_agreement(source, 42)

    refute Enum.any?(
             metadata.forms,
             &match?({:function, _, :__catena_kernel_cps_ignore, _, _}, &1)
           )
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "a handled residual callable is evaluated once before applying its argument" do
    source =
      program("KernelHandledResidual", """
      (effect Build (operation build (params) (Fn Int (effects Ask) Int)))
      (handler Supply
        (effect Build)
        (input (Fn Int (effects Ask) Int))
        (output (Fn Int (effects Ask) Int))
        (return function (var function))
        (operation build (params) (resume next) (resume next (var requested))))
      (def requested (signature (Fn Int (effects Ask) Int) (uses))
        (fn (value Int) (request Ask ask (var value))))
      (def forwarded (signature (Fn Int (effects Ask) Int) (uses))
        (handle Supply (request Build build)))
      (def main (signature Int (uses)) (handle AddOne (call (var forwarded) 41)))
      """)

    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, 42, outcome} = Stepper.run(core, "main")
    requests = for %{label: :request, effect: effect} <- outcome.trace, do: effect
    assert requests == ["Build", "Ask"]
    assert_agreement(source, 42, 2)
  end

  for {name, callback} <- [
        {"Anonymous", "(fn (value Int) (add (var value) 1))"},
        {"Global", "(var increment)"},
        {"Record", "(select (record (field callback (var increment))) callback)"},
        {"Returned", "(call (var identity) (var increment))"}
      ] do
    @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
    test "#{name} pure callback retains its representation inside CPS evaluation" do
      source =
        program("KernelPureCallback#{unquote(name)}", """
        (def requested (signature (Fn Int (effects Ask) Int) (uses))
          (fn (value Int) (request Ask ask (var value))))
        (def ignore (signature (Fn (Fn Int (effects Ask) Int) (effects) Int) (uses))
          (fn (function (Fn Int (effects Ask) Int)) 42))
        (def increment (signature (Fn Int (effects) Int) (uses))
          (fn (value Int) (add (var value) 1)))
        (def identity (signature (Fn (Fn Int (effects) Int) (effects) (Fn Int (effects) Int)) (uses))
          (fn (function (Fn Int (effects) Int)) (var function)))
        (def apply_pure (signature (Fn (Fn Int (effects) Int) (effects) (Fn Int (effects) Int)) (uses))
          (fn (function (Fn Int (effects) Int))
            (fn (value Int) (call (var function) (var value)))))
        (def main (signature Int (uses))
          (sequence (call (var ignore) (var requested))
            (call (var apply_pure) #{unquote(callback)} 41)))
        """)

      assert_agreement(source, 42)
    end
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "a pure anonymous callback executes its local handler before returning" do
    source =
      program("KernelPureHandledCallback", """
      (def apply_pure (signature (Fn (Fn Int (effects) Int) (effects) (Fn Int (effects) Int)) (uses))
        (fn (function (Fn Int (effects) Int))
          (fn (value Int) (call (var function) (var value)))))
      (def main (signature Int (uses))
        (call (var apply_pure)
          (fn (value Int) (handle AddOne (request Ask ask (var value)))) 41))
      """)

    assert_agreement(source, 42, 1)
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "a direct caller passes a pure callback into a global with local control" do
    source =
      program("KernelDirectCallbackIntoControl", """
      (def apply_handled (signature (Fn (Fn Int (effects) Int) (effects) Int) (uses))
        (fn (function (Fn Int (effects) Int))
          (call (var function) (handle AddOne (request Ask ask 40)))))
      (def main (signature Int (uses))
        (call (var apply_handled) (fn (value Int) (add (var value) 1))))
      """)

    assert_agreement(source, 42, 1)
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "a polymorphic pure stage can return an effectful callable" do
    source =
      program("KernelPolymorphicCallable", """
      (def requested (signature (Fn Int (effects Ask) Int) (uses))
        (fn (value Int) (request Ask ask (var value))))
      (def main (signature Int (uses))
        (let identity (fn (value a) (var value))
          (let forwarded (call (var identity) (var requested))
            (handle AddOne (call (var forwarded) 41)))))
      """)

    assert_agreement(source, 42, 1)
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "an effectful first stage completes before its pure result leaves the handler" do
    source =
      program("KernelEarlyCurriedRequest", """
      (handler Make
        (effect Ask) (input (Fn Int (effects) Int)) (output (Fn Int (effects) Int))
        (return function (var function))
        (operation ask (params (value Int)) (resume next)
          (resume next (add (var value) 1))))
      (def staged (signature (Fn Int (effects Ask) (Fn Int (effects) Int)) (uses))
        (fn (left Int)
          (let observed (request Ask ask (var left))
            (fn (right Int) (add (var observed) (var right))))))
      (def main (signature Int (uses))
        (let partial (handle Make (call (var staged) 10)) (call (var partial) 31)))
      """)

    assert_agreement(source, 42, 1)
  end

  for {name, callee} <- [
        {"Global", "(var staged)"},
        {"Anonymous",
         "(fn (left Int) (sequence (request Ask ask (var left)) (fn (right Int) (var right))))"}
      ] do
    @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
    test "#{name} first-stage decline prevents evaluation of a later argument" do
      source =
        program("KernelEarlyDecline#{unquote(name)}", """
        (handler Decline
          (effect Ask) (input Int) (output Int)
          (return result (var result))
          (operation ask (params (value Int)) (resume next) 99))
        (def staged (signature (Fn Int (effects Ask) (Fn Int (effects) Int)) (uses))
          (fn (left Int)
            (sequence (request Ask ask (var left)) (fn (right Int) (var right)))))
        (def main (signature Int (uses))
          (handle Decline (call #{unquote(callee)} 10 (trap 7))))
        """)

      assert_agreement(source, 99)
    end
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "global callable lookup happens before evaluating its first argument" do
    source =
      program("KernelGlobalLookupBeforeArgument", """
      (handler Decline
        (effect Ask) (input Int) (output Int)
        (return result (var result))
        (operation ask (params (value Int)) (resume next) 99))
      (def staged (signature (Fn Int (effects) Int) (uses Ask))
        (sequence (request Ask ask 10) (fn (value Int) (var value))))
      (def main (signature Int (uses))
        (handle Decline (call (var staged) (trap 7))))
      """)

    metadata = assert_agreement(source, 99)
    assert Enum.any?(metadata.forms, &match?({:function, _, :staged, 1, _}, &1))
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-011 FK-OBL-013)
  test "a pure first-stage trap precedes a later argument trap" do
    source =
      program("KernelPureStageTrapOrder", """
      (def staged (signature (Fn Int (effects) (Fn Int (effects) Int)) (uses))
        (fn (left Int) (sequence (trap 7) (fn (right Int) (var right)))))
      (def main (signature Int (uses)) (call (var staged) 10 (trap 8)))
      """)

    assert {:ok, core} = Catena.check_kernel(source)
    assert {:trap, 7, _outcome} = Stepper.run(core, "main")
    assert {:ok, module, binary, metadata} = Backend.compile(core)
    assert {:module, ^module} = :code.load_binary(module, ~c"kernel-stage-trap-order", binary)

    try do
      assert catch_error(apply(module, :main, [])) == {:catena_trap, 7}
      assert Enum.any?(metadata.forms, &match?({:function, _, :staged, 2, _}, &1))

      refute Enum.any?(
               metadata.forms,
               &match?({:function, _, :__catena_kernel_cps_main, _, _}, &1)
             )
    after
      :code.delete(module)
      :code.purge(module)
    end
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-013)
  test "a declining handler aborts the whole forwarded continuation" do
    source =
      program("KernelForwardedAbort", """
      (handler Decline
        (effect Ask) (input Int) (output Int)
        (return result (var result))
        (operation ask (params (value Int)) (resume next) 99))
      (def requested (signature Int (uses Ask)) (request Ask ask 41))
      (def middle (signature Int (uses Ask)) (sequence (var requested) (trap 7)))
      (def outer (signature Int (uses Ask)) (add (var middle) 1))
      (def main (signature Int (uses)) (handle Decline (var outer)))
      """)

    assert_agreement(source, 99)
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-012)
  test "pure and reserved Process rows retain direct lowering" do
    source = """
    (module KernelDirectBoundaries
      (edition 0.1) (revision 0.1.8) (origin "test://kernel-direct-boundaries")
      (export value main)
      (export process Done)
      (def pure (signature Int (uses)) 42)
      (process Done (mailbox Unit) (params) (unit))
      (def own (signature Unit (uses Process)) (let child (spawn Done) (unit)))
      (def main (signature Int (uses Process)) (sequence (var own) (var pure))))
    """

    metadata = assert_agreement(source, 42)
    function_names = for {:function, _, name, _, _} <- metadata.forms, do: name
    assert Enum.all?([:main, :own, :pure], &(&1 in function_names))

    refute Enum.any?(
             function_names,
             &String.starts_with?(Atom.to_string(&1), "__catena_kernel_cps_")
           )
  end

  @tag obligations: ~w(FK-OBL-007 FK-OBL-013)
  test "backend still rejects forged transitive effect evidence" do
    source =
      program("KernelForgedForwarding", """
      (def requested (signature Int (uses Ask)) (request Ask ask 41))
      (def middle (signature Int (uses Ask)) (var requested))
      (def main (signature Int (uses)) (handle AddOne (var middle)))
      """)

    assert {:ok, core} = Catena.check_kernel(source)

    forged = %{
      core
      | definitions:
          Enum.map(core.definitions, fn
            %{name: "middle"} = definition ->
              %{definition | expression: %{definition.expression | effects: []}}

            definition ->
              definition
          end)
    }

    assert {:error, %{id: "I001"}} = Backend.compile(forged)
  end

  defp program(module, definitions) do
    """
    (module #{module}
      (edition 0.1) (revision 0.1.8) (origin "test://#{module}")
      (export value main)
      (effect Ask (operation ask (params Int) Int))
      (handler AddOne
        (effect Ask) (input Int) (output Int)
        (return result (var result))
        (operation ask (params (value Int)) (resume next)
          (resume next (add (var value) 1))))
      #{definitions})
    """
  end

  defp assert_agreement(source, expected, expected_resumes \\ nil) do
    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, ^expected, _outcome} = Stepper.run(core, "main")
    assert {:ok, module, binary, metadata} = Backend.compile(core)
    assert {:module, ^module} = :code.load_binary(module, ~c"kernel-transitive-effects", binary)

    try do
      {actual, trace} = Catena.Effect.Runtime.capture_trace(fn -> apply(module, :main, []) end)
      assert actual == expected
      if expected_resumes, do: assert(Enum.count(trace, &(&1 == :resume)) == expected_resumes)
      metadata
    after
      :code.delete(module)
      :code.purge(module)
    end
  end
end
