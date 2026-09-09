defmodule Catena.ForeignNativeTest do
  use ExUnit.Case, async: false
  @moduletag obligations: ~w(ET-OBL-003 ET-OBL-010)

  test "an isolated native VM preserves finite Float bits and refuses nonfinite NIF/ETF ingress" do
    directory =
      Path.join(System.tmp_dir!(), "catena-foreign-native-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    include = Path.join(to_string(:code.root_dir()), "usr/include")
    library = Path.join(directory, "foreign_float_nif")

    assert {_, 0} =
             System.cmd(
               "cc",
               [
                 "-std=c11",
                 "-Wall",
                 "-Wextra",
                 "-Werror",
                 "-fPIC",
                 "-shared",
                 "-I",
                 include,
                 "test/fixtures/foreign-float-nif.c",
                 "-o",
                 library <> ".so"
               ],
               stderr_to_stdout: true
             )

    {:ok, :foreign_float_nif, binary} =
      :compile.file(~c"test/fixtures/foreign-float-nif.erl", [:binary])

    File.write!(Path.join(directory, "foreign_float_nif.beam"), binary)
    ebin = Path.expand(Path.join(Mix.Project.build_path(), "lib/catena/ebin"))

    {output, status} =
      System.cmd(
        "elixir",
        [
          "--erl",
          "+S 2:2",
          "-pa",
          ebin,
          "test/fixtures/foreign-float-probe.exs",
          directory,
          library
        ],
        stderr_to_stdout: true
      )

    assert status == 0, output
    assert output =~ "4 finite patterns preserved; 3 NIF and 3 ETF nonfinite patterns refused"
  end
end
