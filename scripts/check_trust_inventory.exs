case Catena.Trust.Profile.audit(File.cwd!()) do
  :ok ->
    IO.puts("Trusted boundary inventory matches reviewed source and data.")

  {:error, reason} ->
    IO.puts(:stderr, inspect(reason, limit: :infinity))
    System.halt(1)
end
