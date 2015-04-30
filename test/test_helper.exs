ExUnit.start []

# This is a subset of what the mix test helper does. Arguably, the ExUnit
# library should provide some sort of temporary file management.
defmodule GvTest.Case do
  use ExUnit.CaseTemplate

  using do
    quote do
      import GvTest.Case
    end
  end

  setup do
    on_exit fn ->
      del_tmp_paths
    end
  end

  def fixture_path do
    Path.expand("../fixtures", "./test/test_helper.exs")
  end

  def fixture_path(extension) do
    Path.join fixture_path, extension
  end

  def tmp_path do
    Path.expand("../../tmp", "./test/test_helper.exs")
  end

  def tmp_path(extension) do
    Path.join tmp_path, extension
  end

  def del_tmp_paths do
    #tmp = tmp_path |> binary_to_list
    #to_remove = Enum.filter :code.get_path, fn(path) -> :string.str(path, tmp) != 0 end
    #Enum.map to_remove, fn x -> :code.del_path(x) end
  end

  def in_tmp(which, function) do
    path = tmp_path(which)
    File.rm_rf! path
    File.mkdir_p! path
    File.cd! path, function
  end

  def assert_prints(function, expected) do
    path = tmp_path
    File.rm_rf! path
    { :ok, file } = File.open(path, [ :write ])
    function.(file)
    :ok = File.close(file)
    assert_file_content(path, expected)
  end

  def assert_file_content(path, expected) do
    { :ok, actual } = File.read(path)
    assert actual == expected
    File.rm_rf! path
    :ok
  end
end
