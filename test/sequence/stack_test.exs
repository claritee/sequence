defmodule Sequence.StackTest do
  use ExUnit.Case
  doctest Sequence.Stack

  alias Sequence.Stack

  test "first item is popped off the stack" do
  	{:ok, pid} = GenServer.start_link(Stack, [5, "cat", 9, 100, "dog"])
  	popped = GenServer.call(pid, :pop)
  	assert popped == 5
  end

  test "item is pushed to the top of the stack" do
  	{:ok, pid} = GenServer.start_link(Stack, [5, "cat", 9, 100, "dog"])
  	GenServer.cast(pid, {:push, 99})
    popped = GenServer.call(pid, :pop)
  	assert popped == 99
  end
end