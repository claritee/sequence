defmodule Sequence.Stack do
	use GenServer

  def init(initial_stack) do
    {:ok, initial_stack}
  end

  def handle_call(:pop, _from, current_stack) do
    # popped = List.first(current_stack)
    [popped | stack] = current_stack
    {:reply, popped, stack}
  end

  def handle_cast({:push, item}, current_stack) do
    {:noreply, [item] ++ current_stack}
  end
end