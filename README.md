# Sequence

## About

Project following "Programming Elixir" by Dave Thomas, demonstrating OTP and GenServers

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `sequence` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:sequence, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/sequence](https://hexdocs.pm/sequence).

## Notes

### What is OTP

* `OTP` Open Transfer Protocol is used to handle distributed systems
* `Mnesia` is the DB that comes with OTP
* `mix`, Erlang compiler come with OTP
* OTP define systems as a hierarchy of `applications`
* Applications contain `processes`
* Processes follow `behaviours`
* Each implementation of behaviour will implement its own server `GenServer`
* `Supervisors` monitor the health of each process and strategies to restart
* OTP calls the approciate `handle_call` callback to handle a situation - sychronous
* `handle_cast` is asynchronous, args: call arg and current_state, response: `{:noreply, new_state}`
* `sys` module. System messages - are sent in the background between processes

### Steps

1. Project creation:
`mix new sequence`

2. Start GenServer

```
{ :ok, pid } = GenServer.start_link(Sequence.Server, 100)
```

This starts the `Sequence.Server` with initial number `100` (calling the `init` function)

3. Callback `handle_call`

```
GenServer.call(pid, :next_number)
```

This invokes the `handle_call` function

`def handle_call(:next_number, _from, current_number) do`

Response: `{:reply, current_number, current_number+1}`

* 1st arg: reply to client
* 2nd arg: value
* 3rd arg: new state

4. Async `handle_cast`

```
  def handle_cast({:increment_number, delta}, current_number) do
    { :noreply, current_number + delta}
  end
```

5. Debugging

*Trace:*
`{:ok,pid} = GenServer.start_link(Sequence.Server, 100, [debug: [:trace]])`

*Statistics:*

```
iex> {:ok,pid} = GenServer.start_link(Sequence.Server, 100, [debug: [:statistics]])

iex> :sys.statistics pid, :get

{:ok,
 [start_time: {{2018, 5, 6}, {9, 47, 39}},
  current_time: {{2018, 5, 6}, {9, 47, 54}}, reductions: 84, messages_in: 2,
  messages_out: 0]}
```

Timestamps: {{y,m,d},{h,m,s}} tuples

* Enabling/Disabling after the server has started

`sys.trace pid, true`
`sys.trace pid, false`

6. Status of PID

```
iex> :sys.get_status pid
{:status, #PID<0.153.0>, {:module, :gen_server},
 [["$ancestors": [#PID<0.143.0>, #PID<0.57.0>],
   "$initial_call": {Sequence.Server, :init, 1}], :running, #PID<0.143.0>,
  [statistics: {{{2018, 5, 6}, {9, 52, 1}}, {:reductions, 21}, 6, 0}],
  [header: 'Status for generic server <0.153.0>',
   data: [{'Status', :running}, {'Parent', #PID<0.143.0>},
    {'Logged events', []}], data: [{'State', 106}]]]}
```

7. Formatting status `format_status` callback


