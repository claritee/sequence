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
* OTP calls the approciate `handle_call` callback to handle a situation

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
