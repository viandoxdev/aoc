### Deps

- elixir
- mix
- gcc (for Matrex)
- make (for Matrex)

### Setup

```sh
mix deps.get # might error out
MATREX_BLAS=noblas mix deps.compile matrex 
```

### Running

```sh
mix run
```

> Tested with Elixir 1.14.1 (Erlang/OTP 25)
