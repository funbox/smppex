[![Build Status](https://travis-ci.org/savonarola/smppex.svg?branch=master)](https://travis-ci.org/savonarola/smppex)

# Smppex

SMPP 3.4 protocol and framework implementation in [Elixir](http://elixir-lang.org)

See [Examples](https://hexdocs.pm/smppex/SMPPEX.html)

## Documentation

API documentation is available at http://hexdocs.pm/smppex

## Installation

The package can be installed as:

  1. Add `smppex` to your list of dependencies in `mix.exs`:

    ```elixir
    def deps do
      [{:smppex, "~> 0.1.0"}]
    end
    ```

  2. Ensure `smppex` is started before your application:

    ```elixir
    def application do
      [applications: [:smppex]]
    end
    ```

## LICENSE

This software is licensed under [MIT License](LICENSE).
