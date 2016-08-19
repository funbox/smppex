[![Build Status](https://travis-ci.org/savonarola/smppex.svg?branch=master)](https://travis-ci.org/savonarola/smppex)

# Smppex

SMPP 3.4 protocol and framework implementation in [Elixir](http://elixir-lang.org)

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

