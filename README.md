# Smppex

[![Build Status](https://travis-ci.org/funbox/smppex.svg?branch=master)](https://travis-ci.org/funbox/smppex)
[![Documentation](https://img.shields.io/badge/docs-hexpm-blue.svg)](http://hexdocs.pm/smppex)
[![Version](https://img.shields.io/hexpm/v/smppex.svg)](https://hex.pm/packages/smppex)
[![Coverage Status](https://coveralls.io/repos/github/funbox/smppex/badge.svg?branch=master&1504538909)](https://coveralls.io/github/funbox/smppex?branch=master)

SMPP 3.4 protocol and framework implementation in [Elixir](http://elixir-lang.org).

See [Examples](https://hexdocs.pm/smppex/SMPPEX.html) for details.

## Documentation

API documentation is available at **[hexdocs.pm/smppex](http://hexdocs.pm/smppex)**.

## Live Demo

There is a simple online demo MC (SMPP server) at **[smppex.rubybox.ru](http://smppex.rubybox.ru)**.

## Installation

To install and use the package:

1. Add `smppex` to your list of dependencies in `mix.exs`:

   ```elixir
   def deps do
     [{:smppex, "~> 2.0"}]
   end
   ```

2. Ensure `smppex` is started before your application:

   ```elixir
   def application do
     [applications: [:smppex]]
   end
   ```

## License

This software is licensed under [MIT License](LICENSE).

[![Sponsored by FunBox](https://funbox.ru/badges/sponsored_by_funbox_centered.svg)](https://funbox.ru)
