#!/bin/bash
set -x
docker run -t -v $(pwd):/smppex -w /smppex elixir-1.1.1 mix test --trace
