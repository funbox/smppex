#!/bin/bash

set -x

echo "Elixir: $TRAVIS_ELIXIR_VERSION Erlang: $TRAVIS_OTP_RELEASE"

if [ "$COVERALLS_ELIXIR_VERSION" == "$TRAVIS_ELIXIR_VERSION" ] && [ "$COVERALLS_OTP_RELEASE" == "$TRAVIS_OTP_RELEASE" ]; then
    echo "Posting coveralls"
    mix deps.get --only docs
    mix coveralls.travis
    MIX_ENV=docs mix inch.report    
fi
