#!/usr/bin/env bash

# mill core.runMain --mainClass peschke.mock4s.CliApp --port 9000 --settings file:examples/json-predicate.json5

URI='http://[::1]:9000/'

# no-params
curl "$URI"; printf '\n'

# simple-eq
curl "$URI?hello=true&world=1"; printf '\n'

# omits:foo
curl "$URI?world=true&hello=1"; printf '\n'
curl "$URI?hello=1&world=1"; printf '\n'

# empty:bar
curl "$URI?foo=1&bar"; printf '\n'

# 404 Not Found
curl "$URI?foo=1&bar="; printf '\n'

# foo:prefix
curl "$URI?foo=abc-123&baz="; printf '\n'
