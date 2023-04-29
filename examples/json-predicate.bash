#!/usr/bin/env bash

# mill core.runMain --mainClass peschke.mock4s.CliApp --port 9000 --settings file:examples/json-predicate.json5

URI='http://[::1]:9000/'

# {"foo":{"bar":false},"abc":false}
curl "$URI"; printf '\n'

# {"foo":{"bar":true},"abc":false}
curl "$URI" --json '{"foo": {"bar": true } }'; printf '\n'

# {"foo":{"bar":false},"abc":false}
curl "$URI" --json '{"foo": ["hello", "world"]}'; printf '\n'

# {"foo":{"bar":false},"abc":true}
curl "$URI" --json '{"foo": ["hello", "world", "abc-def"]}'; printf '\n'