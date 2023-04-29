#!/usr/bin/env bash

# mill core.runMain --mainClass peschke.mock4s.CliApp --port 9000 --settings file:examples/state.json5

URI='http://[::1]:9000'

newline () { printf '\n'; }

# {"get:state":-1}
curl "$URI"/mock4s/state/all; newline; newline

# {"state":-1}
# {"get:state":0}
curl "$URI"/foo/cycle; newline
curl "$URI"/mock4s/state/all; newline

newline

# {"state":0}
# {"get:state":1,"get:cycling":true}
# {"state":1}
# {"get:state":2,"get:cycling":true}
# {"state":2}
# {"get:state":0}
for i in {0..2}; do
  curl "$URI"/foo/cycle; newline
  curl "$URI"/mock4s/state/all; newline
done

newline

# {"updated":"get:cycling"}
# {"updated":"get:state"}
curl "$URI/mock4s/state/keys/get:cycling" --json 'true'; newline
curl "$URI/mock4s/state/keys/get:state" --json 1; newline

newline

# {"state":1}
# {"get:state":2,"get:cycling":true}
curl "$URI"/foo/cycle; newline
curl "$URI"/mock4s/state/all; newline

newline

# {"deleted-all":true}
# {}
curl -X DELETE "$URI"/mock4s/state/keys; newline
curl "$URI"/mock4s/state/all; newline

newline

# {"reset":true}
# {"get:state":-1}
curl -X POST "$URI"/mock4s/state/reset; newline
curl "$URI"/mock4s/state/all; newline