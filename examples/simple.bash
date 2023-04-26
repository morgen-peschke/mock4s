#!/usr/bin/env bash

# mill core.runMain --mainClass peschke.mock4s.CliApp --port 9000 --settings file:examples/simple.json

# *   Trying [::1]:9000...
# * Connected to ::1 (::1) port 9000 (#0)
# > GET / HTTP/1.1
# > Host: [::1]:9000
# > User-Agent: curl/7.87.0
# > Accept: */*
# >
# * Mark bundle as not supporting multiuse
# * HTTP 1.0, assume close after body
# < HTTP/1.0 200 OK
# < Date: Wed, 26 Apr 2023 19:51:31 GMT
# * HTTP/1.0 connection set to keep alive
# < Connection: keep-alive
# < Content-Length: 14
# < Content-Type: application/json
# <
# * Connection #0 to host ::1 left intact
# "Hello World!"
curl -v 'http://[::1]:9000/'; printf '\n'