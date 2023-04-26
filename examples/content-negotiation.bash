#!/usr/bin/env bash

# mill core.runMain --mainClass peschke.mock4s.CliApp --port 9000 --settings file:examples/json-predicate.json

URI='http://[::1]:9000/'

# *   Trying [::1]:9000...
# * Connected to ::1 (::1) port 9000 (#0)
# > GET //foo HTTP/1.1
# > Host: [::1]:9000
# > User-Agent: curl/7.87.0
# > Accept: application/json
# >
# * Mark bundle as not supporting multiuse
# * HTTP 1.0, assume close after body
# < HTTP/1.0 200 OK
# < Date: Wed, 26 Apr 2023 20:23:59 GMT
# * HTTP/1.0 connection set to keep alive
# < Connection: keep-alive
# < Content-Length: 16
# < Content-Type: application/json
# <
# { [16 bytes data]
# * Connection #0 to host ::1 left intact
# {"method":"GET"}
curl -v --silent "$URI"/foo -H 'Accept: application/json'; printf '\n\n'

# *   Trying [::1]:9000...
# * Connected to ::1 (::1) port 9000 (#0)
# > GET //foo HTTP/1.1
# > Host: [::1]:9000
# > User-Agent: curl/7.87.0
# > Accept: text/plain
# >
# * Mark bundle as not supporting multiuse
# * HTTP 1.0, assume close after body
# < HTTP/1.0 200 OK
# < Date: Wed, 26 Apr 2023 20:25:40 GMT
# * HTTP/1.0 connection set to keep alive
# < Connection: keep-alive
# < Content-Length: 12
# < Content-Type: text/plain; charset=UTF-8
# <
# { [12 bytes data]
# * Connection #0 to host ::1 left intact
# Got the GET!
curl -v --silent "$URI"/foo -H 'Accept: text/plain'; printf '\n\n'

# *   Trying [::1]:9000...
# * Connected to ::1 (::1) port 9000 (#0)
# > GET //foo HTTP/1.1
# > Host: [::1]:9000
# > User-Agent: curl/7.87.0
# > Accept: application/octet-stream
# >
# * Mark bundle as not supporting multiuse
# * HTTP 1.0, assume close after body
# < HTTP/1.0 200 OK
# < Date: Wed, 26 Apr 2023 23:30:12 GMT
# * HTTP/1.0 connection set to keep alive
# < Connection: keep-alive
# < Content-Length: 4
# < Content-Type: application/octet-stream
# <
# * Connection #0 to host ::1 left intact
# ޭ��
curl -v --silent "$URI"/foo -H 'Accept: application/octet-stream'; printf '\n\n'

# 00000000: dead beef
curl --silent "$URI"/foo -H 'Accept: application/octet-stream' | xxd; printf '\n'

# *   Trying [::1]:9000...
# * Connected to ::1 (::1) port 9000 (#0)
# > GET //foo HTTP/1.1
# > Host: [::1]:9000
# > User-Agent: curl/7.87.0
# > Accept: */*
# >
# * Mark bundle as not supporting multiuse
# * HTTP 1.0, assume close after body
# < HTTP/1.0 204 No Content
# < Date: Wed, 26 Apr 2023 20:30:25 GMT
# * HTTP/1.0 connection set to keep alive
# < Connection: keep-alive
# <
# * Connection #0 to host ::1 left intact
curl -v --silent "$URI"/foo; printf '\n'