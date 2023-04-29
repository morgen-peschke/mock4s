Mock4s
======

Overview
--------

`mock4s` is a simple mocked server built on http4s and cats. Currently it is extremely bare-bones, with an emphasis on
making running it as painless as possible.

Quickstart
----------

The quickest way to get a server running is using `CliApp` main class: 

```bash
mill -i core.runMain --mainClass peschke.mock4s.CliApp --port 9010 --settings 'json:[]'
```

As this starts a server without any mocks defined, it's only useful when subsequent set up is scripted. The fastest way 
to get a server running that actually does anything is often using a settings file:

```bash
mill -i core.runMain --mainClass peschke.mock4s.CliApp --port 9010 --settings 'file:mocks.json'
```

`mocks.json` is a file with the mock definitions, a simple example of a definition for a server which always returns the
text `"Hello World"` would look like this:

```json
[
  {
    "name": "Hello World",
    "route": "always",
    "actions": [
      {
        "name": "fixed response",
        "when": "always",
        "respond-with": {
          "status": 200,
          "headers": [],
          "body": {
            "text": "Hello World!"
          }
        }
      }
    ]
  }
]
```

How to run
----------

### Command line via [`CliApp`](core/src/peschke/mock4s/CliApp.scala)

For interactive use, the most comfortable way to start up an instance of the server is using configuration taken from 
the command line. The easiest way to get information on the command line parameters is the integrated help.

```bash
mill -i core.runMain --mainClass peschke.mock4s.CliApp --help
```

#### Command line via [`EntryPoint`](core/src/peschke/mock4s/EntryPoint.scala)

For situations where starting an instance of the server using command line parameters isn't desirable, `EntryPoint`
allows configuration using environment variables.

```bash
mill -i core.runMain --mainClass peschke.mock4s.EntryPoint
```

#### Embedding a server

Constructing the configuration programmatically and starting the server directly is also very straightforward, as
[`peschke.mock4s.Config`](core/src/peschke/mock4s/Config.scala) is a case class, and 
[`peschke.mock4s.SetupServer.run`](core/src/peschke/mock4s/SetupServer.scala) is implemented in terms of the standard
cats-effect typeclasses.

Querying/Modifying Server Behavior
----------------------------------
Basic CRUD endpoints are provided to view and adjust the current settings. 
See [`SettingsRoutes`](core/src/peschke/mock4s/SettingsRoutes.scala) for details

Mock Definition
---------------

Mocks are defined in a settings file, which is JSON file with a particular format. The top-level is an array, each
entry is a mock definition. 

These are ordered so, as a practical matter, only a single mock can be defined with `"route": "always"`, and it must 
be the final mock. Such a mock is not strictly necessary, as a standard "Not Found" response will be returned if no 
mock matches an incoming request.

```javascript
[
  {/* highest priority mock /*},
  {/* second-highest priority mock /*},
  /* ... */
  {/* lowest priority mock/*}
]
```

### Semantics

Each mock has three parts, a name, a route it handles, and a series of possible actions. 

#### Name
The `name` is a string, and is used for logging and as an identifier for CRUD operations. It must be unique within a 
server.

#### Route
The `route` is defines conditions when this mock takes responsibility for creating a response. Once a mock matches, 
no other mock is considered, even if none of the mock's actions match.

#### Actions
`actions` is an array of conditions and responses, again ordered. If none of the conditions match, a 404 response will
be generated.

Actions also have a name which is used for logging and as an identifier for CRUD operations. The name must be unique
within a mock.

### Schema

The schema for `Settings` isn't really conductive to a readable JSON schema, and writing a BNF grammar would involve
duplicating a bunch of JSON grammar, so this schema will be presented in a hybrid format based on BNF.

#### Conventions

- `UPPER_SNAKE_CASE` is used for identifiers
- `FOO?` is an optional modifier to a base identifier
- `FOO*` is `FOO` repeated any number of times
- `FOO+` is `FOO` repeated at least once
- `FOO(bar)` means `FOO` acts as a function, producing a modifier equivalent to what text replacement would produce.
  ```
  QUOTE(value) := "'" value "'"
  ESCAPE := QUOTE("\\")
  ```
  Would be equivalent to `ESCAPE := "'" "\\" "'"`
- `package.path.ClassName` means the definition of this is is deferred to the referenced class
- `()` is used for grouping when necessary
- Anything else (like `[]`) is unquoted to cut down on noise.

#### Definitions
```
"text"            := the literal JSON "text", commonly as keys or fixed values
'text'            := literal values that are't not wrapped in JSON
JBOOL             := 'true' | 'false'
JNULL             := 'null'
JNUMBER           := valid JSON number
JNUMBER..JNUMBER  := valid JSON number with a restricted range
JNUMBER.MAX       := maximum JSON number
JKEY              := valid key for a JSON object

JSTRING           := valid JSON string
CI_STRING         := valid JSON string, treated as case-insensitive
BARE_STRING       := valid JSON string, just without the enclosing double-quotes
REGEX             := valid JSON string, containing a valid regular expression
 
BASE64_BARE   := See https://datatracker.ietf.org/doc/html/rfc4648#section-4
BASE64_STRING := '"' BASE64_BARE? '"'

JSON              := any valid JSON
[ ELEM* ]         := JSON array of any number of values defined by ELEM
{ JKEY: VALUE }   := JSON object with key defined by KEY and value defined by VALUE
{ KEY_VALUE* }    := JSON object with any number of key/value pairs defined by KEY_VALUE
DEFINITION(param) := parameterized route (to avoid a bunch of repetition)
```

#### Settings Schema :: `SETTINGS`
```
MOCKS_ONLY := [ MOCK* ]
MOCKS_AND_STATE := { "mocks": MOCKS_ONLY, "state": STATE }
SETTINGS := MOCKS_ONLY | MOCKS_AND_STATE
```

#### State Schema :: `STATE`
```
STATE_ENTRY := JKEY: JSON
STATE := { KEY_VALUE* }
```

#### Mocks Schema :: `MOCK`
```
MOCK := {
  "name": JSTRING,
  "route": ROUTE,
  "actions": [ ACTION* ]
}
```

#### Fixed Schema :: `FIXED`
```
ALWAYS := "any"  | "always"
NEVER  := "fail" | "never"
```

#### Combinators Predicate Schema :: `COMBINATORS(predicate)`
```
FOR_ALL(predicate)     := { "forall": [ X* ] }
EXISTS(predicate)      := { "exists": [ X* ] }
NOT(predicate)         := { "!": X }
COMBINATORS(predicate) := { "forall": [ X* ] }
                        | { "exists": [ X* ] }
                        | { "!": X }
```

#### Equality Predicate Schema :: `EQ(value)`
```
EQ(value)  := { "is": value* }
            | { "in": [ value* ] }
```

#### Order Predicate Schema :: `ORDER(value)`
```
ORDER(value) := { "<": value } 
              | { "<=": value }
              | { ">": value }
              | { ">=": value }
```

#### String Predicate Schema :: `STRING_PREDICATE` 
```
STRING_PREDICATE := FIXED
                  | { "starts-with": JSTRING }
                  | { "ends-with": JSTRING }
                  | { "contains": JSTRING }
                  | { "matches": REGEX }
                  | EQ(java.lang.String) 
                  | COMBINATORS(STRING_PREDICATE)
```

#### JSON Path Schema :: `JSON_PATH`
```
DIGIT  := 0..9
LETTER_OR_DIGIT := java.lang.Char#isLetterOrDigit

VALID_QUOTED_FIELD_CHAR := '\\' | '\b' | '\f' | '\n' | '\t'
                         | '\u' DIGIT DIGIT DIGIT DIGIT
                         | java.lang.Char

DOWN_ARRAY := [ DIGIT* ]
            
BARE_FIELD       := LETTER_OR_DIGIT | - | _
BARE_FIELD_CHAIN := BARE_FIELD (. BARE_FIELD)*
QUOTED_FIELD     := '["' VALID_QUOTED_FIELD_CHAR '"]'

SEGMENT := .? DOWN_ARRAY
         | .? QUOTED_FIELD
         | . BARE_FIELD

JSON_PATH := $ SEGMENT*
           | ''

```

#### JSON Predicate Schema :: `JSON_PREDICATE`
```
NUMBER_PREDICATE := FIXED 
                  | EQ(scala.math.BigDecimal) 
                  | ORDER(scala.math.BigDecimal)
                  
JSON_TESTS := { "string": STRING_PREDCIATE }
            | { "number": NUMBER_PREDICATE }
            | { "at": '"' JSON_PATH '"', "when": JSON_PREDICATE }

JSON_PREDICATE := FIXED 
                | EQ(io.circe.Jsno) 
                | JSON_TESTS 
                | COMBINATORS(JSON_PREDICATE)
```

#### State Predicate Schema :: `STATE_PREDICATE`
```
STATE_NAME      := JKEY
STATE_PREDICATE := { "cleared": JKEY }
                 | { "set": { STATE_NAME: JSON_PREDICATE }
```

#### Route Predicate Schema :: `ROUTE_PREDICATE`
```
METHOD_PRED := FIXED | EQ(org.http4s.Method) | COMBINATORS(METHOD_PRED)

SLASH               := /
PATH_SEGMENT        := '*' | BARE_STRING
RELATIVE_PATH       := PATH_SEGMENT (SLASH PATH_SEGMENT)?
ABSOLUTE_PATH       := SLASH RELATIVE_PATH SLASH?
SANITIZED_PATH      := ABSOLUTE_PATH | RELATIVE_PATH
SANITIZED_PREDICATE := { "sanitized": '"' SANITIZED_PATH '"' }

PATH_PRED := FIXED | EQ(org.http4s.Uri.Path) | SANITIZED_PREDICATE | COMBINATORS(PATH_PRED)

QUERY_PRED :=  FIXED | EQ(org.http4s.Query) | COMBINATORS(QUERY_PRED)

ROUTE_PREDICATE := { "method": METHOD_PRED } 
                 | { "path": PATH_PRED } 
                 | { "query": QUERY_PRED } 
                 | { "state": STATE_PREDICATE }
                 | FIXED 
                 | COMBINATORS(ROUTE_PREDICATE) 
                 | EQ(peschke.mock4s.models.ParsedRequest.Route)
```

#### Request Predicate Schema :: `REQUEST_PREDICATE`
```
HEADER_NAME       := CI_STRING
HEADER_PREDICATE  := { HEADER_NAME: STRING_PREDICATE }

BASE64_STRING_PREDICATE := FIXED | UsingEq(BASE64_BARE)

BODY_PREDICATE := "empty" 
                | { "text": STRING_PREDICATE } 
                | { "json": JSON_PREDICATE } 
                | { "raw" : BASE64_STRING_PREDICATE }

REQUEST_PREDICATE := FIXED 
                   | { "route": ROUTE_PREDICATE } 
                   | { "headers": [ HEADER_PREDICATE* ] } 
                   | { "body": BODY_PREDICATE }
                   | { "state": STATE_PREDICATE } 
                   | COMBINATORS(REQUEST_PREDICATE)
```

#### Body Definition Schema :: `BODY_DEFINITION`
```
TEXT_BODY := { "text": JSTRING" }
JSON_BODY := { "json": JSON }
RAW_BODY  := { "bytes": BASE64_STRING }
BODY_DEFINITION := "empty" | TEXT_BODY | JSON_BODY | RAW_BODY
```

#### State Transition Schema :: `STATE_TRANSITION`
```
STATE_ENTRY := JKEY: JSON
STATE_TRANSITION := { "clear": [ JKEY* ] }
                  | { "set": { STATE_ENTRY* } }
```

#### Action Schema :: `ACTION`
```
HTTP_VERSION_STR := JSTRING // See org.http4s.HttpVersion.fromVersion
HTTP_VERSION_OBJ := {
  "major": 0..JSNUMBER.MAX,
  "minor": 0..JNUMBER.MAX
}
HTTP_VERSION := HTTP_VERSION_STR | HTTP_VERSION_OBJ

HEADER := { "name": JSTRING, "value: JSTRING }

RESPONSE_DEFINITION := {
  "status": 200..599,
  ("httpVersion": HTTP_VERSION,)?
  ("headers": [ HEADER* ],)?
  ("state": [ STATE_TRANSITION* ],)?
  "body": BODY_DEFINITION
}

ACTION := {
  "name": JSTRING,
  "when": REQUEST_PREDICATE,
  "respond-with": RESPONSE_DEFINITION
}
```