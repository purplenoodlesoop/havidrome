-- | The sentence each way a call can fail reads as, which is what the strip
-- along the bottom of whatever screen is up shows.
module Havidrome.Subsonic.TypesTest (tests) where

import Havidrome.Check (example)
import Havidrome.Subsonic.Types
  ( SubsonicError (AuthRejected, MalformedResponse, NetworkFailure, ServerFailure)
  , explain
  )
import Hedgehog (Group (Group), (===))

tests :: Group
tests =
  Group
    "Havidrome.Subsonic.Types"
    [
      ( "explain says a server could not be reached"
      , example
          ( explain (NetworkFailure "no route to host")
              === "The server could not be reached: no route to host"
          )
      )
    ,
      ( "explain says credentials were refused"
      , example
          ( explain (AuthRejected "Wrong username or password")
              === "The server refused these credentials: Wrong username or password"
          )
      )
    ,
      ( "explain says a server answered with a failure, with the code it gave"
      , example
          ( explain (ServerFailure 70 "Album not found")
              === "The server answered with an error (70): Album not found"
          )
      )
    ,
      ( "explain says an answer could not be read"
      , example
          ( explain (MalformedResponse "not JSON")
              === "The server's answer could not be read: not JSON"
          )
      )
    ]
