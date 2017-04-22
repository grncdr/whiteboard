module Confirm exposing (confirm)

import Native.Confirm


confirm : String -> Bool
confirm =
    Native.Confirm.confirm
