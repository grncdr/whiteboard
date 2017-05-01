module Util exposing (..)

import Task exposing (Task)

attemptWith : (a -> msg) -> (x -> msg) -> Task x a -> Cmd msg
attemptWith onSuccess onError task =
  let mapResult res = case res of
    Ok yay -> onSuccess yay
    Err nay -> onError nay
  in Task.attempt mapResult task
