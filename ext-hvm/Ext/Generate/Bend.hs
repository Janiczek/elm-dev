{-# LANGUAGE OverloadedStrings #-}
module Ext.Generate.Bend
  ( generate
  )
  where

import qualified AST.Optimized as Opt
import qualified Generate.Mode as Mode

generate :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generate mode (Opt.GlobalGraph graph _) mains =
  let
    state = Map.foldrWithKey (addMain mode graph) emptyState mains
  in
  "(function(scope){\n'use strict';"
  <> Functions.functions
  <> perfNote mode
  <> stateToBuilder state
  <> toMainExports mode mains
  <> "}(this));"
