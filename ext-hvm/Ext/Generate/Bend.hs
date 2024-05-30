{-# LANGUAGE OverloadedStrings #-}

module Ext.Generate.Bend
  ( generate,
  )
where

import qualified AST.Optimized as Opt
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Name (Name)
import qualified Data.Set as Set
import qualified Elm.ModuleName as ModuleName
import qualified Generate.Mode as Mode

-- GENERATE

type Graph = Map.Map Opt.Global Opt.Node

type Mains = Map.Map ModuleName.Canonical Opt.Main

generate :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generate mode (Opt.GlobalGraph graph _) mains =
  let state = Map.foldrWithKey (addMain mode graph) emptyState mains
   in stateToBuilder state
        <> "\n\n# wassup!"

addMain :: Mode.Mode -> Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home "main")

-- GRAPH TRAVERSAL STATE

data State = State
  { _revBuilders :: [B.Builder],
    _seenGlobals :: Set.Set Opt.Global
  }

emptyState :: State
emptyState =
  State
    initBuilders
    Set.empty

stateToBuilder :: State -> B.Builder
stateToBuilder (State revBuilders _) =
  prependBuilders revBuilders mempty

prependBuilders :: [B.Builder] -> B.Builder -> B.Builder
prependBuilders revBuilders monolith =
  List.foldl' (\m b -> b <> m) monolith revBuilders

initBuilders :: [B.Builder]
initBuilders =
  tupleGetters

tupleGetters :: [B.Builder]
tupleGetters =
  [ "_Elm.GetTuple.el0 (a,*) = a",
    "_Elm.GetTuple.el1 (*,b) = b",
    "_Elm.GetTriple.el0 (a,*)     = a",
    "_Elm.GetTriple.el1 (*,(b,*)) = b",
    "_Elm.GetTriple.el2 (*,(*,c)) = c"
  ]

-- ADD DEPENDENCIES

addGlobal :: Mode.Mode -> Graph -> State -> Opt.Global -> State
addGlobal mode graph state@(State revBuilders seen) global =
  if Set.member global seen
    then state
    else
      addGlobalHelp mode graph global $
        State revBuilders (Set.insert global seen)

addGlobalHelp :: Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlobalHelp mode graph global state =
  let addDeps deps someState =
        Set.foldl' (addGlobal mode graph) someState deps
   in case graph ! global of
        Opt.Define expr deps ->
          let stateWithDeps = addDeps deps state
           in addValueDecl global expr stateWithDeps
        Opt.DefineTailFunc argNames body deps ->
          let stateWithDeps = addDeps deps state
           in addFunctionDecl global argNames body stateWithDeps
        Opt.Ctor index arity ->
          -- addStmt
          --   state
          --   ( var global (Expr.generateCtor mode global index arity)
          --   )
          error "TODO Opt.Ctor"
        Opt.Link linkedGlobal ->
          -- addGlobal mode graph state linkedGlobal
          error "TODO Opt.Link"
        Opt.Cycle names values functions deps ->
          -- addStmt
          --   (addDeps deps state)
          --   ( generateCycle mode global names values functions
          --   )
          error "TODO Opt.Cycle"
        Opt.Manager effectsType ->
          -- generateManager mode graph global effectsType state
          error "TODO Opt.Manager"
        Opt.Kernel chunks deps ->
          -- if isDebugger global && not (Mode.isDebug mode)
          --   then state
          --   else addKernel (addDeps deps state) (generateKernel mode chunks)
          error "TODO Opt.Kernel"
        Opt.Enum index ->
          -- addStmt
          --   state
          --   ( generateEnum mode global index
          --   )
          error "TODO Opt.Enum"
        Opt.Box ->
          -- addStmt
          --   (addGlobal mode graph state identity)
          --   ( generateBox mode global
          --   )
          error "TODO Opt.Box"
        Opt.PortIncoming decoder deps ->
          -- addStmt
          --   (addDeps deps state)
          --   ( generatePort mode global "incomingPort" decoder
          --   )
          error "TODO Opt.PortIncoming"
        Opt.PortOutgoing encoder deps ->
          -- addStmt
          --   (addDeps deps state)
          --   ( generatePort mode global "outgoingPort" encoder
          --   )
          error "TODO Opt.PortOutgoing"

addValueDecl :: Opt.Global -> Opt.Expr -> State -> State
addValueDecl global expr state =
  error "TODO addValueDecl"

addFunctionDecl :: Opt.Global -> [Name] -> Opt.Expr -> State -> State
addFunctionDecl global argNames body state =
  error "TODO addFunctionDecl"
