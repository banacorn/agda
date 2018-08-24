{-# LANGUAGE OverloadedStrings #-}

module Agda.Interaction.JSONTop
    ( jsonREPL
    ) where
import Control.Monad.State

import Data.Aeson hiding (Result(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Agda.Interaction.Encode
import Agda.Interaction.AgdaTop
import Agda.Interaction.Response as R
import Agda.Interaction.EmacsCommand hiding (putResponse)
import Agda.Interaction.Highlighting.JSON
import Agda.Interaction.Highlighting.Precise (TokenBased(..))
import Agda.Syntax.Common
import Agda.TypeChecking.Monad
import Agda.Utils.Pretty
import Agda.VersionCommit

--------------------------------------------------------------------------------

-- | 'jsonREPL' is a interpreter like 'mimicGHCi', but outputs JSON-encoded strings.
--
--   'jsonREPL' reads Haskell values (that starts from 'IOTCM' ...) from stdin,
--   interprets them, and outputs JSON-encoded strings. into stdout.

jsonREPL :: TCM () -> TCM ()
jsonREPL = repl (liftIO . BS.putStrLn <=< return . encode <=< encodeTCM) "JSON> "

--------------------------------------------------------------------------------

instance EncodeTCM Response where
  encodeTCM (Resp_HighlightingInfo info remove method modFile) =
    liftIO (encodeHighlightingInfo info remove method modFile)
  encodeTCM (Resp_DisplayInfo info) = return $ object
    [ "kind" .= String "DisplayInfo"
    , "info" .= info
    ]
  encodeTCM (Resp_ClearHighlighting tokenBased) = return $ object
    [ "kind"          .= String "ClearHighlighting"
    , "tokenBased"    .= tokenBased
    ]
  encodeTCM Resp_DoneAborting = return $ object [ "kind" .= String "DoneAborting" ]
  encodeTCM Resp_ClearRunningInfo = return $ object [ "kind" .= String "ClearRunningInfo" ]
  encodeTCM (Resp_RunningInfo debugLevel msg) = return $ object
    [ "kind"          .= String "RunningInfo"
    , "debugLevel"    .= debugLevel
    , "message"       .= msg
    ]
  encodeTCM (Resp_Status status) = return $ object
    [ "kind"          .= String "Status"
    , "status"        .= status
    ]
  encodeTCM (Resp_JumpToError filepath position) = return $ object
    [ "kind"          .= String "JumpToError"
    , "filepath"      .= filepath
    , "position"      .= position
    ]
  encodeTCM (Resp_InteractionPoints interactionPoints) = return $ object
    [ "kind"              .= String "InteractionPoints"
    , "interactionPoints" .= interactionPoints
    ]
  encodeTCM (Resp_GiveAction i giveResult) = return $ object
    [ "kind"              .= String "GiveAction"
    , "interactionPoint"  .= i
    , "giveResult"        .= giveResult
    ]
  encodeTCM (Resp_MakeCase variant clauses) = return $ object
    [ "kind"          .= String "MakeCase"
    , "variant"       .= variant
    , "clauses"       .= clauses
    ]
  encodeTCM (Resp_SolveAll solutions) = return $ object
    [ "kind"          .= String "SolveAll"
    , "solutions"     .= map encodeSolution solutions
    ]
    where
      encodeSolution (i, expr) = object
        [ "interactionPoint"  .= i
        , "expression"        .= show expr
        ]

--------------------------------------------------------------------------------

instance ToJSON DisplayInfo where
  toJSON (Info_CompilationOk warnings errors) = object
    [ "kind"        .= String "CompilationOk"
    , "warnings"    .= warnings
    , "errors"      .= errors
    ]
  toJSON (Info_Constraints constraints) = object
    [ "kind"        .= String "Constraints"
    , "constraints" .= constraints
    ]
  toJSON (Info_AllGoalsWarnings goals warnings errors) = object
    [ "kind"        .= String "AllGoalsWarnings"
    , "goals"       .= goals
    , "warnings"    .= warnings
    , "errors"      .= errors
    ]
  toJSON (Info_Time doc) = object [ "kind" .= String "Time", "payload" .= render doc ]
  toJSON (Info_Error msg) = object [ "kind" .= String "Error", "payload" .= msg ]
  toJSON (Info_Intro doc) = object [ "kind" .= String "Intro", "payload" .= render doc ]
  toJSON (Info_Auto msg) = object [ "kind" .= String "Auto", "payload" .= msg ]
  toJSON (Info_ModuleContents doc) = object [ "kind" .= String "ModuleContents", "payload" .= render doc ]
  toJSON (Info_SearchAbout doc) = object [ "kind" .= String "SearchAbout", "payload" .= render doc ]
  toJSON (Info_WhyInScope doc) = object [ "kind" .= String "WhyInScope", "payload" .= render doc ]
  toJSON (Info_NormalForm doc) = object [ "kind" .= String "NormalForm", "payload" .= render doc ]
  toJSON (Info_GoalType doc) = object [ "kind" .= String "GoalType", "payload" .= render doc ]
  toJSON (Info_CurrentGoal doc) = object [ "kind" .= String "CurrentGoal", "payload" .= render doc ]
  toJSON (Info_InferredType doc) = object [ "kind" .= String "InferredType", "payload" .= render doc ]
  toJSON (Info_Context doc) = object [ "kind" .= String "Context", "payload" .= render doc ]
  toJSON (Info_HelperFunction doc) = object [ "kind" .= String "HelperFunction", "payload" .= render doc ]
  toJSON Info_Version = object
    [ "kind" .= String "Version"
    , "version" .= (("Agda version " ++ versionWithCommitInfo) :: String)
    ]

instance EncodeTCM DisplayInfo where
  encodeTCM = return . toJSON

instance ToJSON Status where
  toJSON status = object
    [ "showImplicitArguments" .= sShowImplicitArguments status
    , "checked" .= sChecked status
    ]

instance ToJSON InteractionId where
  toJSON (InteractionId i) = toJSON i

instance ToJSON GiveResult where
  toJSON (Give_String s) = toJSON s
  toJSON Give_Paren = toJSON True
  toJSON Give_NoParen = toJSON False

instance ToJSON MakeCaseVariant where
  toJSON R.Function = String "Function"
  toJSON R.ExtendedLambda = String "ExtendedLambda"
