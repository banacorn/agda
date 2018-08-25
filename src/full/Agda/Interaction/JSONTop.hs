{-# LANGUAGE OverloadedStrings #-}

module Agda.Interaction.JSONTop
    ( jsonREPL
    ) where
import Control.Monad.State

import Data.Aeson hiding (Result(..))
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.List (nub, sortBy)
import qualified Data.ByteString.Lazy.Char8 as BS

import Agda.Interaction.Encoding
import Agda.Interaction.AgdaTop
import Agda.Interaction.Response as R
import Agda.Interaction.EmacsCommand hiding (putResponse)
import Agda.Interaction.Highlighting.JSON
import Agda.Interaction.Highlighting.Precise (TokenBased(..))
import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Concrete as C
import Agda.Syntax.Common
import qualified Agda.Syntax.Fixity as F
import qualified Agda.Syntax.Internal as I
import qualified Agda.Syntax.Notation as N
import Agda.Syntax.Position
  (Range, Range'(..), Interval, Interval'(..), Position, Position'(..))
import qualified Agda.Syntax.Scope.Base as S
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Errors (topLevelModuleDropper)
import Agda.Utils.Pretty
import qualified Agda.Utils.Maybe.Strict as Strict
import Agda.Utils.FileName (AbsolutePath(..))
import Agda.Utils.AssocList (AssocList)
import qualified Agda.Utils.AssocList as Assoc
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
  encodeTCM (Resp_DisplayInfo info) = do
    info' <- encodeTCM info
    return $ object
      [ "kind" .= String "DisplayInfo"
      , "info" .= info'
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

instance EncodeTCM DisplayInfo where
  encodeTCM (Info_CompilationOk warnings errors) = return $ object
    [ "kind"        .= String "CompilationOk"
    , "warnings"    .= warnings
    , "errors"      .= errors
    ]
  encodeTCM (Info_Constraints constraints) = return $ object
    [ "kind"        .= String "Constraints"
    , "constraints" .= constraints
    ]
  encodeTCM (Info_AllGoalsWarnings goals warnings errors) = return $ object
    [ "kind"        .= String "AllGoalsWarnings"
    , "goals"       .= goals
    , "warnings"    .= warnings
    , "errors"      .= errors
    ]
  encodeTCM (Info_Time doc) = return $ object [ "kind" .= String "Time", "payload" .= render doc ]
  encodeTCM (Info_Error err msg) = do
    err' <- encodeTCM err
    return $ object
      [ "kind" .= String "Error"
      , "error" .= err'
      ]
  encodeTCM (Info_Intro doc) = return $ object
    [ "kind" .= String "Intro", "payload" .= render doc ]
  encodeTCM (Info_Auto msg) = return $ object
    [ "kind" .= String "Auto", "payload" .= msg ]
  encodeTCM (Info_ModuleContents doc) = return $ object
    [ "kind" .= String "ModuleContents", "payload" .= render doc ]
  encodeTCM (Info_SearchAbout doc) = return $ object
    [ "kind" .= String "SearchAbout", "payload" .= render doc ]
  encodeTCM (Info_WhyInScope doc) = return $ object
    [ "kind" .= String "WhyInScope", "payload" .= render doc ]
  encodeTCM (Info_NormalForm doc) = return $ object
    [ "kind" .= String "NormalForm", "payload" .= render doc ]
  encodeTCM (Info_GoalType doc) = return $ object
    [ "kind" .= String "GoalType", "payload" .= render doc ]
  encodeTCM (Info_CurrentGoal doc) = return $ object
    [ "kind" .= String "CurrentGoal", "payload" .= render doc ]
  encodeTCM (Info_InferredType doc) = return $ object
    [ "kind" .= String "InferredType", "payload" .= render doc ]
  encodeTCM (Info_Context doc) = return $ object
    [ "kind" .= String "Context", "payload" .= render doc ]
  encodeTCM (Info_HelperFunction doc) = return $ object
    [ "kind" .= String "HelperFunction", "payload" .= render doc ]
  encodeTCM Info_Version = return $ object
    [ "kind" .= String "Version"
    , "version" .= (("Agda version " ++ versionWithCommitInfo) :: String)
    ]

instance EncodeTCM TCErr where
  encodeTCM (TypeError _ closure) = do
    typeError <- encodeTCM (clValue closure)
    return $ object
      [ "kind" .= String "TypeError"
      , "range" .= envRange (clEnv closure)
      , "typeError" .= typeError
      ]
  encodeTCM (Exception range doc) = return $ object
    [ "kind" .= String "Exception"
    , "range" .= range
    , "description" .= toJSON (render doc)
    ]
  encodeTCM (IOException _ range exception) = return $ object
    [ "kind" .= String "IOException"
    , "range" .= range
    , "exception" .= toJSON (show exception)
    ]
  encodeTCM PatternErr = return $ object
    [ "kind" .= String "PatternErr"
    , "range" .= (NoRange :: Range)
    ]

instance EncodeTCM TypeError where
  encodeTCM err = case err of
    InternalError s -> return $ object
      [ "kind" .= String "InternalError"
      , "message" .= toJSON s
      ]

    NotImplemented s -> return $ object
      [ "kind" .= String "NotImplemented"
      , "message" .= toJSON s
      ]

    NotSupported s -> return $ object
      [ "kind" .= String "NotSupported"
      , "message" .= toJSON s
      ]

    CompilationError s -> return $ object
      [ "kind" .= String "CompilationError"
      , "message" .= toJSON s
      ]

    GenericError s -> return $ object
      [ "kind" .= String "GenericError"
      , "message" .= toJSON s
      ]

    GenericDocError d -> return $ object
      [ "kind" .= String "GenericDocError"
      , "message" .= toJSON (render d)
      ]

    TerminationCheckFailed because -> do
      -- topLevelModuleDropper produces a function which drops the filename
      -- component of the qualified name.
      dropTopLevel <- topLevelModuleDropper
      let functionNames = map dropTopLevel (concatMap termErrFunctions because)
      let problematicCalls = sortBy (compare `on` callInfoRange)
            $ (concatMap termErrCalls because)
      return $ object
        [ "kind" .= String "TerminationCheckFailed"
        , "functions" .= functionNames
        , "problematicCalls" .= problematicCalls
        ]
    -- PropMustBeSingleton -> fwords
    --   "Datatypes in Prop must have at most one constructor when proof irrelevance is enabled"
    --
    -- DataMustEndInSort t -> fsep $
    --   pwords "The type of a datatype must end in a sort."
    --   ++ [prettyTCM t] ++ pwords "isn't a sort."



    _ -> return $ object [ "kind" .= String "error not handled yet"]

--------------------------------------------------------------------------------
-- Agda.TypeChecking.Monad.Base

instance ToJSON CallInfo where
  toJSON (CallInfo target range call) = object
    [ "target"  .= target
    , "range"   .= range
    , "call"    .= call
    ]

instance ToJSON a => ToJSON (Closure a) where
  toJSON (Closure sig env scope checkpoints value) = object
    [ "value" .= value ]
    -- [ "signature"   .= sig
    -- , "environment" .= env
    -- , "scope"       .= scope
    -- , "checkpoints" .= checkpoints
    -- , "value"       .= value
    -- ]

instance ToJSON CheckpointId where
  toJSON (CheckpointId i) = toJSON i

--------------------------------------------------------------------------------
-- Agda.Syntax.Internal

instance ToJSON I.Term where
  toJSON term = toJSON (show term)

--------------------------------------------------------------------------------
-- Agda.Syntax.Common

instance ToJSON FreeVariables where
  toJSON UnknownFVs       = Null
  toJSON (KnownFVs vars)  = toJSON vars

instance ToJSON Modality where
  toJSON (Modality relevance quantity) = object
    [ "relevance" .= relevance
    , "quantity"  .= quantity
    ]

instance ToJSON Quantity where
  toJSON Quantity0  = String "Quantity0"
  toJSON Quantityω  = String "Quantityω"

instance ToJSON Relevance where
  toJSON Relevant   = String "Relevant"
  toJSON NonStrict  = String "NonStrict"
  toJSON Irrelevant = String "Irrelevant"

instance ToJSON Overlappable where
  toJSON YesOverlap = String "YesOverlap"
  toJSON NoOverlap  = String "NoOverlap"

instance ToJSON Hiding where
  toJSON Hidden     = object [ "kind" .= String "Hidden" ]
  toJSON NotHidden  = object [ "kind" .= String "NotHidden" ]
  toJSON (Instance overlappable) = object
    [ "kind"          .= String "Instance"
    , "overlappable"  .= overlappable
    ]

instance ToJSON Origin where
  toJSON UserWritten  = String "UserWritten"
  toJSON Inserted     = String "Inserted"
  toJSON Reflected    = String "Reflected"
  toJSON CaseSplit    = String "CaseSplit"
  toJSON Substitution = String "Substitution"

instance ToJSON ArgInfo where
  toJSON (ArgInfo hiding modality origin freeVars) = object
    [ "hiding"    .= hiding
    , "modality"  .= modality
    , "origin"    .= origin
    , "freeVars"  .= freeVars
    ]

instance ToJSON NameId where
  toJSON (NameId name modul) = object
    [ "name"    .= name
    , "module"  .= modul
    ]

instance ToJSON a => ToJSON (Ranged a) where
  toJSON (Ranged range payload) = object
    [ "range"    .= range
    , "payload" .= payload
    ]

instance (ToJSON name, ToJSON a) => ToJSON (Named name a) where
  toJSON (Named name payload) = object
    [ "name"    .= name
    , "payload" .= payload
    ]

instance ToJSON a => ToJSON (Arg a) where
  toJSON (Arg info payload) = object
    [ "info"    .= info
    , "payload" .= payload
    ]

instance ToJSON DataOrRecord where
  toJSON IsData   = String "IsData"
  toJSON IsRecord = String "IsRecord"

--------------------------------------------------------------------------------
-- Agda.Syntax.Concrete

instance ToJSON C.NamePart where
  toJSON C.Hole = Null
  toJSON (C.Id name) = toJSON name

instance ToJSONKey C.Name

instance ToJSON C.Name where
  toJSON (C.Name   range parts) = object
    [ "kind"  .= String "Name"
    , "range" .= range
    , "parts" .= parts
    ]
  toJSON (C.NoName range name) = object
    [ "kind"  .= String "NoName"
    , "range" .= range
    , "name"  .= name
    ]

instance ToJSONKey C.QName
instance ToJSON C.QName where
  toJSON (C.QName name) = object
    [ "kind"  .= String "QName"
    , "name"  .= name
    ]
  toJSON (C.Qual name qname) = object
    [ "kind"      .= String "Qual"
    , "name"      .= name
    , "namespace" .= qname
    ]

--------------------------------------------------------------------------------
-- Agda.Syntax.Abstract

instance ToJSON A.Name where
  toJSON (A.Name name concrete bindingSite fixity) = object
    [ "id"          .= name
    , "concrete"    .= concrete
    , "bindingSite" .= bindingSite
    , "fixity"      .= fixity
    ]

instance ToJSONKey A.QName
instance ToJSON A.QName where
  toJSON (A.QName moduleName name) = object
    [ "module"  .= moduleName
    , "name"    .= name
    ]

-- instance ToJSON a => ToJSON (A.QName a) where
--   toJSON (A.QNamed qname named) = object
--     [ "qname" .= qname
--     , "named" .= named
--     ]

instance ToJSONKey A.ModuleName where
instance ToJSON A.ModuleName where
  toJSON (A.MName names) = toJSON names

--------------------------------------------------------------------------------
-- Agda.Syntax.Fixity

instance ToJSON F.ParenPreference where
  toJSON F.PreferParen = String "PreferParen"
  toJSON F.PreferParenless = String "PreferParenless"

instance ToJSON F.Precedence where
  toJSON F.TopCtx = object
    [ "kind" .= String "TopCtx" ]
  toJSON F.FunctionSpaceDomainCtx = object
    [ "kind" .= String "FunctionSpaceDomainCtx" ]
  toJSON (F.LeftOperandCtx fixity) = object
    [ "kind" .= String "FunctionSpaceDomainCtx"
    , "fixity" .= fixity
    ]
  toJSON (F.RightOperandCtx fixity parentPref) = object
    [ "kind" .= String "RightOperandCtx"
    , "fixity" .= fixity
    , "parenPreference" .= parentPref
    ]
  toJSON F.FunctionCtx = object
    [ "kind" .= String "FunctionCtx" ]
  toJSON (F.ArgumentCtx parentPref) = object
    [ "kind" .= String "ArgumentCtx"
    , "parenPreference" .= parentPref
    ]
  toJSON F.InsideOperandCtx = object
    [ "kind" .= String "InsideOperandCtx" ]
  toJSON F.WithFunCtx = object
    [ "kind" .= String "WithFunCtx" ]
  toJSON F.WithArgCtx = object
    [ "kind" .= String "WithArgCtx" ]
  toJSON F.DotPatternCtx = object
    [ "kind" .= String "DotPatternCtx" ]

instance ToJSON F.PrecedenceLevel where
  toJSON F.Unrelated = Null
  toJSON (F.Related n) = toJSON n

instance ToJSON F.Associativity where
  toJSON F.NonAssoc = String "NonAssoc"
  toJSON F.LeftAssoc = String "LeftAssoc"
  toJSON F.RightAssoc = String "RightAssoc"

instance ToJSON F.Fixity where
  toJSON (F.Fixity range level assoc) = object
    [ "range" .= range
    , "level" .= level
    , "assoc" .= assoc
    ]

instance ToJSON F.Fixity' where
  toJSON (F.Fixity' fixity notation range) = object
    [ "fixity"    .= fixity
    , "notation"  .= notation
    , "range"     .= range
    ]

--------------------------------------------------------------------------------
-- Agda.Syntax.Notation

instance ToJSON N.GenPart where
  toJSON (N.BindHole n) = object
    [ "kind"      .= String "BindHole"
    , "position"  .= n
    ]
  toJSON (N.NormalHole n) = object
    [ "kind"      .= String "NormalHole"
    , "position"  .= n
    ]
  toJSON (N.WildHole n) = object
    [ "kind"      .= String "WildHole"
    , "position"  .= n
    ]
  toJSON (N.IdPart name) = object
    [ "kind"      .= String "IdPart"
    , "rawName"   .= name
    ]


--------------------------------------------------------------------------------
-- Agda.Syntax.Scope.Base

instance ToJSON S.Scope where
  toJSON (S.Scope name parents namespaces imports datatypeModule) = object
    [ "name"            .= name
    , "parents"         .= parents
    , "namespaces"      .= namespaces
    , "imports"         .= imports
    , "datatypeModule"  .= datatypeModule
    ]

instance ToJSON S.ScopeInfo where
  toJSON (S.ScopeInfo current modules vars locals prec inverseName inverseModule inScope) = object
    [ "current"       .= current
    , "modules"       .= modules
    , "varsToBind"    .= vars
    , "locals"        .= locals
    , "precedence"    .= prec
    , "inverseName"   .= inverseName
    , "inverseModule" .= inverseModule
    , "inScope"       .= inScope
    ]

instance ToJSON S.Binder where
  toJSON S.LambdaBound  = String "LambdaBound"
  toJSON S.PatternBound = String "PatternBound"
  toJSON S.LetBound     = String "LetBound"

instance ToJSON S.LocalVar where
  toJSON (S.LocalVar name binder shadowedBy) = object
    [ "name"        .= name
    , "binder"      .= binder
    , "shadowedBy"  .= shadowedBy
    ]

instance ToJSON S.NameSpaceId where
  toJSON S.PrivateNS        = String "PrivateNS"
  toJSON S.PublicNS         = String "PublicNS"
  toJSON S.ImportedNS       = String "ImportedNS"
  toJSON S.OnlyQualifiedNS  = String "OnlyQualifiedNS"

instance ToJSON S.NameSpace where
  toJSON (S.NameSpace names modules inScope) = object
    [ "names"   .= names
    , "modules" .= modules
    , "inScope" .= inScope
    ]

instance ToJSON S.KindOfName where
  toJSON S.ConName        = String "ConName"
  toJSON S.FldName        = String "FldName"
  toJSON S.DefName        = String "DefName"
  toJSON S.PatternSynName = String "PatternSynName"
  toJSON S.GeneralizeName = String "GeneralizeName"
  toJSON S.MacroName      = String "MacroName"
  toJSON S.QuotableName   = String "QuotableName"

instance ToJSON S.WhyInScope where
  toJSON S.Defined = object
    [ "kind"        .= String "Defined" ]
  toJSON (S.Opened name whyInScope) = object
    [ "kind"        .= String "Opened"
    , "name"        .= name
    , "whyInScope"  .= whyInScope
    ]
  toJSON (S.Applied name whyInScope) = object
    [ "kind"        .= String "Applied"
    , "name"        .= name
    , "whyInScope"  .= whyInScope
    ]

instance ToJSON S.AbstractName where
  toJSON (S.AbsName name kind whyInScope) = object
    [ "name"        .= name
    , "kind"        .= kind
    , "whyInScope"  .= whyInScope
    ]

instance ToJSON S.AbstractModule where
  toJSON (S.AbsModule moduleName whyInScope) = object
    [ "module"      .= moduleName
    , "whyInScope"  .= whyInScope
    ]

--------------------------------------------------------------------------------

instance ToJSON AbsolutePath where
  toJSON (AbsolutePath path) = toJSON path

instance ToJSON a => ToJSON (Strict.Maybe a) where
  toJSON (Strict.Just a) = toJSON a
  toJSON Strict.Nothing = Null

--------------------------------------------------------------------------------

instance ToJSON a => ToJSON (Position' a) where
  toJSON (Pn src pos line col) = toJSON
    [ toJSON line, toJSON col, toJSON pos, toJSON src ]

instance ToJSON a => ToJSON (Interval' a) where
  toJSON (Interval start end) = toJSON [toJSON start, toJSON end]

instance ToJSON a => ToJSON (Range' a) where
  toJSON (Range src is) = object
    [ "kind" .= String "Range"
    , "source" .= src
    , "intervals" .= is
    ]
  toJSON NoRange = object
    [ "kind" .= String "NoRange" ]

--------------------------------------------------------------------------------

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
