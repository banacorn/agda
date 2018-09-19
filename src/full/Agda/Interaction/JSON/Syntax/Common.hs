{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Instances of EncodeTCM or ToJSON under Agda.Syntax.Common

module Agda.Interaction.JSON.Syntax.Common where

import Control.Monad ((>=>))
import Data.Aeson

import Agda.Interaction.JSON.Encode
import Agda.Interaction.JSON.Syntax.Position
import Agda.Syntax.Common
import qualified Agda.Syntax.Translation.InternalToAbstract as I2A
import qualified Agda.Syntax.Translation.AbstractToConcrete as A2C

--------------------------------------------------------------------------------
-- | Instances of EncodeTCM

instance EncodeTCM DataOrRecord
instance EncodeTCM Hiding
instance EncodeTCM ArgInfo
instance EncodeTCM MetaId
instance EncodeTCM ProjOrigin

instance (EncodeTCM a, EncodeTCM b) => EncodeTCM (ImportedName' a b) where
  encodeTCM (ImportedModule value) = kind "ImportedModule"
    [ "value"       @= value
    ]
  encodeTCM (ImportedName value) = kind "ImportedName"
    [ "value"       @= value
    ]

instance (EncodeTCM a) => EncodeTCM (Arg a) where
  encodeTCM (Arg argInfo value) = obj
    [ "argInfo"     @= argInfo
    , "value"       @= value
    ]

instance (EncodeTCM name, EncodeTCM a) => EncodeTCM (Named name a) where
  encodeTCM (Named name value) = obj
    [ "name"        @= name
    , "value"       @= value
    ]

instance EncodeTCM a => EncodeTCM (Ranged a) where
  encodeTCM (Ranged range value) = obj
    [ "range"       @= range
    , "value"       @= value
    ]

instance (I2A.Reify i a, A2C.ToConcrete a c, EncodeTCM c) => EncodeTCM (Dom i) where
  encodeTCM = I2A.reify >=> A2C.abstractToConcrete_ >=> encodeTCM

instance EncodeTCM a => EncodeTCM (WithHiding a) where
  encodeTCM (WithHiding hiding value) = obj
    [ "hiding"      @= hiding
    , "value"       @= value
    ]


--------------------------------------------------------------------------------
-- | Instances of ToJSON

instance ToJSON HasEta where
  toJSON NoEta  = String "NoEta"
  toJSON YesEta = String "YesEta"

instance ToJSON Induction where
  toJSON Inductive   = String "Inductive"
  toJSON CoInductive = String "CoInductive"

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

instance ToJSON a => ToJSON (WithHiding a) where
  toJSON (WithHiding hiding value) = object
    [ "hiding"  .= hiding
    , "value"   .= value
    ]

instance ToJSON Origin where
  toJSON UserWritten  = String "UserWritten"
  toJSON Inserted     = String "Inserted"
  toJSON Reflected    = String "Reflected"
  toJSON CaseSplit    = String "CaseSplit"
  toJSON Substitution = String "Substitution"

instance ToJSON Modality where
  toJSON (Modality relevance quantity) = object
    [ "relevance" .= relevance
    , "quantity"  .= quantity
    ]

instance ToJSON Quantity where
  toJSON Quantity0  = String "Quantity0"
  toJSON Quantityω  = String "QuantityOmega"

instance ToJSON Relevance where
  toJSON Relevant   = String "Relevant"
  toJSON NonStrict  = String "NonStrict"
  toJSON Irrelevant = String "Irrelevant"

instance ToJSON FreeVariables where
  toJSON UnknownFVs       = Null
  toJSON (KnownFVs vars)  = toJSON vars

instance ToJSON ArgInfo where
  toJSON (ArgInfo hiding modality origin freeVars) = object
    [ "hiding"    .= hiding
    , "modality"  .= modality
    , "origin"    .= origin
    , "freeVars"  .= freeVars
    ]

instance ToJSON a => ToJSON (Arg a) where
  toJSON (Arg argInfo value) = object
    [ "argInfo" .= argInfo
    , "value"   .= value
    ]


instance ToJSON a => ToJSON (Dom a) where
  toJSON (Dom argInfo finite value) = object
    [ "argInfo" .= argInfo
    , "finite"  .= finite
    , "value"   .= value
    ]

instance (ToJSON name, ToJSON a) => ToJSON (Named name a) where
  toJSON (Named name value) = object
    [ "name"    .= name
    , "value"   .= value
    ]

instance ToJSON a => ToJSON (Ranged a) where
  toJSON (Ranged range value) = object
    [ "range"   .= range
    , "value"   .= value
    ]

instance ToJSON ConOrigin where
  toJSON ConOSystem = String "ConOSystem"
  toJSON ConOCon    = String "ConOCon"
  toJSON ConORec    = String "ConORec"
  toJSON ConOSplit  = String "ConOSplit"

instance ToJSON ProjOrigin where
  toJSON ProjPrefix   = String "ProjPrefix"
  toJSON ProjPostfix  = String "ProjPostfix"
  toJSON ProjSystem   = String "ProjSystem"

instance ToJSON DataOrRecord where
  toJSON IsData   = String "IsData"
  toJSON IsRecord = String "IsRecord"

instance ToJSON IsInfix where
  toJSON InfixDef   = String "InfixDef"
  toJSON PrefixDef  = String "PrefixDef"

instance ToJSON Access where
  toJSON (PrivateAccess origin) = object
    [ "kind"      .= String "PrivateAccess"
    , "origin"    .= origin
    ]
  toJSON PublicAccess = object
    [ "kind"      .= String "PublicAccess"
    ]
  toJSON OnlyQualified = object
    [ "kind"      .= String "OnlyQualified"
    ]

instance ToJSON IsAbstract where
  toJSON AbstractDef  = String "AbstractDef"
  toJSON ConcreteDef  = String "ConcreteDef"

instance ToJSON IsInstance where
  toJSON InstanceDef    = String "InstanceDef"
  toJSON NotInstanceDef = String "NotInstanceDef"

instance ToJSON IsMacro where
  toJSON MacroDef    = String "MacroDef"
  toJSON NotMacroDef = String "NotMacroDef"

instance ToJSON NameId where
  toJSON (NameId name modul) = object
    [ "name"    .= name
    , "module"  .= modul
    ]

instance ToJSON MetaId where
  toJSON (MetaId i)   = toJSON i

instance ToJSON PositionInName where
  toJSON Beginning  = String "Beginning"
  toJSON Middle     = String "Middle"
  toJSON End        = String "End"

instance ToJSON e => ToJSON (MaybePlaceholder e) where
  toJSON (Placeholder pos) = object
    [ "kind"      .= String "Placeholder"
    , "position"  .= pos
    ]
  toJSON (NoPlaceholder pos value) = object
    [ "kind"      .= String "NoPlaceholder"
    , "position"  .= pos
    , "value"     .= value
    ]

instance ToJSON InteractionId where
  toJSON (InteractionId i) = toJSON i

instance (ToJSON a, ToJSON b) => ToJSON (ImportDirective' a b) where
  toJSON (ImportDirective range using hiding impRenaming publicOpen) = object
    [ "range"       .= range
    , "using"       .= using
    , "hiding"      .= hiding
    , "impRenaming" .= impRenaming
    , "publicOpen"  .= publicOpen
    ]

instance (ToJSON a, ToJSON b) => ToJSON (Using' a b) where
  toJSON UseEverything = Null
  toJSON (Using importedNames) = object
    [ "importedNames"  .= importedNames
    ]

instance (ToJSON a, ToJSON b) => ToJSON (ImportedName' a b) where
  toJSON (ImportedModule value) = object
    [ "kind"        .= String "ImportedModule"
    , "value"       .= value
    ]
  toJSON (ImportedName value) = object
    [ "kind"        .= String "ImportedName"
    , "value"       .= value
    ]

instance (ToJSON a, ToJSON b) => ToJSON (Renaming' a b) where
  toJSON (Renaming from to range) = object
    [ "range"       .= range
    , "from"        .= from
    , "to"          .= to
    ]

instance ToJSON m => ToJSON (TerminationCheck m) where
  toJSON TerminationCheck = object
    [ "kind"      .= String "TerminationCheck"
    ]
  toJSON NoTerminationCheck = object
    [ "kind"      .= String "NoTerminationCheck"
    ]
  toJSON NonTerminating = object
    [ "kind"      .= String "NonTerminating"
    ]
  toJSON Terminating = object
    [ "kind"      .= String "Terminating"
    ]
  toJSON (TerminationMeasure range value) = object
    [ "kind"      .= String "TerminationMeasure"
    , "range"       .= range
    , "value"       .= value
    ]
