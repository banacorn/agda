{-# LANGUAGE OverloadedStrings #-}

-- | Instances of EncodeTCM or ToJSON under Agda.Syntax.Concrete.Name

module Agda.Interaction.JSON.Syntax.Concrete.Name where

import Control.Monad ((>=>))
import Data.Aeson

import Agda.Interaction.JSON.Encode
import Agda.Interaction.JSON.Syntax.Common
import Agda.Interaction.JSON.Syntax.Position

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Concrete.Name
import qualified Agda.Syntax.Translation.AbstractToConcrete as A2C

--------------------------------------------------------------------------------
-- | Instances of EncodeTCM
instance EncodeTCM Name
instance EncodeTCM QName
instance EncodeTCM TopLevelModuleName

instance EncodeTCM A.QName where
  encodeTCM = A2C.abstractToConcrete_ >=> encodeTCM

instance EncodeTCM A.Name where
  encodeTCM = A2C.abstractToConcrete_ >=> encodeTCM

--------------------------------------------------------------------------------
-- | Instances of ToJSON

instance ToJSON NamePart where
  toJSON Hole      = Null
  toJSON (Id name) = toJSON name

instance ToJSON Name where
  toJSON (Name   range parts) = object
    [ "kind"  .= String "Name"
    , "range" .= range
    , "parts" .= parts
    ]
  toJSON (NoName range name) = object
    [ "kind"  .= String "NoName"
    , "range" .= range
    , "name"  .= name
    ]

instance ToJSON QName where
  toJSON = toJSON . toList
    where
      toList (QName name)      = name : []
      toList (Qual name qname) = name : toList qname

instance ToJSON TopLevelModuleName where
  toJSON (TopLevelModuleName range parts) = object
    [ "range" .= range
    , "parts" .= parts
    ]
