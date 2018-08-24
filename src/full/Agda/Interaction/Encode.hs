{-# LANGUAGE OverloadedStrings #-}

-- | Encode things into JSON values in TCM

module Agda.Interaction.Encode (EncodeTCM(..)) where

import Data.Aeson
import Agda.TypeChecking.Monad

---------------------------------------------------------------------------
-- * The EncodeTCM class

-- | The JSON version of`PrettyTCM`, for encoding JSON value in TCM
class EncodeTCM a where
  encodeTCM :: a -> TCM Value
