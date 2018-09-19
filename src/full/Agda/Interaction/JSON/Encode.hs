{-# LANGUAGE OverloadedStrings #-}

-- | Encoding stuff into JSON values in TCM

module Agda.Interaction.JSON.Encode where

import Control.Monad ((>=>), sequence, liftM2)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Data.Vector (fromList)

import qualified Agda.Syntax.Translation.AbstractToConcrete as A2C
import qualified Agda.Syntax.Translation.InternalToAbstract as I2A

import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Internal as I
import           Agda.TypeChecking.Monad
import           Agda.TypeChecking.Pretty (PrettyTCM(..))
import           Agda.Utils.Pretty
import qualified Agda.Utils.Maybe.Strict as Strict
import qualified Agda.Utils.FileName as File


---------------------------------------------------------------------------
-- * The EncodeTCM class

-- | The JSON version of`PrettyTCM`, for encoding JSON value in TCM
class EncodeTCM a where
  encodeTCM :: a -> TCM Value
  default encodeTCM :: ToJSON a => a -> TCM Value
  encodeTCM = return . toJSON


-- | TCM monadic version of object
obj :: [TCM Pair] -> TCM Value
obj pairs = object <$> sequence pairs

-- | TCM monadic version of (.=)
(%=) :: (ToJSON a) => Text -> a -> TCM Pair
(%=) key value = return (key .= toJSON value)

-- | Pairs a key with a value wrapped in TCM
(#=) :: (ToJSON a) => Text -> TCM a -> TCM Pair
(#=) key boxed = do
  value <- boxed
  return (key .= toJSON value)

-- | Abbreviation of `_ #= encodeTCM _`
(@=) :: (EncodeTCM a) => Text -> a -> TCM Pair
(@=) key value = do
  encoded <- encodeTCM value
  return (key .= encoded)

-- | A handy alternative of `obj` with kind specified
kind :: Text -> [TCM Pair] -> TCM Value
kind k pairs = obj (("kind" @= String k) : pairs)

---------------------------------------------------------------------------
-- | Instances of EncodeTCM

instance EncodeTCM Value where
instance EncodeTCM Bool where
instance EncodeTCM Doc

instance EncodeTCM a => EncodeTCM [a] where
  encodeTCM = mapM encodeTCM >=> return . Array . fromList
instance {-# OVERLAPPING #-} EncodeTCM String
instance EncodeTCM Int

instance EncodeTCM a => EncodeTCM (Maybe a) where
  encodeTCM Nothing   = return Null
  encodeTCM (Just a)  = encodeTCM a

instance (EncodeTCM a, EncodeTCM b) => EncodeTCM (a, b) where
  encodeTCM (a, b) = do
    a' <- encodeTCM a
    b' <- encodeTCM b
    encodeTCM (a', b')

--------------------------------------------------------------------------------
-- | Instances of ToJSON

instance ToJSON Doc where
  toJSON = toJSON . render

instance EncodeTCM File.AbsolutePath where
instance ToJSON File.AbsolutePath where
  toJSON (File.AbsolutePath path) = toJSON path

instance ToJSON a => ToJSON (Strict.Maybe a) where
  toJSON (Strict.Just a) = toJSON a
  toJSON Strict.Nothing = Null
