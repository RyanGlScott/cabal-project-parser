{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Project.Newtypes where

import Control.Applicative
import Data.Char

import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

-------------------------------------------------------------------------------
-- PackageLocation
-------------------------------------------------------------------------------

newtype PackageLocation = PackageLocation String
  deriving anyclass (C.Newtype String)

-- | This is a bit tricky since it has to cover globs which have embedded @,@
-- chars. But we don't just want to parse strictly as a glob since we want to
-- allow http urls which don't parse as globs, and possibly some
-- system-dependent file paths. So we parse fairly liberally as a token, but
-- we allow @,@ inside matched @{}@ braces.
instance C.Parsec PackageLocation where
    parsec = PackageLocation <$> outerTerm
      where
        outerTerm = ($ "") <$> outerChars

        outerChars, outerChar, innerChars, innerChar :: C.CabalParsing m => m ShowS
        outerChars = foldr (.) id <$> C.some outerChar
        innerChars = foldr (.) id <$> C.many innerChar

        outerChar = do
            c <- C.satisfy $ \c -> not (isSpace c || c == '}' || c == ',')
            kont c

        innerChar = do
            c <- C.satisfy $ \c -> not (isSpace c || c == '}')
            kont c

        kont :: C.CabalParsing m => Char -> m ShowS
        kont c = case c of
           '{' -> do
               cs <- innerChars
               c' <- C.char '}'
               return (showChar c . cs . showChar c')
           _   -> return $ showChar c


instance C.Pretty PackageLocation where
    pretty (PackageLocation p) = PP.text p

-------------------------------------------------------------------------------
-- NoCommas: something which can be comma separated
-------------------------------------------------------------------------------

newtype NoCommas = NoCommas String
  deriving anyclass (C.Newtype String)

instance C.Parsec NoCommas where
    parsec = NoCommas <$> liftA2 (:) (C.satisfy (not . isSpace)) (C.munch (/= ','))

instance C.Pretty NoCommas where
    pretty (NoCommas p) = PP.text p

-------------------------------------------------------------------------------
-- Newtype
-------------------------------------------------------------------------------

newtype Int' = Int' Int
  deriving anyclass (C.Newtype Int)

instance C.Parsec Int' where
    parsec = Int' <$> C.integral

instance C.Pretty Int' where
    pretty (Int' i) = PP.int i
