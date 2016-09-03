{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, TypeInType #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module IsList where

import Data.Proxy
import GHC.TypeLits
import GHC.Types (Type)

type family Extract (k :: Type) :: Type where
  Extract Symbol = String
  Extract [a]    = [Extract a]

class IsList (a :: [k]) where
  symbols :: Proxy a -> [Extract k]

instance IsList '[] where
  symbols _ = []

instance (Extractable x, IsList xs) => IsList (x : xs) where
  symbols _ = extract (Proxy :: Proxy x) : symbols (Proxy :: Proxy xs)

class Extractable (a :: k) where
  extract :: Proxy (a :: k) -> Extract k

instance KnownSymbol a => Extractable (a :: Symbol) where
  extract p = symbolVal p

instance IsList xs => Extractable (xs :: [a]) where
  extract p = symbols p

-- symbols2 :: Proxy (a :: [[Symbol]]) -> [[String]]
