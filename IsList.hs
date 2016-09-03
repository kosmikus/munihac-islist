{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, TypeInType #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module IsList where

import Data.Proxy
import GHC.TypeLits hiding (Nat)
import GHC.Types (Type)

type family Extract (k :: Type) :: Type where
  Extract Symbol = String
  Extract [a]    = [Extract a]

class Extractable (a :: k) where
  extract :: Proxy (a :: k) -> Extract k

instance KnownSymbol a => Extractable (a :: Symbol) where
  extract p = symbolVal p

instance Extractable ('[] :: [a]) where
  extract _ = []

instance (Extractable x, Extractable xs) => Extractable (x ': xs) where
  extract _ = extract (Proxy :: Proxy x) : extract (Proxy :: Proxy xs)

-- symbols2 :: Proxy (a :: [[Symbol]]) -> [[String]]


data Nat = Z | S Nat

data Vec (n :: Nat) (a :: Type) where
  Nil  :: Vec Z a
  (:*) :: a -> Vec n a -> Vec (S n) a

infixr 5 :*

deriving instance Show a => Show (Vec n a)
  
vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _f Nil       = Nil
vmap  f (x :* xs) = f x :* vmap f xs

class Repeatable (n :: Nat) where
  vrepeat :: a -> Vec n a

instance Repeatable Z where
  vrepeat _ = Nil

instance Repeatable n => Repeatable (S n) where
  vrepeat x = x :* vrepeat x

{-
data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data SBool (b :: Bool) where
  SFalse :: SBool False
  STrue  :: SBool True


class SBoolI (b :: Bool) where
  sBool :: SBool b

instance SBoolI False where
  sBool = SFalse

instance SBoolI True where
  sBool = STrue
-}

class SingI (a :: k) where
  sing :: Sing a

instance SingI Z where
  sing = SZ

instance (SingI n) => SingI (S n) where
  sing = SS sing

data family Sing (a :: k)

data instance Sing (n :: Nat) where
  SZ :: Sing Z
  SS :: Sing n -> Sing (S n)

data instance Sing (b :: Bool) where
  SFalse :: Sing False
  STrue  :: Sing True


vrepeat' :: Sing n -> a -> Vec n a
vrepeat' SZ     _ = Nil
vrepeat' (SS n) x = x :* vrepeat' n x

vrepeat'' :: SingI n => a -> Vec n a
vrepeat'' = vrepeat' sing
