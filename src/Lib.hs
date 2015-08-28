{-# LANGUAGE TypeFamilies,TypeOperators #-}
{-# LANGUAGE UndecidableInstances,MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

data Yep
data Nope

data Fix e = In (e (Fix e))
data (f ⊕ g) e = Inl (f e) | Inr (g e)

(▽)::(f e -> a) -> (g e -> a) -> (f ⊕ g) e -> a
(f ▽ g) (Inl x) = {-# SCC triangler  #-} f x
(f ▽ g) (Inr x) = {-# SCC trianglel #-} g x

type family IsIn f g where
    IsIn f f     = Yep
    IsIn f (g ⊕ h) = Or (IsIn f g) (IsIn f h)
    IsIn f g     = Yep

type family Or b c where
    Or Nope Nope = Nope
    Or b c = Yep

--
data Refl
data L x
data R x

type family Into f g where
    Into f f       = Refl
    Into f (g ⊕ h) = Ifi (Into f g) (IsIn f h)
                         (Into f h) (IsIn f g)
    Into f g       = Nope

type family Ifi lp inr rp inl where
    Ifi Nope inr Nope inl = Nope
    Ifi Nope inr rp Nope  = R rp
    Ifi lp Nope rp inl    = L lp
    Ifi lp inr rp inl     = Nope

--

class Inj f g p where
    injp :: p -> f e -> g e

instance Inj f f Refl where
    injp _ = id

instance Inj f g p => Inj f (g ⊕ h) (L p) where
    injp (_ :: L p) = Inl . injp (undefined :: p)

instance Inj f h p => Inj f (g ⊕ h) (R p) where
    injp (_ :: R p) = Inr . injp (undefined :: p)

inj ::forall f g e . (Inj f g (Into f g)) => f e -> g e
inj = {-# SCC inj #-} injp (undefined :: Into f g)

--
data Onl (h :: * -> *)
data Onr (h :: * -> *)
data Le  (g :: * -> *) p
data Ri  (f :: * -> *) p
data Found

type family Minus f g where
    Minus f f       = Nope
    Minus (f ⊕ g) f = Onl g
    Minus (f ⊕ g) g = Onr f
    Minus (f ⊕ g) h = Ifm g (Minus f h) (IsIn f g) f (Minus g h) (IsIn f h)
    Minus f g       = Found

type family Ifm g lp inr f rp inl where
    Ifm g Nope inr  f Nope inl  = Nope
    Ifm g Nope inr  f rp   Nope = Onr f --rp
    Ifm g lp   Nope f rp   inl  = Onl g --lp

type family OutOf p where
    OutOf (Onl x)  = x
    OutOf (Onr x)  = x
    OutOf (Le f p) = OutOf p ⊕ f
    OutOf (Ri f p) = f ⊕ OutOf p

--
class Without f g p where
    (??) :: (g e -> r) -> (OutOf p e -> r) -> p -> f e  -> r

instance Without (f ⊕ g) f (Onl g) where
    (m ?? n) _ = {-# SCC without_onl #-}  m ▽ n

instance Without (f ⊕ g) g (Onr f) where
    (m ?? n) _ = {-# SCC without_onr #-}  n ▽ m

instance Without f h p => Without (f ⊕ g) h (Le g p) where
    (m ?? n) (_ :: Le g p) = (m ?? (n . Inl)) (undefined :: p) ▽ (n . Inr)

instance Without g h p => Without (f ⊕ g) h (Ri f p) where
    (m ?? n) (_ :: Ri f p) = (n . Inl) ▽ (m ?? (n . Inr)) (undefined :: p)

(?) :: forall f g e r . Without f g (Minus f g) => (g e -> r) -> (OutOf (Minus f g) e -> r) -> f e -> r
m ? n = {-# SCC branch #-}  (m ?? n) (undefined :: Minus f g)

cases cs = f where f (In e) = cs e f
