{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
module Text.Printf.Safe
    (
      printf
    , Template
    , FormatSpec (..)
    , (%)
    , (<%>)
    , hex
    , binary
    , string
    ) where

import GHC.TypeLits
import Data.Proxy
import qualified Text.Printf as Pf

data Format
  = Lit Symbol
  | forall (k :: *). TypeSpecifier k
  | CustomSpecifier FormatSpec

data InternalFormat
  = ILit Symbol
  | ISpecifier Symbol

------ formatting options -----
data FormatSpec
  = Hex
  | Binary
  | Oct
  | Sci

-- fixed-kinded proxy for storing a list of Formats for printf
data Template' (f :: [Format]) = Template

-- NB: type families only seem to be polykinded when their return type
-- is also explicitly parametric?
--
-- We can have (Template ('Hex % Int)) or even (Template 'Hex).
type family Template (a :: k) :: k' where
    Template (a :: [Format])   = Template' a
    Template (a :: Format)     = Template' '[a]
    Template (a :: FormatSpec) = Template' '[ToFormat a]
    Template (a :: *)          = Template' '[ToFormat a]
    Template (a :: Symbol)     = Template' '[ToFormat a]

------ template composition -----

-- type-level list append
type family xs :++ ys where
    '[] :++ ys = ys
    (x ': xs) :++ ys = x ': (xs :++ ys)

-- composes two templates
(%) :: Template' a -> Template' b -> Template (a :++ b)
(%) _ _ = Template

-- composes two templates, and inserts a space
(<%>) :: Template' a -> Template' b -> Template (a :++ ('Lit " " ': b))
(<%>) _ _ = Template


-- Symbols (type-level strings) are the literals
-- Any * kind (hask type) can be used as a specifier
type family ToFormat (s :: k) :: Format where
    ToFormat (s :: Symbol)     = 'Lit s
    ToFormat (c :: *)          = 'TypeSpecifier c
    ToFormat (c :: FormatSpec) = 'CustomSpecifier c

type family FormatType a where
    FormatType 'Hex    = Int
    FormatType 'Binary = Int
    FormatType 'Oct    = Int
    FormatType 'Sci    = Double

------ format list -----
infixr 5 %
type family (e :: k) % (ls :: k') :: [Format] where
    e % (ls :: [Format])
      = (ToFormat e) ': ls

    e % ls
      = (ToFormat e) ': '[ToFormat ls]

type family PrintfType e where
    PrintfType '[] = String
    PrintfType (('Lit s) ': fs) = PrintfType fs
    PrintfType (('TypeSpecifier s) ': fs) = s -> PrintfType fs
    PrintfType (('CustomSpecifier s) ': fs) = FormatType s -> PrintfType fs

-- TODO: this is incomplete
-- 'Lit "asd" ==> "asd"
type family PrintfFormat e where
    PrintfFormat ('Lit s)
      = 'ILit s
    PrintfFormat ('TypeSpecifier String)
      = 'ISpecifier "%s"
    PrintfFormat ('TypeSpecifier Int)
      = 'ISpecifier "%d"
    PrintfFormat ('TypeSpecifier Double)
      = 'ISpecifier "%f"
    PrintfFormat ('TypeSpecifier Char)
      = 'ISpecifier "%c"
    PrintfFormat ('CustomSpecifier 'Hex)
      = 'ISpecifier "%x"
    PrintfFormat ('CustomSpecifier 'Oct)
      = 'ISpecifier "%o"
    PrintfFormat ('CustomSpecifier 'Binary)
      = 'ISpecifier "%b"
    PrintfFormat ('CustomSpecifier 'Sci)
      = 'ISpecifier "%e"

-- '["a", "b", "c"] ==> (term) "abc"
class FoldToString a where
    foldToString :: proxy a -> String

instance FoldToString '[] where
    foldToString _ = ""

instance (KnownSymbol x, FoldToString xs) => FoldToString ('ILit x ': xs) where
    foldToString _
      = curr ++ foldToString (Proxy :: Proxy xs)
          where curr = symbolVal (Proxy :: Proxy x) >>= \case '%' -> "%%"
                                                              c   -> return c

instance (KnownSymbol x, FoldToString xs) => FoldToString ('ISpecifier x ': xs) where
    foldToString _
      = symbolVal (Proxy :: Proxy x) ++ foldToString (Proxy :: Proxy xs)

-- '[ 'Lit "asd", 'TypeSpecifier String] ==> '[ "asd", "%s"]
type family FormatList s where
    FormatList '[] = '[]
    FormatList (f ': fs) = PrintfFormat f ': FormatList fs

formatString :: forall x list proxy. (FormatList x ~ list, FoldToString list) =>
    proxy x -> String
formatString _ = foldToString (Proxy :: Proxy list)

------ printf -----------------
printf :: forall (a :: [Format]) proxy.
    (FoldToString (FormatList a), Pf.PrintfType (PrintfType a)) =>
    proxy a -> PrintfType a
printf f = Pf.printf (formatString f)

-- basic combinators ----------
hex :: Template 'Hex
hex = Template

binary :: Template 'Binary
binary = Template

string :: Template String
string = Template

------ EXAMPLES ---------------
--greeter :: Show a => Template ("Hello, " % String % "! Today is " % a)
--greeter = Template
--
--four :: Show a => Template (a % ", " % a % ", " % a % ", " % a)
--four = Template
--
--one :: Show a => Template a
--one = Template
--
---- pass arbitrary format specifier
--base :: template a -> Template ("Your number: " % Int % ", formatted : " % a)
--base _ = Template
