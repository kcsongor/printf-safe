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

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Printf.Safe
-- Copyright   :  (C) 2016 Csongor Kiss
-- License     :  GPL-3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- A type-safe interface for Text.Printf that ensures at compile-time
-- that the number and type of arguments passed to printf matches the
-- format specifier.
--
-- Thus, the specifier is lifted to the type-level and can be composed manually
-- using the (%) type-level function, or with the (%) term-level function.
--
-- Note that while the term-level composition retains type-safety, it
-- does not provide a way to define string literals within the format string,
-- those must be passed on as an argument to printf in that case.
--
-- Specifiers composed at the type-level can be bound to terms using the
-- Template' proxy (as they are not of kind *), and those terms can then be
-- further composed with each other.
-----------------------------------------------------------------------------
module Text.Printf.Safe
    (
    -- * Safe printf
      printf
    -- * Format template
    , Template
    , Template' (..)
    -- * Formatting options
    , FormatSpec (..)
    -- * Format template composition
    -- ** Type-level
    , type (%)
    -- ** Term-level
    , (%)
    , (<%>)
    -- * Term-level combinators
    , hex
    , oct
    , binary
    , sci
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

--------------------------------------------------------------------------

-- |Format specifiers that are not directly tied to a type.
-- For example, Hex (%x) belongs here, but Int (%d) doesn't.
-- These constructors are used ticked, promoting FormatSpec to its own kind.
data FormatSpec
    -- | %x (requires an Int)
    = Hex
    -- | %b (requires an Int)
    | Binary
    -- | %o (requires an Int)
    | Oct
    -- | %e (requires a Double)
    | Sci

-- |Fixed-kinded proxy for storing a list of Formats for printf
data Template' (f :: [Format]) = Template

-- |Type family that produces a proxy (Template') from any compatible
-- template candidate.
--
-- This way, we can have produce templates from single types, as in
--
-- > Template :: Template Int
--
-- which acts as a `%d' specifier, or
--
-- > Template :: Template ('Hex % Int)
--
-- which is a "%x %d" format string. (see composition below).
type family Template (a :: k) :: * where
    Template (a :: [Format])   = Template' a
    Template (a :: Format)     = Template' '[a]
    Template (a :: FormatSpec) = Template' '[ToFormat a]
    Template (a :: *)          = Template' '[ToFormat a]
    Template (a :: Symbol)     = Template' '[ToFormat a]

------ template composition -----

-- |Type-level list append
type family xs :++ ys where
    '[] :++ ys = ys
    (x ': xs) :++ ys = x ': (xs :++ ys)

-- |Composes two templates.
--
-- Given two terms representing templates, simply appends them to produce
-- a new term representing the composition.
--
-- @
-- format :: Template (Int % " in base " % Int % " is: ")
-- format = Template
--
-- hex :: Template 'Hex
-- hex = Template
--
-- printf (format % hex) 30 16 30
-- @
--
-- which yields the same result as calling 'Text.Printf.printf' as such:
--
-- > Text.Printf.printf "%d in base %d is: %x" 30 16 30
(%) :: Template' a -> Template' b -> Template (a :++ b)
(%) _ _ = Template

-- |Same as (%), but inserts a space in between the two formats.
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

-- |Acts like a `cons' function for `Format' lists, first turning the the new
-- head element into a Format. (since it can be a string literal (kind Symbol),
-- for example, or a Haskell type (kind *)).
--
-- Because the `Template' type family is poly-kinded, it supports both lists
-- and singular format specifiers. Composition produces lists.
--
-- @
-- format :: Template (Int % "in base 16 is: " % 'Hex)
-- format = Template
--
-- printf format 50 50
-- @
infixr 5 %
type family (e :: k) % (ls :: k') :: [Format] where
    e % (ls :: [Format])
      = (ToFormat e) ': ls
    e % ls
      = (ToFormat e) ': '[ToFormat ls]

-- |Construct the type of the final printf function
type family PrintfType e where
    PrintfType '[] = String
    PrintfType (('Lit s) ': fs) = PrintfType fs
    PrintfType (('TypeSpecifier s) ': fs) = s -> PrintfType fs
    PrintfType (('CustomSpecifier s) ': fs) = FormatType s -> PrintfType fs

-- |Convert from the Haskell type representation to the printf format notation.
--
-- We map the appropriate types to their corresponding specifiers, such as
--
-- String -> %s
-- Int    -> %d
-- etc ...
--
-- TODO: this is incomplete (or is it?)
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

-- |Essentially map PrintfFormat over the Template's [Format].
--
-- Unfortunately we don't have support for unsaturated type family applications,
-- so the recursion has to be written out explicitly :(
type family FormatList s where
    FormatList '[] = '[]
    FormatList (f ': fs) = PrintfFormat f ': FormatList fs

-- |Then fold the list into its final form, the format String.
class FoldToString a where
    foldToString :: proxy a -> String

instance FoldToString '[] where
    foldToString _ = ""

-- We want to replace all %s with %%s in the string literals. This is to
-- ensure that the user-specified literals don't interfere with our
-- formatting machinery.
instance (KnownSymbol x, FoldToString xs) => FoldToString ('ILit x ': xs) where
    foldToString _
      = curr ++ foldToString (Proxy :: Proxy xs)
          where curr = symbolVal (Proxy :: Proxy x) >>= \case '%' -> "%%"
                                                              c   -> return c

instance (KnownSymbol x, FoldToString xs) => FoldToString ('ISpecifier x ': xs) where
    foldToString _
      = symbolVal (Proxy :: Proxy x) ++ foldToString (Proxy :: Proxy xs)

formatString :: forall x list proxy. (FormatList x ~ list, FoldToString list) =>
    proxy x -> String
formatString _ = foldToString (Proxy :: Proxy list)

-- |The printf frontend.
--
-- `a` is of kind [Format]. It is first turned into a list of format specifiers
-- (the literals are kept in place), then the format string is constructed from
-- it.
--
-- Example:
--
-- > printf (Template :: Template ("hex: " % 'Hex % " binary: " % 'Binary)) 500 500
--
-- generates a format string "hex: %x binary: %b", and calls the internal
-- 'Text.Printf.printf' with the format string and the provided arguments.
printf :: (FoldToString (FormatList a), Pf.PrintfType (PrintfType a))
      => proxy a      -- ^ Witness the Format list. The proxy is usually a Template'.
      -> PrintfType a -- ^ The constructed type for printf
printf f = Pf.printf (formatString f)

-- basic combinators ----------
-- |Term-level representation of a 'Hex (hexadecimal) specifier for composition.
--
-- Example:
--
-- > printf hex 35
hex :: Template 'Hex
hex = Template

-- |Term-level representation of an 'Oct (octal) specifier for composition.
oct :: Template 'Oct
oct = Template

-- |Term-level representation of an 'Sci (scientific) specifier for composition.
sci :: Template 'Sci
sci = Template

-- |Term-level representation of a 'Binary specifier for composition.
binary :: Template 'Binary
binary = Template

-- |Term-level representation of a String specifier for composition.
--
-- This will not generate string literals in the format string (as that's only
-- possible at the type-level), but insert a `%s' specifier and modify the
-- type of printf accordingly, so it is useful when the string is not known
-- at compile time.
--
-- Example:
--
-- > printf string "example"
--
-- This means this is the same as calling
--
-- > Text.Printf.printf "%s" "example"
string :: Template String
string = Template
