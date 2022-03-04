{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DocUtils.Doc
Description : 'Doc' helpers using prettyprinter
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module DocUtils.Doc (
  doc1',
  docI,
  docNI,
  doc2,
  doc2String,
  doc2PShow,
  trim,
  renderLong,
  renderCompact,
  hasParens,
  wrapParens,
  wrapSQuote,
  wrapDQuote,
  wrapBrackets,
  wrapBraces,
  addParens,
  stripParens,
  wrap,
  numbered,
  packInt,
  packLen,
  pSepEmpty,
  pJoinEmpty,
  pFilterEmpty,
  docTheseImpl,
  docPref,
  docSuff,
  docBoth,
  maybeTheseImpl,
  maybePref,
  maybeSuff,
  maybeBoth,
  unlessNull,
  unlessNullDef,
  pCompact,
  psiCT,
  psiT,
  psiC,
  psi,
  psiCString,
  psiString,
  ppsiCS,
  ppsiS,
  psiCS,
  psiS,
  ppOptsCompact,
  ppOptsNormal,
) where

import Control.Monad
import Data.Char
import Data.Function
import qualified Data.List as L
import qualified Data.List.Extra as LE
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.These
import Prettyprinter
import qualified Prettyprinter.Internal as PI
import Prettyprinter.Render.Text
import Text.Pretty.Simple

-- | pretty print nested documents
doc1' :: Bool -> Doc ann -> [Doc ann] -> Doc ann
doc1' inline hdr =
  \case
    [] -> hdr
    [x] ->
      if inline
        then hdr <+> pipe <+> x
        else hdr <+> "#" <> hardline <> indent 2 x
    xs -> hdr <+> "~" <> hardline <> indent 2 (vcat xs)

-- | pretty print nested documents inline
{-# INLINE docI #-}
docI :: Doc ann -> [Doc ann] -> Doc ann
docI = doc1' True

-- | pretty print nested documents not inline
{-# INLINE docNI #-}
docNI :: Doc ann -> [Doc ann] -> Doc ann
docNI = doc1' False

-- | pretty print key and a number
{-# INLINE doc2 #-}
doc2 :: Doc ann -> Int -> Doc ann
doc2 key val = key <> equals <> pretty val

-- | pretty print key and a string
doc2String :: Doc ann -> String -> Doc ann
doc2String key val = key <> equals <> pretty val

-- | pretty print key and a showable value
doc2PShow :: Show b => Doc ann -> b -> Doc ann
doc2PShow key val = key <> equals <> pretty (psi val)

ppOptsCompact, ppOptsNormal :: OutputOptions

-- | pretty print using compact format
ppOptsCompact = defaultOutputOptionsNoColor{outputOptionsCompact = True}

-- | pretty print using default format
ppOptsNormal = defaultOutputOptionsNoColor

-- | pretty print value to a strict text
psiCT, psiT :: Show a => a -> T.Text
psiCT = TL.toStrict . psiC
psiT = TL.toStrict . psi
{-# INLINEABLE psiCT #-}
{-# INLINEABLE psiT #-}

-- | pretty print value to a lazy text
psiC, psi :: Show a => a -> TL.Text
psiC = pShowOpt ppOptsCompact
psi = pShowOpt ppOptsNormal
{-# INLINEABLE psiC #-}
{-# INLINEABLE psi #-}

-- | pretty print value to a lazy text
psiCString, psiString :: String -> TL.Text
psiCString = pStringOpt ppOptsCompact
psiString = pStringOpt ppOptsNormal
{-# INLINEABLE psiCString #-}
{-# INLINEABLE psiString #-}

-- | pretty print value to IO
ppsiCS, ppsiS :: Show a => a -> IO ()
ppsiCS = putStrLn . psiCS
ppsiS = putStrLn . psiS

-- | pretty print value to a string
psiCS, psiS :: Show a => a -> String
psiCS = TL.unpack . psiC
psiS = TL.unpack . psi
{-# INLINEABLE psiCS #-}
{-# INLINEABLE psiS #-}

-- | trim a string
trim :: String -> String
trim = dropWhile isSpace . L.dropWhileEnd isSpace

-- | render using normal format
renderLong :: Doc ann -> T.Text
renderLong doc = renderStrict (layoutPretty (LayoutOptions Unbounded) doc)

-- | render using compact format
renderCompact :: Doc ann -> T.Text
renderCompact = renderStrict . layoutCompact

-- | is the first character an open parenthesis
hasParens :: String -> Bool
hasParens = (== "(") . take 1

-- | wrap a string with parentheses
wrapParens :: (IsString s, Semigroup s) => s -> s
wrapParens = wrap ("(", ")")

-- | wrap a string with single quotes
wrapSQuote :: (IsString s, Semigroup s) => s -> s
wrapSQuote = wrap ("'", "'")

-- | wrap a string with double quotes
wrapDQuote :: (IsString s, Semigroup s) => s -> s
wrapDQuote = wrap ("\"", "\"")

-- | wrap a string with brackets
wrapBrackets :: (IsString s, Semigroup s) => s -> s
wrapBrackets = wrap ("[", "]")

-- | wrap a string with braces
wrapBraces :: (IsString s, Semigroup s) => s -> s
wrapBraces = wrap ("{", "}")

-- | wraps with parentheses if not already present
addParens :: String -> String
addParens s
  | hasParens s = s
  | otherwise = wrapParens s

-- | strip parens if wrapped (trims the string first)
stripParens :: String -> String
stripParens s' =
  let s = LE.trim s'
   in fromMaybe s ((L.stripPrefix "(" >=> LE.stripSuffix ")") s)

-- | wrap a value with start and end delimiters
wrap :: Semigroup s => (s, s) -> s -> s
wrap (start, end) = (start <>) . (<> end)

-- | add a title and add numbers to a list of documents
numbered :: T.Text -> [Doc ann] -> Doc ann
numbered title =
  \case
    [] -> mempty
    ds@(_ : _) ->
      pretty title
        <+> "length"
        <> equals
        <> pretty (length ds)
        <> hardline
        <> vcat (zipWith (\i s -> fill 4 (pretty i <> dot) <+> s) [1 :: Int ..] ds)

-- | convert an 'Int' to 'Text'
packInt :: Int -> T.Text
packInt = T.pack . show

-- | convert the length of a container to 'Text'
packLen :: Foldable t => t a -> T.Text
packLen = T.pack . show . length

-- | separate documents using "sp" after filtering out any empty documents
pSepEmpty :: Doc ann -> [Doc ann] -> Doc ann
pSepEmpty sp = concatWith (surround sp) . pFilterEmpty

-- | filter out empty documents and then join together
pJoinEmpty :: [Doc ann] -> Doc ann
pJoinEmpty = hsep . pFilterEmpty

-- | filter out empty documents
pFilterEmpty :: [Doc ann] -> [Doc ann]
pFilterEmpty = filter (\case PI.Empty -> False; _o -> True)

-- | add a space as prefix or suffix or both to a document
docTheseImpl :: These () () -> Maybe (Doc ann) -> Doc ann
docTheseImpl th =
  \case
    Nothing -> mempty
    Just d ->
      d & case th of
        This () -> (" " <>)
        That () -> (<> " ")
        These () () -> (" " <>) . (<> " ")

-- | prefix optional document with a space
docPref :: Maybe (Doc ann) -> Doc ann
docPref = docTheseImpl (This ())

-- | suffix optional document with a space
docSuff :: Maybe (Doc ann) -> Doc ann
docSuff = docTheseImpl (That ())

-- | prefix and suffix optional document with a space
docBoth :: Maybe (Doc ann) -> Doc ann
docBoth = docTheseImpl (These () ())

-- | add a space as prefix or suffix or both to some text
maybeTheseImpl :: These () () -> (a -> Text) -> Maybe a -> Text
maybeTheseImpl th f =
  \case
    Nothing -> mempty
    Just a ->
      f a & case th of
        This () -> (" " <>)
        That () -> (<> " ")
        These () () -> (" " <>) . (<> " ")

-- | prefix optional resulting text with a space
maybePref :: (a -> Text) -> Maybe a -> Text
maybePref = maybeTheseImpl (This ())

-- | suffix optional resulting text with a space
maybeSuff :: (a -> Text) -> Maybe a -> Text
maybeSuff = maybeTheseImpl (That ())

-- | prefix and suffix optional resulting text with a space
maybeBoth :: (a -> Text) -> Maybe a -> Text
maybeBoth = maybeTheseImpl (These () ())

-- | only run "f" if there is data
unlessNull :: Monoid m => [a] -> ([a] -> m) -> m
unlessNull xs f =
  case xs of
    [] -> mempty
    ys@(_ : _) -> f ys

-- | only run "f" if there is data otherwise use the default
unlessNullDef :: [a] -> m -> ([a] -> m) -> m
unlessNullDef xs m f =
  case xs of
    [] -> m
    ys@(_ : _) -> f ys

-- | pretty print using compact format
pCompact :: Show a => a -> IO ()
pCompact =
  pPrintOpt
    CheckColorTty
    defaultOutputOptionsDarkBg
      { outputOptionsCompactParens = True
      , outputOptionsCompact = True
      , outputOptionsIndentAmount = 2
      , outputOptionsPageWidth = 120
      }
