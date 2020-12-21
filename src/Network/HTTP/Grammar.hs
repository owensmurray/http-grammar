{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn=missing-import-lists #-}

{- |
  This module provides Attoparsec-based parsers for the HTTP grammar
  rules as defined by RFC-2616.
-}
module Network.HTTP.Grammar (
  -- * Parsable data types.
  UserAgent(..),
  Product(..),

  -- * Productions
  userAgent,
  product,
  comment,
  ctext,
  quotedPair,
  text,
  char,
  ctl,
  token,
  separators,
  lws,
  crlf,
) where

import Prelude hiding (product)

import Control.Applicative (many, (<|>))
import Control.Monad (void)
import Data.Attoparsec.ByteString (Parser, word8, takeWhile1, satisfy,
  inClass, option, string, many1)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Data.ByteString as BS


{- |
  The User-Agent header field is defined as a list of tokens, each
  of which is either a product or a comment. Values of this data type
  represents one such token.
-}
data UserAgent
  = UAProduct Product
  | UAComment ByteString


{- |
  A representation of an HTTP User-Agent product.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.8
-}
data Product = Product {
       productName :: ByteString,
    productVersion :: Maybe ByteString
  }


{- |
  Parser for the User-Agent header, defined:
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.43
-}
userAgent :: Parser [UserAgent]
userAgent = many1 (
    (UAProduct <$> product)
    <|> (UAComment <$> comment)
  )


{- |
  product - http product token.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.8
-}
product :: Parser Product
product =
    Product
      <$> token
      <*> option Nothing versionParser 
  where
    versionParser = do
      void $ string "/"
      Just <$> token


{- |
  comment - basic http grammar rule.
  The result is the parsed comment content, rather than the raw source.

  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
comment :: Parser ByteString
comment = do
  void $ word8 40 -- "("
  segments <- many (
      takeWhile1 ctext
      <|> (BS.singleton <$> quotedPair)
      <|> comment
    )
  void $ word8 41 -- ")"
  return (mconcat segments)


{- |
  ctext - basic http grammar rule.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
ctext :: Word8 -> Bool
ctext x = text x && x /= 40 && x /= 41 -- "(", ")"


{- |
  quotedPair - basic http grammar rule.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
quotedPair :: Parser Word8
quotedPair = do
  void $ word8 92 -- <\>
  satisfy char


{- |
  TEXT - basic http grammar rule.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
text :: Word8 -> Bool
text = not . ctl


{- |
  CHAR - basic http grammar rule.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
char :: Word8 -> Bool
char x = x >= 0 && x <= 127


{- |
  CTL - basic http grammar rule.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
ctl :: Word8 -> Bool
ctl x = (x >= 0 && x <= 31) || x == 127


{- |
  token - basic http grammar rule.

  The grammar specifies that adjacent LWS should be consumed without affecting
  the meaning of the token. This parser returns the token stripped of any
  adjacent lws.

  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
token :: Parser ByteString
token = do
  option () lws
  t <- takeWhile1 (\x -> char x && not (separators x) && not (ctl x))
  option () lws
  return t

 
{- |
  separators - basic http grammar rule.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
separators :: Word8 -> Bool
separators = inClass "()<>@,;:\\\"/[]?={} \t"


{- |
  LWS - basic http grammar rule. (L)inear (W)hite(S)pace.

  Consumes all linear whitespace.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
lws :: Parser ()
lws = do
  option () crlf
  void $ takeWhile1 (\x -> x == 32 || x == 9)


{- |
  crlf - basic http grammar rule.

  Consumes one crlf.
  https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
-}
crlf :: Parser ()
crlf = void $ string "\r\n"


