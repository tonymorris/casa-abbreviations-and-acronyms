{-# LANGUAGE FlexibleContexts #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms where

import Text.Parsec

parseNoTab ::
  Stream s m Char =>
  ParsecT s u m String
parseNoTab =
  many1 (noneOf "\t")

data Source =
  AIP
  | AIS
  | ATA
  | ATSB
  | CAR1988
  | CAR1998
  | CASA
  | CASR1998
  | CASRPart String
  | FAR
  | ICAO
  | JAA
  | OBPR
  | ORR
  | WATOG
  | OtherSource String
  deriving (Eq, Ord, Show)

parseSource ::
  Stream s m Char =>
  ParsecT s u m Source
parseSource =
  try (AIP <$ string "AIP") <|>
  try (AIS <$ string "AIS") <|>
  try (ATA <$ string "ATA") <|>
  try (ATSB <$ string "ATSB") <|>
  try (CAR1988 <$ string "CAR 1988") <|>
  try (CAR1998 <$ string "CAR 1998") <|>
  try (CASA <$ string "CASA") <|>
  try (CASR1998 <$ string "CASR 1998") <|>
  try (CASRPart <$> (string "CASR Part " *> many1 (noneOf "\n/"))) <|>
  try (FAR <$ string "FAR") <|>
  try (ICAO <$ string "ICAO") <|>
  try (JAA <$ string "JAA") <|>
  try (OBPR <$ string "OBPR") <|>
  try (ORR <$ string "ORR") <|>
  try (WATOG <$ string "WATOG") <|>
  OtherSource <$> many1 (noneOf "\n/")

newtype Sources =
  Sources
    [Source]
  deriving (Eq, Ord, Show)

parseSources ::
  Stream s m Char =>
  ParsecT s u m Sources
parseSources =
  Sources <$> sepBy parseSource (char '/')

data Acronym =
  Acronym
    String
    String -- Meaning
    Sources
  deriving (Eq, Ord, Show)  

parseAcronym ::
  Stream s m Char =>
  ParsecT s u m Acronym
parseAcronym =
  do  a <- parseNoTab
      _ <- char '\t'
      m <- parseNoTab
      _ <- char '\t'
      s <- parseSources
      pure (Acronym a m s)

newtype Acronyms =
  Acronyms
    [Acronym]
  deriving (Eq, Ord, Show)

parseAcronyms ::
  Stream s m Char =>
  ParsecT s u m Acronyms
parseAcronyms =  
  Acronyms <$> sepBy parseAcronym (char '\n')
