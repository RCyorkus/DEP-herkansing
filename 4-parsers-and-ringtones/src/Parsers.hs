module Parsers where

import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Control.Applicative
import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Data.Maybe (isJust, fromMaybe)
import Data.Char (toUpper)
import Control.Monad(mzero, mplus)
import Data.List (uncons)
import Types
import Instruments

type Parser = (StateT String Maybe)
type CharSet = [Char]

pCharSet :: CharSet -> Parser Char
pCharSet cs = do input <- uncons <$> get
                 case input of
                   Just (h, t) -> if h `elem` cs then put t >> return h else mzero
                   Nothing -> mzero

-- TODO: Schrijf en documenteer de Parser `pComplementCharSet` die een lijst van karakters meekrijgt,
-- en het eerste karakter parset wanneer dit niet in de meegegeven set karakters zit.
pComplementCharSet :: CharSet -> Parser Char
-- input -> get input sting en uncons splitst de string in het eerste karakter (h) en de rest (t)
-- `notElem` kijkt of h niet in de list cs zit
pComplementCharSet cs = do input <- uncons <$> get
                           case input of
                             Just (h, t) -> if h `notElem` cs then put t >> return h else mzero
                             Nothing -> mzero

-- TODO: Schrijf en documenteer de Parser `pString` die een gegeven String probeert te parsen. 
-- TIPS: Parse een enkele letter met `pCharSet` en parse de rest recursief met `pString`; 
--       combineer beide waarden weer voordat je deze returnt. 
--       Vergeet niet een geval voor een lege String te schrijven.
pString :: String -> Parser String
pString [] = return []
pString (x:xs) = do
    h <- pCharSet [x] -- Parse een enkele letter met `pCharSet`
    t <- pString xs -- de rest recursief met `pString`
    return (h:t) -- combineer beide waarden weer voordat je deze returnt.

pOptional :: Parser a -> Parser (Maybe a)
pOptional p = Just <$> p <|> return Nothing 

pRepeatSepBy :: Parser a -> Parser b -> Parser [b]
pRepeatSepBy sep p = (:) <$> p <*> mplus (sep *> pRepeatSepBy sep p) (return [])

pEmpty :: Parser ()
pEmpty = return ()

-- TODO: Schrijf en documenteer de Parser `pRepeat` die een enkele Parser herhaaldelijk toepast.
-- TIPS: Maak gebruik van `pRepeatSepBy` en `pEmpty`.


-- pRepeatSepBy verwacht een scheidingsparser en een parser voor wat je wilt vinden
-- bij 1,2,3 gebruik je bvb een parser voor "," en een parser voor getallen
-- maar bij 123 gebruiken we pEmpty. Dit zorgt ervoor dat er geen scheidingsteken nodig is maar dat de gegeven parser herhaald wordt totdat deze faalt
pRepeat :: Parser a -> Parser [a]
pRepeat p = pRepeatSepBy pEmpty p

-- TODO: Schrijf en documenteer de Parser `pNumber`, die een geheel getal parset.
-- TIPS: Combineer de voorgaande Parsers en de Prelude-functie read. 

-- pCharSet kijkt of het eerste karakter een cijfer van 0-9 is 
-- pRepeat herhaalt dit en zet alle gevonden getallen in een string
-- Vervolgens zet read de geparste String om naar een geheel getal (Int)
pNumber :: Parser Int
pNumber = read <$> pRepeat (pCharSet ['0'..'9'])

pTone :: Parser Tone
pTone = do tone <- tRead . toUpper <$> pCharSet "abcdefg"
           sharp <- pOptional (pCharSet "#")
           if isJust sharp && tone `elem` [C,D,F,G,A]
             then return (succ tone)
             else return tone
  where tRead 'C' = C
        tRead 'D' = D
        tRead 'E' = E
        tRead 'F' = F
        tRead 'G' = G
        tRead 'A' = A
        tRead 'B' = B
        tRead _   = error "Invalid note"

-- TODO: Schrijf en documenteer de Parser` `pOctave`, die een getal parset naar de bijbehorende octaaf.
-- TIPS: Kijk in Types.hs naar het type Octave voor je begint te schrijven.

-- pNumber zoekt een nummer en geeft Parser Int terug
-- <$> Parser Int om in noramle Int zo dat toEnum deze kan gebruiken
pOctave :: Parser Octave
pOctave = toEnum <$> pNumber

pDuration :: Parser Duration
pDuration = do number <- pNumber
               case number of
                 1 -> return Full
                 2 -> return Half
                 4 -> return Quarter
                 8 -> return Eighth
                 16 -> return Sixteenth
                 32 -> return Thirtysecond
                 _ -> mzero

pPause :: Duration -> Parser Note
pPause d = do duration <- fromMaybe d <$> pOptional pDuration
              _ <- pCharSet "pP"
              return $ Pause duration

pNote :: Duration -> Octave -> Parser Note
pNote d o = do duration <- fromMaybe d <$> pOptional pDuration
               tone <- pTone
               dot <- pOptional (pCharSet ".")
               octave <- fromMaybe o <$> pOptional pOctave
               return $ Note tone octave (if isJust dot then Dotted duration else duration)

pComma :: Parser ()
pComma = () <$ do _ <- pCharSet ","
                  pOptional (pCharSet " ")

-- TODO: Schrijf en documenteer de Parser `pHeader`, die de start van de RTTL-string parset.
-- TIPS: We hebben je de naam van het bestand, en het converteren van bpm met fromIntegral vast gegeven.
--       Het stuk dat je rest om te parsen zit tussen de twee dubbele punten!


-- input= AbbaSOS:d=4, o=6, b=124:
pHeader :: Parser (String, Duration, Octave, Beats)
pHeader = do name <- pRepeat (pComplementCharSet ":") -- pRepeat blijf de parser(pComplementCharSet ":") herhalen deze leest string karakters tot ie een : tegenkomt. alles daar voor wordt op geslagen als name. input wordt= ':d=4, o=6, b=124:'
             _ <- pCharSet ":" -- pComplementCharSet vind de : maar haalt deze niet weg, dat doen we dus hier met pCharSet. input wordt= 'd=4, o=6, b=124:'
             _ <- pOptional (pCharSet " ") -- als er een " " staat halen we deze ook weg

             _ <- pString "d=" -- zoek naar "d=", als dit zo is gaan we door en halen we "d=" uit de string. input wordt= '4, o=6, b=124:'
             duration <- pDuration -- pDuration kijkt of het eerst volgende item een getal is en vertaalt dit naar een duration. input wordt= ', o=6, b=124:'

             _ <- pSeparator -- hier halen we de sparators weg "," + " ". input wordt= 'o=6, b=124:'
             _ <- pString "o="  -- zoek naar "o=", als dit zo is gaan we door en halen we "o=" uit de string. input wordt= '6, b=124:'
             octave <- pOctave -- pOctave kijkt of het eerst volgende item een getal is en maakt het tot een int. input wordt= ', b=124:'

             _ <- pSeparator -- hier halen we de sparators weg "," + " ". input wordt= 'b=124:'
             _ <- pString "b=" -- zoek naar "b=", als dit zo is gaan we door en halen we "b=" uit de string. input wordt= '124:'
             bpm <- pNumber -- zoek het bij behorende getal op. input wordt= ':'

             _ <- pCharSet ":" -- . input wordt= ''
             _ <- pOptional (pCharSet " ")
             return (name, duration, octave, fromIntegral bpm) -- return de gevonde name, duration, ocatve en bpm

pSeparator :: Parser ()
pSeparator = () <$ foldl1 mplus [pString " ", pString ", ", pString ","]

pRTTL :: Parser (String, [Note], Beats)
pRTTL = do (t, d, o, b) <- pHeader
           notes <- pRepeatSepBy pSeparator $ mplus (pNote d o) (pPause d)
           return (t, notes, b)

parse :: String -> Maybe (String, [Note], Beats)
parse x = evalStateT pRTTL x