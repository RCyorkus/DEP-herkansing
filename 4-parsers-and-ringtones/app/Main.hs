{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List (sort)
import Text.Read
import Data.Maybe (fromMaybe)
import Control.Exception
import Text.Printf
import Data.Semigroup (stimes)
import System.Process (runCommand)
import Data.WAVE (putWAVEFile, WAVE(WAVE), WAVEHeader(WAVEHeader))
import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Parsers
import Types
import Instruments

data Colour = Reset | Red | Yellow | Green | Grey

instance Show Colour where
  show Reset = "\ESC[0m"
  show Red = "\ESC[31m"
  show Yellow = "\ESC[33m"
  show Green = "\ESC[32m"
  show Grey = "\ESC[1;30m"

colour :: Colour -> String -> String
colour c = (show c <>) . (<> show Reset)

successString :: String -> String
successString n = colour Green $ "Cool! Functie " ++ n ++ " werkt!"

incorrectString :: Show a => String -> a -> a -> String
incorrectString n r e = (colour Yellow $ "\nHet uitvoeren van " ++ n ++ " werkt nog niet...\n")
  <> "Je antwoord was " <> (colour Red $ show r)
  <> ", maar dit moest " <> (colour Green $ show e) <> " zijn.\n"

errorString :: String -> String
errorString n = colour Red $ "De functie " ++ n ++ " werkt nog niet, je error was:"

data ThreeCase x a b c = ThreeLeft x a | ThreeMiddle x b | ThreeRight x c

singleTest :: forall a. (Eq a, Show a) => (a, a, String, String) -> IO (ThreeCase String SomeException (a, a) a)
singleTest (given, expected, funcname, operationname) = do
  res <- try (evaluate $ given) :: IO (Either SomeException a)
  case res of
    Left error -> do return (ThreeLeft funcname error)
    Right value -> if value == expected then return (ThreeRight funcname value)
                                        else return (ThreeMiddle operationname (value, expected))

lowestCase :: IO (ThreeCase x a b c) -> IO (ThreeCase x a b c) -> IO (ThreeCase x a b c)
lowestCase x y = do
  first <- x
  second <- y
  case (first, second) of
    (x@(ThreeLeft _ _), _)   -> return x
    (_, y@(ThreeLeft _ _))   -> return y
    (x@(ThreeMiddle _ _), _) -> return x
    (_, y@(ThreeMiddle _ _)) -> return y
    otherwise                -> return first

multiTest :: forall a. (Eq a, Show a) => [(a, a, String, String)] -> IO()
multiTest l = do
  result <- foldl1 lowestCase $ map singleTest l
  case result of
    ThreeLeft s x -> do putStrLn $ errorString s
                        putStrLn $ colour Grey $ displayException x
                        putStrLn $ colour Reset ""
    ThreeMiddle s (x, y) -> putStrLn $ incorrectString s x y
    ThreeRight s x -> putStrLn $ successString s

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

typesTests :: IO ()
typesTests = do putStrLn "Tests aan het uitvoeren voor Types.hs:"
                multiTest [((zipWithL (+) [1..5] [9..13]), [10,12,14,16,18], "zipWithL", "zipWithL (+) [1..5] [9..13]"),
                           ((zipWithL (+) [1..5] [9..11]), [10,12,14,4,5], "zipWithL", "zipWithL (+) [1..5] [9..11]"),
                           ((zipWithL (+) [1..3] [9..13]), [10,12,14], "zipWithL", "zipWithL (+) [1..3] [9..13]"),
                           ((zipWithL (+) [1..4] [1..2]), [2,4,3,4], "zipWithL", "zipWithL (+) [1..4] [1..2]")]
                multiTest [((zipWithR (+) [1..5] [9..13]), [10,12,14,16,18], "zipWithR", "zipWithR (+) [1..5] [9..13]"),
                           ((zipWithR (+) [1..5] [9..11]), [10,12,14], "zipWithR", "zipWithR (+) [1..5] [9..11]"),
                           ((zipWithR (+) [1..3] [9..13]), [10,12,14,12,13], "zipWithR", "zipWithR (+) [1..3] [9..13]"),
                           ((zipWithR (+) [1..2] [1..4]), [2,4,3,4], "zipWithR", "zipWithR (+) [1..2] [1..4]")]
                multiTest [(exSound1 <> exSound2, exSound3, "Semigroup en Monoid voor Sound", "exSound1 <> exSound2"),
                           (exSound4 <> exSound1, exSound5, "Semigroup en Monoid voor Sound", "exSound4 <> exSound1"),
                           (mempty <> exSound1, exSound1, "Semigroup en Monoid voor Sound", "mempty <> exSound1"),
                           (exSound2 <> mempty, exSound2, "Semigroup en Monoid voor Sound", "exSound2 <> mempty")]
                multiTest [(exSound1 <+> exSound2, exSound6, "<+>", "exSound1 <+> exSound2"),
                           (exSound1 <+> exSound7, exSound8, "<+>", "exSound1 <+> exSound7"),
                           (exSound7 <+> exSound2, exSound9, "<+>", "exSound7 <+> exSound2")]
                multiTest [((arrange (modifyInstrument sine (modifier (map double))) 440 1), (arrange sine2 440 1), "modifyInstrument en arrange", "arrange (modifyInstrument sine (modifier (map double))) 440 1")]
                main'

parsersTests :: IO ()
parsersTests = do putStrLn "Tests aan het uitvoeren voor Parsers.hs:"
                  -- pComplementCharSet
                  multiTest [(evalStateT (pComplementCharSet ":-") "Yes!", Just 'Y', "pComplementCharSet", "evalStateT (pComplementCharSet \":-\") \"Yes!\""),
                             (evalStateT (pComplementCharSet ":-") ";", Just ';', "pComplementCharSet", "evalStateT (pComplementCharSet \":-\") \";\""),
                             (evalStateT (pComplementCharSet ":-") "-- comment", Nothing, "pComplementCharSet", "evalStateT (pComplementCharSet \":-\") \"-- comment\""),
                             (evalStateT (pComplementCharSet ":-") "::", Nothing, "pComplementCharSet", "evalStateT (pComplementCharSet \":-\") \"::\"")]
                  -- pString
                  multiTest [(evalStateT (pString "foo bar") "foo bar", Just "foo bar", "pString", "evalStateT (pString \"foo bar\") \"foo bar\""),
                             (evalStateT (pString "foo bar") "foo bar!", Just "foo bar", "pString", "evalStateT (pString \"foo bar\") \"foo bar!\""),
                             (evalStateT (pString "foo bar") "foo", Nothing, "pString", "evalStateT (pString \"foo bar\") \"foo\"")]
                  -- pRepeat
                  multiTest [(evalStateT (pRepeat (pCharSet "x")) "xxxyyy", Just "xxx", "pRepeat", "evalStateT (pRepeat (pCharSet \"x\")) \"xxxyyy\""),
                             (evalStateT (pRepeat (pCharSet "x")) "yyy", Nothing, "pRepeat", "evalStateT (pRepeat (pCharSet \"x\")) \"yyy\"")]
                  -- pNumber
                  multiTest [(evalStateT pNumber "1234", Just 1234, "pNumber", "evalStateT pNumber \"1234\""),
                             (evalStateT pNumber "abcd", Nothing, "pNumber", "evalStateT pNumber \"abcd\""),
                             (evalStateT pNumber "1234abcd", Just 1234, "pNumber", "evalStateT pNumber \"1234abcd\"")]
                  -- pOctave
                  multiTest [(evalStateT pOctave "1", Just One, "pOctave", "evalStateT pOctave \"1\""),
                             (evalStateT pOctave "a", Nothing, "pOctave", "evalStateT pOctave \"a\"")]
                  -- pHeader
                  multiTest [(evalStateT pHeader "AbbaSOS:d=4, o=6, b=124:", Just ("AbbaSOS", Quarter, Six, 124), "pHeader", "evalStateT pHeader \"AbbaSOS:d=4, o=6, b=124:\"")]
                  main'

ringtoneMenu :: IO ()
ringtoneMenu = do putStrLn "Huidige opties:"
                  putStrLn "(1) - Sandstorm, door Darude"
                  putStrLn "(2) - ResuRection, door PPK"
                  putStrLn "(3) - Axel F, door Crazy Frog"
                  putStrLn "(4) - Children, door Robert Miles"
                  putStrLn "(5) - NGGYU, door Rick Astley"
                  putStrLn "(6) - We Are Number One, door LazyTown"
                  putStrLn "(7) - Megalovania, door Toby Fox, gearrangeerd door Laurens Lancel"
                  putStrLn "(i) - handmatige eigen invoer"
                  putStrLn "(b) - terug naar het hoofdmenu"
                  putStrLn "(q) - stoppen"
                  i <- getLine
                  case i of
                    "1" -> playRTTL defaultInstrument sandstorm
                    "2" -> playRTTL defaultInstrument ppk
                    "3" -> playRTTL defaultInstrument axelf
                    "4" -> playRTTL defaultInstrument children
                    "5" -> playRTTL defaultInstrument rick
                    "6" -> playRTTL defaultInstrument numberone
                    "7" -> playRTTL defaultInstrument megalovania
                    "i" -> do putStrLn "Voer je eigen RTTL-string in:"
                              j <- getLine
                              playRTTL defaultInstrument j
                    "b" -> main'
                    "q" -> return ()
                    _   -> ringtoneMenu

main' :: IO ()
main' = do putStrLn "Wil je Types.hs testen (t), Parsers.hs testen (p),"
           putStrLn "ringtones genereren (r) of stoppen (q)?"
           i <- getLine
           case i of
             "t" -> typesTests
             "p" -> parsersTests
             "r" -> ringtoneMenu
             "q" -> return ()
             _   -> main

main :: IO ()
main = main'

sandstorm, ppk, axelf, children, numberone, rick, megalovania :: Ringtone
sandstorm = "Sandstorm:d=16, o=3, b=120:16b3 16b3 16b3 16b3 8b3 16b3 16b3 16b3 16b3 16b3 16b3 8b3 16e4 16e4 16e4 16e4 16e4 16e4 8e4 16d4 16d4 16d4 16d4 16d4 16d4 8d4 8a3 16b3 16b3 16b3 16b3 4b3 16b3 16b3 16b3 16b3 8b3 8e4 16b3 16b3 16b3 16b3 4b3 16b3 16b3 16b3 16b3 4b3"
ppk = "ResuRection:d=4, o=6, b=140:2d, 8e, 8f, 8e, 2d, 8e, 8f, 8e, 8d, 8e, 8f, 8e, 8d, 8e, 8f, 8e, 8d, 8e, 8f, 8e, d, a5, 1c, 1p, a#5, f, 8g, 8f, 8e, 2f, 8g, 8f, 8e, 8f, 8g, 8f, 8e, 8f, 8g, 8f, 8e, 8f, 8g, 8f, 8e, f, e, 16f, 16e, 2d"
axelf = "AxelF:d=4, o=5, b=117:f#, 8a., 8f#, 16f#, 8a#, 8f#, 8e, f#, 8c.6, 8f#, 16f#, 8d6, 8c#6, 8a, 8f#, 8c#6, 8f#6, 16f#, 8e, 16e, 8c#, 8g#, f#."
children = "Children:d=4, o=4, b=137:8p, f.5, 1p, g#5, 8g5, d#.5, 1p, g#5, 8g5, c.5, 1p, g#5, 8g5, g#., 1p, 16f, 16g, 16g#, 16c5, f.5, 1p, g#5, 8g5, d#.5, 1p, 16c#5, 16c5, c#5, 8c5, g#, 2p, g., g#, 8c5, f."
rick = "NGGYU:d=4, o=5, b=120:16c, 16d, 16f, 16d, 16a., 16p, 32p, 8a, 16p, g., 16c, 16d, 16f, 16d, 16g., 16p, 32p, 8g, 16p, 8f., 16e, 8d, 16c, 16d, 16f, 16d, f, 8g, 8e., 16d, 8c, 8c4, 8c, 8g, 8p, 2f, 16c, 16d, 16f, 16d, 16a., 16p, 32p, 8a, 16p, g., 16c, 16d, 16f, 16d, c6, 8e, 8f., 16e, 8d, 16c, 16d, 16f, 16d, f, 8g, 8e., 16d, 8c, 8c4, 8c, 8g, 8p, 2f"
numberone = "NumberOne:d=16, o=5, b=168:4f., 8c6, 16b5, 16c6, 16b5, 16c6, 8b5, 8c6, 4g#5, 4f., 8f, 8g#5, 8c6, 4c#6, 4g#5, 4c#6, 4d#6, 8c6, 8c#6, 8c6, 8c#6, 2c6"
megalovania = "Megalovania:d=4, o=4, b=120:16d, 16d, 8d5, 8a, 16p, 16g#, 16p, 16g, 16p, 8f, 16d, 16f, 16g, 16c, 16c, 8d5, 8a, 16p, 16g#, 16p, 16g, 16p, 8f, 16d, 16f, 16g, 16b3, 16b3, 8d5, 8a, 16p, 16g#, 16p, 16g, 16p, 8f, 16d, 16f, 16g, 16a#3, 16a#3, 8d5, 8a, 16p, 16g#, 16p, 16g, 16p, 8f, 16d, 16f, 16g"

playRTTL :: Instrument -> String -> IO ()
playRTTL inst input = case parse input of
  Just (title, notes, bpm) -> playVLC [(title <> ".wav", bpm, [(inst, notes)])]
  Nothing -> putStrLn "Error"

playVLC :: [(FilePath, Beats, [Track])] -> IO ()
playVLC files = do
  mapM_ ((\f (a, b, c) -> f a b c) waves) files
  _ <- runCommand . printf "vlc %s vlc://quit" . unwords . map (\(a, b, c) -> a) $ files
  return ()

playFF :: FilePath -> Beats -> [Track] -> IO ()
playFF outputFilePath bpm tracks = do
  waves outputFilePath bpm tracks
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

waves :: FilePath -> Beats -> [Track] -> IO ()
waves filePath bpm = putWAVEFile filePath . WAVE (WAVEHeader 1 (round sampleRate) 32 Nothing) . map pure
                   . getAsInts . foldr1 (<+>) . map (\(i, ns) -> generateWave bpm i ns)

ps :: String -> IO ()
ps = playRTTL defaultInstrument

exSound1 :: Sound
exSound1 = FloatFrames [0.0, 0.2, 0.1, 0.3, 0.2, 0.4]

exSound2 :: Sound
exSound2 = FloatFrames [0.5, 0.4, 0.3, 0.2, 0.1, 0.0]

exSound3 :: Sound
exSound3 = FloatFrames [0.0001, 0.1999, 0.1001, 0.2999, 0.2001, 0.3999, 0.5001, 0.3999, 0.3001, 0.1999, 0.1001, -0.0001 ]

exSound4 :: Sound
exSound4 = FloatFrames [0.1, 0.08, 0.06, 0.04, 0.02]

exSound5 :: Sound
exSound5 = FloatFrames [0.0999, 0.0799, 0.0599, 0.0399, 0.0199, 0.0001, 0.1999, 0.1001, 0.2999, 0.2001, 0.3999]

exSound6 :: Sound
exSound6 = FloatFrames [0.4999, 0.5999, 0.3999, 0.4999, 0.2999, 0.3999]

exSound7 :: Sound
exSound7 = FloatFrames [0.35, 0.25, 0.15]

exSound8 :: Sound
exSound8 = FloatFrames [0.35, 0.4499, 0.2499, 0.2999, 0.2001, 0.3999]

exSound9 :: Sound
exSound9 = FloatFrames [0.85, 0.6501, 0.4499, 0.1999, 0.1001, -0.0001]

sine2 :: Instrument
sine2 = instrument $ \hz s -> map (double . sin . (* (hz * pi / 24000))) [0.0..48000 * s]

double :: Float -> Float
double = (2*)