{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import Data.List (sort)
import Text.Read
import Data.Maybe (fromMaybe)
import Control.Exception
import Text.Printf

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

singleTest :: forall a. (Eq a, Show a) => String -> (a, a, String) -> IO (ThreeCase String SomeException (a, a) a)
singleTest funcname (given, expected, operationname) = do
  res <- try (evaluate $ given) :: IO (Either SomeException a)
  case res of
    Left error -> do return (ThreeLeft funcname error)
    Right value -> if value == expected then return (ThreeRight funcname value)
                                        else return (ThreeMiddle (funcname ++ " " ++ operationname) (value, expected))

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

multiTest :: forall a. (Eq a, Show a) => String -> [(a, a, String)] -> IO()
multiTest fn l = do
  result <- foldl1 lowestCase $ map (singleTest fn) l
  case result of
    ThreeLeft s x -> do putStrLn $ errorString s
                        putStrLn $ colour Grey $ displayException x
                        putStrLn $ colour Reset ""
    ThreeMiddle s (x, y) -> putStrLn $ incorrectString s x y
    ThreeRight s x -> putStrLn $ successString s

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

sec1 :: IO ()
sec1 = do putStrLn "Tests aan het uitvoeren voor sectie 1:"
          multiTest "getLabels" [((getLabels miniSet1), ["blue","green","pink","purple","gray"], "miniSet1"), 
                     ((getLabels miniSet2), ["pink","pink","purple","blue","blue"], "miniSet2"),
                     ((getLabels miniSet3), ["blue","green","green","green","orange","orange"], "miniSet3")]
          multiTest "countLabels" [((sort $ countLabels miniSet1), [("blue",1),("gray",1),("green",1),("pink",1),("purple",1)], "miniSet1"),
                     ((sort $ countLabels miniSet2), [("blue",2),("pink",2),("purple",1)], "miniSet2"),
                     ((sort $ countLabels miniSet3), [("blue",1),("green",3),("orange",2)], "miniSet3"),
                     ((sort $ countLabels irisFull), [("Iris-setosa",50),("Iris-versicolor",50),("Iris-virginica",50)], "irisFull")]
          multiTest "mostFrequentLabel" [((mostFrequentLabel miniSet3), "green", "miniSet3"),
                     ((mostFrequentLabel exSet2), "green", "exSet2"),
                     ((mostFrequentLabel exSet4), "green", "exSet4")]
          multiTest "gini" [((roundToStr 3 $ gini miniSet1), "0.800", "miniSet1"),
                     ((roundToStr 3 $ gini miniSet2), "0.640", "miniSet2"),
                     ((roundToStr 3 $ gini exSet1), "0.500", "exSet1"),
                     ((roundToStr 3 $ gini exSet4), "0.444", "exSet4"),
                     ((roundToStr 3 $ gini irisFull), "0.667", "irisFull")]
          multiTest "giniAfterSplit" [((roundToStr 3 $ giniAfterSplit miniSet1 miniSet2), "0.720", "miniSet1 miniSet2"),
                     ((roundToStr 3 $ giniAfterSplit miniSet1 miniSet3), "0.697", "miniSet1 miniSet3"),
                     ((roundToStr 3 $ giniAfterSplit exSet1 exSet3), "0.500", "exSet1 exSet3"),
                     ((roundToStr 3 $ giniAfterSplit exSet1 exSet4), "0.472", "exSet1 exSet4"),
                     ((roundToStr 3 $ giniAfterSplit exSet2 exSet4), "0.468", "exSet2 exSet4")]
  
ms1splits0 :: [CSplit]
ms1splits0 = [CSplit {feature = 0, value = 1.5},CSplit {feature = 0, value = 2.5},CSplit {feature = 0, value = 3.5},CSplit {feature = 0, value = 4.5}]

ex1splits0 :: [CSplit]
ex1splits0 = [CSplit {feature = 0, value = 1.5},CSplit {feature = 0, value = 2.5},CSplit {feature = 0, value = 3.5},CSplit {feature = 0, value = 4.5},CSplit {feature = 0, value = 5.5},CSplit {feature = 0, value = 6.5},CSplit {feature = 0, value = 7.5},CSplit {feature = 0, value = 8.5},CSplit {feature = 0, value = 9.5}]

ms1splits :: [CSplit]
ms1splits = [CSplit {feature = 0, value = 1.5},CSplit {feature = 0, value = 2.5},CSplit {feature = 0, value = 3.5},CSplit {feature = 0, value = 4.5},CSplit {feature = 1, value = 1.5},CSplit {feature = 1, value = 2.5},CSplit {feature = 1, value = 3.5},CSplit {feature = 1, value = 4.5}]

ex1splits :: [CSplit]
ex1splits = [CSplit {feature = 0, value = 1.5},CSplit {feature = 0, value = 2.5},CSplit {feature = 0, value = 3.5},CSplit {feature = 0, value = 4.5},CSplit {feature = 0, value = 5.5},CSplit {feature = 0, value = 6.5},CSplit {feature = 0, value = 7.5},CSplit {feature = 0, value = 8.5},CSplit {feature = 0, value = 9.5},CSplit {feature = 1, value = 1.5},CSplit {feature = 1, value = 2.5},CSplit {feature = 1, value = 3.5},CSplit {feature = 1, value = 4.5},CSplit {feature = 1, value = 5.5},CSplit {feature = 1, value = 6.5},CSplit {feature = 1, value = 7.5},CSplit {feature = 1, value = 8.5},CSplit {feature = 1, value = 9.5}]

sec2 :: IO ()
sec2 = do putStrLn "Tests aan het uitvoeren voor sectie 2:"
          multiTest "getFeature" [((getFeature 1 miniSet1), [1.0,2.0,3.0,4.0,5.0], "1 miniSet1"),
                     ((getFeature 1 miniSet3), [1.0,2.0,1.0,2.0,1.0,2.0], "1 miniSet3")]
          multiTest "getUniqueValuesSorted" [((getUniqueValuesSorted [1.0, 2.0, 3.0, 4.0]), [1.0, 2.0, 3.0, 4.0], "[1.0, 2.0, 3.0, 4.0]"),
                     ((getUniqueValuesSorted [4.0, 3.0, 2.0, 1.0]), [1.0, 2.0, 3.0, 4.0], "[4.0, 3.0, 2.0, 1.0]"),
                     ((getUniqueValuesSorted [1.0, 2.0, 1.0, 2.0]), [1.0, 2.0], "[1.0, 2.0, 1.0, 2.0]")]
          multiTest "getAverageValues" [((getAverageValues [2.0, 3.0, 5.0, 9.0]), [2.5, 4.0, 7.0], "[2.0, 3.0, 5.0, 9.0]"),
                     ((getAverageValues [1.0, 4.0, 10.0, 20.0]), [2.5, 7.0, 15.0], "[1.0, 4.0, 10.0, 20.0]"),
                     ((getAverageValues [-1.0, 5.0, 2.0, 4.0]), [2.0, 3.5, 3.0], "[-1.0, 5.0, 2.0, 4.0]")]
          multiTest "getFeatureSplits" [((getFeatureSplits 0 miniSet1), ms1splits0, "0 miniSet1"),
                     ((getFeatureSplits 1 miniSet3), [CSplit {feature = 1, value = 1.5}], "1 miniSet3"),
                     ((getFeatureSplits 0 exSet1), ex1splits0, "0 exSet1")]
          multiTest "getAllFeatureSplits" [((getAllFeatureSplits miniSet1), ms1splits, "miniSet1"),
                     ((getAllFeatureSplits miniSet3), [CSplit {feature = 0, value = 1.5},CSplit {feature = 0, value = 2.5},CSplit {feature = 1, value = 1.5}], "miniSet3"),
                     ((getAllFeatureSplits exSet1), ex1splits, "exSet1")]

ms1sOF12 :: (CDataset, CDataset)
ms1sOF12 = ([CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [2.0,2.0], label = "green"}],[CRecord {properties = [3.0,3.0], label = "pink"},CRecord {properties = [4.0,4.0], label = "purple"},CRecord {properties = [5.0,5.0], label = "gray"}])

ms3sOF01 :: (CDataset, CDataset)
ms3sOF01 = ([CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [1.0,2.0], label = "green"}],[CRecord {properties = [2.0,1.0], label = "green"},CRecord {properties = [2.0,2.0], label = "green"},CRecord {properties = [3.0,1.0], label = "orange"},CRecord {properties = [3.0,2.0], label = "orange"}])

ex1sOF03 :: (CDataset, CDataset)
ex1sOF03 = ([CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [3.0,2.0], label = "blue"},CRecord {properties = [3.0,3.0], label = "blue"},CRecord {properties = [1.0,10.0], label = "blue"},CRecord {properties = [1.0,8.0], label = "blue"},CRecord {properties = [1.0,2.0], label = "blue"},CRecord {properties = [2.0,5.0], label = "blue"},CRecord {properties = [3.0,9.0], label = "blue"},CRecord {properties = [3.0,6.0], label = "blue"}],[CRecord {properties = [5.0,5.0], label = "blue"},CRecord {properties = [5.0,10.0], label = "blue"},CRecord {properties = [4.0,3.0], label = "blue"},CRecord {properties = [5.0,7.0], label = "blue"},CRecord {properties = [4.0,8.0], label = "blue"},CRecord {properties = [5.0,1.0], label = "blue"},CRecord {properties = [6.0,6.0], label = "green"},CRecord {properties = [7.0,9.0], label = "green"},CRecord {properties = [8.0,2.0], label = "green"},CRecord {properties = [9.0,10.0], label = "green"},CRecord {properties = [10.0,4.0], label = "green"},CRecord {properties = [7.0,4.0], label = "green"},CRecord {properties = [8.0,5.0], label = "green"},CRecord {properties = [6.0,1.0], label = "green"},CRecord {properties = [10.0,2.0], label = "green"},CRecord {properties = [10.0,8.0], label = "green"},CRecord {properties = [6.0,9.0], label = "green"},CRecord {properties = [8.0,7.0], label = "green"},CRecord {properties = [10.0,5.0], label = "green"},CRecord {properties = [9.0,8.0], label = "green"},CRecord {properties = [7.0,1.0], label = "green"}])

ms1gAS :: [(CSplit, CDataset, CDataset)]
ms1gAS = [(CSplit {feature = 0, value = 1.5},[CRecord {properties = [1.0,1.0], label = "blue"}],[CRecord {properties = [2.0,2.0], label = "green"},CRecord {properties = [3.0,3.0], label = "pink"},CRecord {properties = [4.0,4.0], label = "purple"},CRecord {properties = [5.0,5.0], label = "gray"}]),(CSplit {feature = 0, value = 2.5},[CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [2.0,2.0], label = "green"}],[CRecord {properties = [3.0,3.0], label = "pink"},CRecord {properties = [4.0,4.0], label = "purple"},CRecord {properties = [5.0,5.0], label = "gray"}]),(CSplit {feature = 0, value = 3.5},[CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [2.0,2.0], label = "green"},CRecord {properties = [3.0,3.0], label = "pink"}],[CRecord {properties = [4.0,4.0], label = "purple"},CRecord {properties = [5.0,5.0], label = "gray"}]),(CSplit {feature = 0, value = 4.5},[CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [2.0,2.0], label = "green"},CRecord {properties = [3.0,3.0], label = "pink"},CRecord {properties = [4.0,4.0], label = "purple"}],[CRecord {properties = [5.0,5.0], label = "gray"}]),(CSplit {feature = 1, value = 1.5},[CRecord {properties = [1.0,1.0], label = "blue"}],[CRecord {properties = [2.0,2.0], label = "green"},CRecord {properties = [3.0,3.0], label = "pink"},CRecord {properties = [4.0,4.0], label = "purple"},CRecord {properties = [5.0,5.0], label = "gray"}]),(CSplit {feature = 1, value = 2.5},[CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [2.0,2.0], label = "green"}],[CRecord {properties = [3.0,3.0], label = "pink"},CRecord {properties = [4.0,4.0], label = "purple"},CRecord {properties = [5.0,5.0], label = "gray"}]),(CSplit {feature = 1, value = 3.5},[CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [2.0,2.0], label = "green"},CRecord {properties = [3.0,3.0], label = "pink"}],[CRecord {properties = [4.0,4.0], label = "purple"},CRecord {properties = [5.0,5.0], label = "gray"}]),(CSplit {feature = 1, value = 4.5},[CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [2.0,2.0], label = "green"},CRecord {properties = [3.0,3.0], label = "pink"},CRecord {properties = [4.0,4.0], label = "purple"}],[CRecord {properties = [5.0,5.0], label = "gray"}])]

ms2gAS :: [(CSplit, CDataset, CDataset)]
ms2gAS = [(CSplit {feature = 0, value = 1.5},[CRecord {properties = [1.0,1.0], label = "pink"}],[CRecord {properties = [2.0,2.0], label = "pink"},CRecord {properties = [3.0,3.0], label = "purple"},CRecord {properties = [4.0,4.0], label = "blue"},CRecord {properties = [5.0,5.0], label = "blue"}]),(CSplit {feature = 0, value = 2.5},[CRecord {properties = [1.0,1.0], label = "pink"},CRecord {properties = [2.0,2.0], label = "pink"}],[CRecord {properties = [3.0,3.0], label = "purple"},CRecord {properties = [4.0,4.0], label = "blue"},CRecord {properties = [5.0,5.0], label = "blue"}]),(CSplit {feature = 0, value = 3.5},[CRecord {properties = [1.0,1.0], label = "pink"},CRecord {properties = [2.0,2.0], label = "pink"},CRecord {properties = [3.0,3.0], label = "purple"}],[CRecord {properties = [4.0,4.0], label = "blue"},CRecord {properties = [5.0,5.0], label = "blue"}]),(CSplit {feature = 0, value = 4.5},[CRecord {properties = [1.0,1.0], label = "pink"},CRecord {properties = [2.0,2.0], label = "pink"},CRecord {properties = [3.0,3.0], label = "purple"},CRecord {properties = [4.0,4.0], label = "blue"}],[CRecord {properties = [5.0,5.0], label = "blue"}]),(CSplit {feature = 1, value = 1.5},[CRecord {properties = [1.0,1.0], label = "pink"}],[CRecord {properties = [2.0,2.0], label = "pink"},CRecord {properties = [3.0,3.0], label = "purple"},CRecord {properties = [4.0,4.0], label = "blue"},CRecord {properties = [5.0,5.0], label = "blue"}]),(CSplit {feature = 1, value = 2.5},[CRecord {properties = [1.0,1.0], label = "pink"},CRecord {properties = [2.0,2.0], label = "pink"}],[CRecord {properties = [3.0,3.0], label = "purple"},CRecord {properties = [4.0,4.0], label = "blue"},CRecord {properties = [5.0,5.0], label = "blue"}]),(CSplit {feature = 1, value = 3.5},[CRecord {properties = [1.0,1.0], label = "pink"},CRecord {properties = [2.0,2.0], label = "pink"},CRecord {properties = [3.0,3.0], label = "purple"}],[CRecord {properties = [4.0,4.0], label = "blue"},CRecord {properties = [5.0,5.0], label = "blue"}]),(CSplit {feature = 1, value = 4.5},[CRecord {properties = [1.0,1.0], label = "pink"},CRecord {properties = [2.0,2.0], label = "pink"},CRecord {properties = [3.0,3.0], label = "purple"},CRecord {properties = [4.0,4.0], label = "blue"}],[CRecord {properties = [5.0,5.0], label = "blue"}])]

ms3gAS :: [(CSplit, CDataset, CDataset)]
ms3gAS = [(CSplit {feature = 0, value = 1.5},[CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [1.0,2.0], label = "green"}],[CRecord {properties = [2.0,1.0], label = "green"},CRecord {properties = [2.0,2.0], label = "green"},CRecord {properties = [3.0,1.0], label = "orange"},CRecord {properties = [3.0,2.0], label = "orange"}]),(CSplit {feature = 0, value = 2.5},[CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [1.0,2.0], label = "green"},CRecord {properties = [2.0,1.0], label = "green"},CRecord {properties = [2.0,2.0], label = "green"}],[CRecord {properties = [3.0,1.0], label = "orange"},CRecord {properties = [3.0,2.0], label = "orange"}]),(CSplit {feature = 1, value = 1.5},[CRecord {properties = [1.0,1.0], label = "blue"},CRecord {properties = [2.0,1.0], label = "green"},CRecord {properties = [3.0,1.0], label = "orange"}],[CRecord {properties = [1.0,2.0], label = "green"},CRecord {properties = [2.0,2.0], label = "green"},CRecord {properties = [3.0,2.0], label = "orange"}])]

inTupleRound :: Int -> (Float, a) -> (String, a)
inTupleRound i (x, y) = (roundToStr i x, y)

sec3 :: IO ()
sec3 = do putStrLn "Tests aan het uitvoeren voor sectie 3:"
          multiTest "splitSingleRecord" [((splitSingleRecord (CSplit 1 3.0) (CRecord [4.0, 2.0, 9.0] "x")), True, "(CSplit 1 3.0) (CRecord [4.0, 2.0, 9.0] \"x\")"),
                     ((splitSingleRecord (CSplit 1 1.0) (CRecord [4.0, 2.0, 9.0] "x")), False, "(CSplit 1 1.0) (CRecord [4.0, 2.0, 9.0] \"x\")"),
                     ((splitSingleRecord (CSplit 2 8.0) (CRecord [4.0, 2.0, 9.0] "x")), False, "(CSplit 2 8.0) (CRecord [4.0, 2.0, 9.0] \"x\")"),
                     ((splitSingleRecord (CSplit 0 8.0) (CRecord [4.0, 2.0, 9.0] "x")), True, "(CSplit 0 8.0) (CRecord [4.0, 2.0, 9.0] \"x\")")]
          multiTest "splitOnFeature" [((splitOnFeature miniSet1 (CSplit 1 2.5)), ms1sOF12, "miniSet1 (CSplit 1 2.5)"),
                     ((splitOnFeature miniSet3 (CSplit 0 1.5)), ms3sOF01, "miniSet3 (CSplit 0 1.5)"),
                     ((splitOnFeature exSet1 (CSplit 0 3.5)), ex1sOF03, "exSet1 (CSplit 0 3.5)")]
          multiTest "generateAllSplits" [((generateAllSplits miniSet1), ms1gAS, "miniSet1"),
                     ((generateAllSplits miniSet2), ms2gAS, "miniSet2"),
                     ((generateAllSplits miniSet3), ms3gAS, "miniSet3")]
          multiTest "findBestSplit" [((inTupleRound 3 $ findBestSplit miniSet3), ("0.250", CSplit {feature = 0, value = 2.5}), "miniSet3"),
                     ((inTupleRound 3 $ findBestSplit exSet2), ("0.463", CSplit {feature = 1, value = 3.5}), "exSet2"),
                     ((inTupleRound 3 $ findBestSplit irisFull), ("0.333", CSplit {feature = 2, value = 2.45}), "irisFull")]

compareCTrees :: Int -> DTree -> DTree -> Bool
compareCTrees _ (Leaf x) (Leaf y) = x == y
compareCTrees i (Branch sa la lb) (Branch sc lc ld) = feature sa == feature sc && roundToStr i (value sa) == roundToStr i (value sc) && compareCTrees i la lc && compareCTrees i lb ld
compareCTrees _ _ _ = False

itfd :: Int -> Int -> Float
itfd a b = (fromIntegral a) / (fromIntegral b)

predictionAccuracy :: DTree -> CDataset -> Float
predictionAccuracy x y = (\x -> itfd (length $ filter ((==) True) x) (length x)) $ map (\z -> ((==) $ label z) $ predict x $ properties z) y

countTreeNodes :: DTree -> Int
countTreeNodes (Branch _ x y) = 1 + countTreeNodes x + countTreeNodes y
countTreeNodes (Leaf _) = 1

sec4 :: IO ()
sec4 = do putStrLn "Als je alle stappen tot dusver hebt gevolgd, is de kans heel groot dat je code"
          putStrLn ((colour Green "precies") ++ (colour Reset " hetzelfde doet als de beoogde oplossing."))
          putStrLn "Echter, de kans bestaat dat je nét iets anders doet dan wij van tevoren kunnen"
          putStrLn "voorzien. Tegelijkertijd willen wij niet, zodat we de kwaliteit van jullie bomen"
          putStrLn "kunnen testen, de predict-functie weggeven."
          putStrLn ""
          putStrLn "Daarom voert deze testcode per dataset de volgende tests uit:"
          putStrLn "[1] Het vergelijkt de boom uit buildDecisionTree met de onze;"
          putStrLn "[2] Het probeert alle waarden uit de dataset op de voorgaande boom."
          putStrLn "Een goede oplossing hoeft níét precies dezelfde boom te geven, maar op de data"
          putStrLn "waarop de boom gebaseerd is móét de precisie 100% (of 1.0) zijn."
          putStrLn ""
          putStrLn "Tests met miniSet3:"
          multiTest "~ test [1] voor miniSet3 ~" [((compareCTrees 3 miniSet3TargetTree (buildDecisionTree miniSet3)), True, "")]
          multiTest "~ test [2] voor miniSet3 ~" [((predictionAccuracy (buildDecisionTree miniSet3) miniSet3), 1.0, "")]
          putStrLn "Tests met exSet4:"
          multiTest "~ test [1] voor exSet4 ~" [((compareCTrees 3 exSet4TargetTree (buildDecisionTree exSet4)), True, "")]
          multiTest "~ test [2] voor exSet4 ~" [((predictionAccuracy (buildDecisionTree exSet4) exSet4), 1.0, "")]
          putStrLn "Tests met de Iris-dataset:"
          multiTest "~ test [1] voor de Iris-dataset ~" [((compareCTrees 3 irisFullTargetTree (buildDecisionTree irisFull)), True, "")]
          multiTest "~ test [2] voor de Iris-dataset ~" [((predictionAccuracy (buildDecisionTree irisFull) irisFull), 1.0, "")]

main :: IO ()
main = do putStrLn "Wil je losse secties laten testen (1-4) of stoppen (q)?"
          choice <- getLine
          case choice of
            "1"       -> do sec1
                            main
            "2"       -> do sec2
                            main
            "3"       -> do sec3
                            main
            "4"       -> do sec4
                            main
            "q"       -> return ()
            otherwise -> main

-- Data sets

exSet1 :: CDataset
exSet1 = [CRecord [1,1] "blue",
          CRecord [3,2] "blue",
          CRecord [3,3] "blue",
          CRecord [1,10] "blue",
          CRecord [5,5] "blue",
          CRecord [1,8] "blue",
          CRecord [5,10] "blue",
          CRecord [4,3] "blue",
          CRecord [5,7] "blue",
          CRecord [1,2] "blue",
          CRecord [2,5] "blue",
          CRecord [3,9] "blue",
          CRecord [4,8] "blue",
          CRecord [3,6] "blue",
          CRecord [5,1] "blue",
          CRecord [6,6] "green",
          CRecord [7,9] "green",
          CRecord [8,2] "green",
          CRecord [9,10] "green",
          CRecord [10,4] "green",
          CRecord [7,4] "green",
          CRecord [8,5] "green",
          CRecord [6,1] "green",
          CRecord [10,2] "green",
          CRecord [10,8] "green",
          CRecord [6,9] "green",
          CRecord [8,7] "green",
          CRecord [10,5] "green",
          CRecord [9,8] "green",
          CRecord [7,1] "green"]

exSet2 :: CDataset
exSet2 = [CRecord [1,1] "blue",
          CRecord [3,2] "blue",
          CRecord [3,3] "blue",
          CRecord [1,10] "green",
          CRecord [5,5] "green",
          CRecord [1,8] "green",
          CRecord [5,10] "green",
          CRecord [4,3] "blue",
          CRecord [5,7] "green",
          CRecord [1,2] "blue",
          CRecord [2,5] "green",
          CRecord [3,9] "green",
          CRecord [4,8] "green",
          CRecord [3,6] "green",
          CRecord [5,1] "blue",
          CRecord [6,6] "blue",
          CRecord [7,9] "blue",
          CRecord [8,2] "green",
          CRecord [9,10] "blue",
          CRecord [10,4] "green",
          CRecord [7,4] "green",
          CRecord [8,5] "green",
          CRecord [6,1] "green",
          CRecord [10,2] "green",
          CRecord [10,8] "blue",
          CRecord [6,9] "blue",
          CRecord [8,7] "blue",
          CRecord [10,5] "green",
          CRecord [9,8] "blue",
          CRecord [7,1] "green"]

exSet3 :: CDataset
exSet3 = [CRecord [1,1] "blue",
          CRecord [3,2] "green",
          CRecord [3,3] "blue",
          CRecord [1,10] "green",
          CRecord [5,5] "blue",
          CRecord [1,8] "green",
          CRecord [5,10] "blue",
          CRecord [4,3] "green",
          CRecord [5,7] "blue",
          CRecord [1,2] "green",
          CRecord [2,5] "blue",
          CRecord [3,9] "green",
          CRecord [4,8] "blue",
          CRecord [3,6] "green",
          CRecord [5,1] "blue",
          CRecord [6,6] "green",
          CRecord [7,9] "blue",
          CRecord [8,2] "green",
          CRecord [9,10] "blue",
          CRecord [10,4] "green",
          CRecord [7,4] "blue",
          CRecord [8,5] "green",
          CRecord [6,1] "blue",
          CRecord [10,2] "green",
          CRecord [10,8] "blue",
          CRecord [6,9] "green",
          CRecord [8,7] "blue",
          CRecord [10,5] "green",
          CRecord [9,8] "blue",
          CRecord [7,1] "green"]

exSet4 :: CDataset
exSet4 = [CRecord [1,1] "blue",
          CRecord [3,2] "blue",
          CRecord [3,3] "blue",
          CRecord [1,10] "green",
          CRecord [5,5] "blue",
          CRecord [1,8] "green",
          CRecord [5,10] "green",
          CRecord [4,3] "blue",
          CRecord [5,7] "green",
          CRecord [1,2] "green",
          CRecord [2,5] "green",
          CRecord [3,9] "green",
          CRecord [4,8] "green",
          CRecord [3,6] "green",
          CRecord [5,1] "blue",
          CRecord [6,6] "green",
          CRecord [7,9] "green",
          CRecord [8,2] "blue",
          CRecord [9,10] "green",
          CRecord [10,4] "green",
          CRecord [7,4] "blue",
          CRecord [8,5] "green",
          CRecord [6,1] "blue",
          CRecord [10,2] "green",
          CRecord [10,8] "green",
          CRecord [6,9] "green",
          CRecord [8,7] "green",
          CRecord [10,5] "green",
          CRecord [9,8] "green",
          CRecord [7,1] "blue"]

irisFull :: CDataset
irisFull = [CRecord [5.1, 3.5, 1.4, 0.2] "Iris-setosa",
            CRecord [4.9, 3.0, 1.4, 0.2] "Iris-setosa",
            CRecord [4.7, 3.2, 1.3, 0.2] "Iris-setosa",
            CRecord [4.6, 3.1, 1.5, 0.2] "Iris-setosa",
            CRecord [5.0, 3.6, 1.4, 0.2] "Iris-setosa",
            CRecord [5.4, 3.9, 1.7, 0.4] "Iris-setosa",
            CRecord [4.6, 3.4, 1.4, 0.3] "Iris-setosa",
            CRecord [5.0, 3.4, 1.5, 0.2] "Iris-setosa",
            CRecord [4.4, 2.9, 1.4, 0.2] "Iris-setosa",
            CRecord [4.9, 3.1, 1.5, 0.1] "Iris-setosa",
            CRecord [5.4, 3.7, 1.5, 0.2] "Iris-setosa",
            CRecord [4.8, 3.4, 1.6, 0.2] "Iris-setosa",
            CRecord [4.8, 3.0, 1.4, 0.1] "Iris-setosa",
            CRecord [4.3, 3.0, 1.1, 0.1] "Iris-setosa",
            CRecord [5.8, 4.0, 1.2, 0.2] "Iris-setosa",
            CRecord [5.7, 4.4, 1.5, 0.4] "Iris-setosa",
            CRecord [5.4, 3.9, 1.3, 0.4] "Iris-setosa",
            CRecord [5.1, 3.5, 1.4, 0.3] "Iris-setosa",
            CRecord [5.7, 3.8, 1.7, 0.3] "Iris-setosa",
            CRecord [5.1, 3.8, 1.5, 0.3] "Iris-setosa",
            CRecord [5.4, 3.4, 1.7, 0.2] "Iris-setosa",
            CRecord [5.1, 3.7, 1.5, 0.4] "Iris-setosa",
            CRecord [4.6, 3.6, 1.0, 0.2] "Iris-setosa",
            CRecord [5.1, 3.3, 1.7, 0.5] "Iris-setosa",
            CRecord [4.8, 3.4, 1.9, 0.2] "Iris-setosa",
            CRecord [5.0, 3.0, 1.6, 0.2] "Iris-setosa",
            CRecord [5.0, 3.4, 1.6, 0.4] "Iris-setosa",
            CRecord [5.2, 3.5, 1.5, 0.2] "Iris-setosa",
            CRecord [5.2, 3.4, 1.4, 0.2] "Iris-setosa",
            CRecord [4.7, 3.2, 1.6, 0.2] "Iris-setosa",
            CRecord [4.8, 3.1, 1.6, 0.2] "Iris-setosa",
            CRecord [5.4, 3.4, 1.5, 0.4] "Iris-setosa",
            CRecord [5.2, 4.1, 1.5, 0.1] "Iris-setosa",
            CRecord [5.5, 4.2, 1.4, 0.2] "Iris-setosa",
            CRecord [4.9, 3.1, 1.5, 0.1] "Iris-setosa",
            CRecord [5.0, 3.2, 1.2, 0.2] "Iris-setosa",
            CRecord [5.5, 3.5, 1.3, 0.2] "Iris-setosa",
            CRecord [4.9, 3.1, 1.5, 0.1] "Iris-setosa",
            CRecord [4.4, 3.0, 1.3, 0.2] "Iris-setosa",
            CRecord [5.1, 3.4, 1.5, 0.2] "Iris-setosa",
            CRecord [5.0, 3.5, 1.3, 0.3] "Iris-setosa",
            CRecord [4.5, 2.3, 1.3, 0.3] "Iris-setosa",
            CRecord [4.4, 3.2, 1.3, 0.2] "Iris-setosa",
            CRecord [5.0, 3.5, 1.6, 0.6] "Iris-setosa",
            CRecord [5.1, 3.8, 1.9, 0.4] "Iris-setosa",
            CRecord [4.8, 3.0, 1.4, 0.3] "Iris-setosa",
            CRecord [5.1, 3.8, 1.6, 0.2] "Iris-setosa",
            CRecord [4.6, 3.2, 1.4, 0.2] "Iris-setosa",
            CRecord [5.3, 3.7, 1.5, 0.2] "Iris-setosa",
            CRecord [5.0, 3.3, 1.4, 0.2] "Iris-setosa",
            CRecord [7.0, 3.2, 4.7, 1.4] "Iris-versicolor",
            CRecord [6.4, 3.2, 4.5, 1.5] "Iris-versicolor",
            CRecord [6.9, 3.1, 4.9, 1.5] "Iris-versicolor",
            CRecord [5.5, 2.3, 4.0, 1.3] "Iris-versicolor",
            CRecord [6.5, 2.8, 4.6, 1.5] "Iris-versicolor",
            CRecord [5.7, 2.8, 4.5, 1.3] "Iris-versicolor",
            CRecord [6.3, 3.3, 4.7, 1.6] "Iris-versicolor",
            CRecord [4.9, 2.4, 3.3, 1.0] "Iris-versicolor",
            CRecord [6.6, 2.9, 4.6, 1.3] "Iris-versicolor",
            CRecord [5.2, 2.7, 3.9, 1.4] "Iris-versicolor",
            CRecord [5.0, 2.0, 3.5, 1.0] "Iris-versicolor",
            CRecord [5.9, 3.0, 4.2, 1.5] "Iris-versicolor",
            CRecord [6.0, 2.2, 4.0, 1.0] "Iris-versicolor",
            CRecord [6.1, 2.9, 4.7, 1.4] "Iris-versicolor",
            CRecord [5.6, 2.9, 3.6, 1.3] "Iris-versicolor",
            CRecord [6.7, 3.1, 4.4, 1.4] "Iris-versicolor",
            CRecord [5.6, 3.0, 4.5, 1.5] "Iris-versicolor",
            CRecord [5.8, 2.7, 4.1, 1.0] "Iris-versicolor",
            CRecord [6.2, 2.2, 4.5, 1.5] "Iris-versicolor",
            CRecord [5.6, 2.5, 3.9, 1.1] "Iris-versicolor",
            CRecord [5.9, 3.2, 4.8, 1.8] "Iris-versicolor",
            CRecord [6.1, 2.8, 4.0, 1.3] "Iris-versicolor",
            CRecord [6.3, 2.5, 4.9, 1.5] "Iris-versicolor",
            CRecord [6.1, 2.8, 4.7, 1.2] "Iris-versicolor",
            CRecord [6.4, 2.9, 4.3, 1.3] "Iris-versicolor",
            CRecord [6.6, 3.0, 4.4, 1.4] "Iris-versicolor",
            CRecord [6.8, 2.8, 4.8, 1.4] "Iris-versicolor",
            CRecord [6.7, 3.0, 5.0, 1.7] "Iris-versicolor",
            CRecord [6.0, 2.9, 4.5, 1.5] "Iris-versicolor",
            CRecord [5.7, 2.6, 3.5, 1.0] "Iris-versicolor",
            CRecord [5.5, 2.4, 3.8, 1.1] "Iris-versicolor",
            CRecord [5.5, 2.4, 3.7, 1.0] "Iris-versicolor",
            CRecord [5.8, 2.7, 3.9, 1.2] "Iris-versicolor",
            CRecord [6.0, 2.7, 5.1, 1.6] "Iris-versicolor",
            CRecord [5.4, 3.0, 4.5, 1.5] "Iris-versicolor",
            CRecord [6.0, 3.4, 4.5, 1.6] "Iris-versicolor",
            CRecord [6.7, 3.1, 4.7, 1.5] "Iris-versicolor",
            CRecord [6.3, 2.3, 4.4, 1.3] "Iris-versicolor",
            CRecord [5.6, 3.0, 4.1, 1.3] "Iris-versicolor",
            CRecord [5.5, 2.5, 4.0, 1.3] "Iris-versicolor",
            CRecord [5.5, 2.6, 4.4, 1.2] "Iris-versicolor",
            CRecord [6.1, 3.0, 4.6, 1.4] "Iris-versicolor",
            CRecord [5.8, 2.6, 4.0, 1.2] "Iris-versicolor",
            CRecord [5.0, 2.3, 3.3, 1.0] "Iris-versicolor",
            CRecord [5.6, 2.7, 4.2, 1.3] "Iris-versicolor",
            CRecord [5.7, 3.0, 4.2, 1.2] "Iris-versicolor",
            CRecord [5.7, 2.9, 4.2, 1.3] "Iris-versicolor",
            CRecord [6.2, 2.9, 4.3, 1.3] "Iris-versicolor",
            CRecord [5.1, 2.5, 3.0, 1.1] "Iris-versicolor",
            CRecord [5.7, 2.8, 4.1, 1.3] "Iris-versicolor",
            CRecord [6.3, 3.3, 6.0, 2.5] "Iris-virginica",
            CRecord [5.8, 2.7, 5.1, 1.9] "Iris-virginica",
            CRecord [7.1, 3.0, 5.9, 2.1] "Iris-virginica",
            CRecord [6.3, 2.9, 5.6, 1.8] "Iris-virginica",
            CRecord [6.5, 3.0, 5.8, 2.2] "Iris-virginica",
            CRecord [7.6, 3.0, 6.6, 2.1] "Iris-virginica",
            CRecord [4.9, 2.5, 4.5, 1.7] "Iris-virginica",
            CRecord [7.3, 2.9, 6.3, 1.8] "Iris-virginica",
            CRecord [6.7, 2.5, 5.8, 1.8] "Iris-virginica",
            CRecord [7.2, 3.6, 6.1, 2.5] "Iris-virginica",
            CRecord [6.5, 3.2, 5.1, 2.0] "Iris-virginica",
            CRecord [6.4, 2.7, 5.3, 1.9] "Iris-virginica",
            CRecord [6.8, 3.0, 5.5, 2.1] "Iris-virginica",
            CRecord [5.7, 2.5, 5.0, 2.0] "Iris-virginica",
            CRecord [5.8, 2.8, 5.1, 2.4] "Iris-virginica",
            CRecord [6.4, 3.2, 5.3, 2.3] "Iris-virginica",
            CRecord [6.5, 3.0, 5.5, 1.8] "Iris-virginica",
            CRecord [7.7, 3.8, 6.7, 2.2] "Iris-virginica",
            CRecord [7.7, 2.6, 6.9, 2.3] "Iris-virginica",
            CRecord [6.0, 2.2, 5.0, 1.5] "Iris-virginica",
            CRecord [6.9, 3.2, 5.7, 2.3] "Iris-virginica",
            CRecord [5.6, 2.8, 4.9, 2.0] "Iris-virginica",
            CRecord [7.7, 2.8, 6.7, 2.0] "Iris-virginica",
            CRecord [6.3, 2.7, 4.9, 1.8] "Iris-virginica",
            CRecord [6.7, 3.3, 5.7, 2.1] "Iris-virginica",
            CRecord [7.2, 3.2, 6.0, 1.8] "Iris-virginica",
            CRecord [6.2, 2.8, 4.8, 1.8] "Iris-virginica",
            CRecord [6.1, 3.0, 4.9, 1.8] "Iris-virginica",
            CRecord [6.4, 2.8, 5.6, 2.1] "Iris-virginica",
            CRecord [7.2, 3.0, 5.8, 1.6] "Iris-virginica",
            CRecord [7.4, 2.8, 6.1, 1.9] "Iris-virginica",
            CRecord [7.9, 3.8, 6.4, 2.0] "Iris-virginica",
            CRecord [6.4, 2.8, 5.6, 2.2] "Iris-virginica",
            CRecord [6.3, 2.8, 5.1, 1.5] "Iris-virginica",
            CRecord [6.1, 2.6, 5.6, 1.4] "Iris-virginica",
            CRecord [7.7, 3.0, 6.1, 2.3] "Iris-virginica",
            CRecord [6.3, 3.4, 5.6, 2.4] "Iris-virginica",
            CRecord [6.4, 3.1, 5.5, 1.8] "Iris-virginica",
            CRecord [6.0, 3.0, 4.8, 1.8] "Iris-virginica",
            CRecord [6.9, 3.1, 5.4, 2.1] "Iris-virginica",
            CRecord [6.7, 3.1, 5.6, 2.4] "Iris-virginica",
            CRecord [6.9, 3.1, 5.1, 2.3] "Iris-virginica",
            CRecord [5.8, 2.7, 5.1, 1.9] "Iris-virginica",
            CRecord [6.8, 3.2, 5.9, 2.3] "Iris-virginica",
            CRecord [6.7, 3.3, 5.7, 2.5] "Iris-virginica",
            CRecord [6.7, 3.0, 5.2, 2.3] "Iris-virginica",
            CRecord [6.3, 2.5, 5.0, 1.9] "Iris-virginica",
            CRecord [6.5, 3.0, 5.2, 2.0] "Iris-virginica",
            CRecord [6.2, 3.4, 5.4, 2.3] "Iris-virginica",
            CRecord [5.9, 3.0, 5.1, 1.8] "Iris-virginica"]

-- Target solutions

miniSet3TargetTree :: DTree
miniSet3TargetTree = Branch (CSplit {feature = 0, value = 2.5}) (Branch (CSplit {feature = 0, value = 1.5}) (Branch (CSplit {feature = 1, value = 1.5}) (Leaf "blue") (Leaf "green")) (Leaf "green")) (Leaf "orange")

exSet1TargetTree :: DTree
exSet1TargetTree = Branch (CSplit {feature = 0, value = 5.5}) (Leaf "blue") (Leaf "green")

exSet2TargetTree :: DTree
exSet2TargetTree = Branch (CSplit {feature = 1, value = 3.5}) (Branch (CSplit {feature = 0, value = 5.5}) (Leaf "blue") (Leaf "green")) (Branch (CSplit {feature = 0, value = 5.5}) (Leaf "green") (Branch (CSplit {feature = 1, value = 5.5}) (Leaf "green") (Leaf "blue")))

exSet3TargetTree :: DTree
exSet3TargetTree = Branch (CSplit {feature = 1, value = 6.5}) (Branch (CSplit {feature = 0, value = 7.5}) (Branch (CSplit {feature = 1, value = 5.5}) (Branch (CSplit {feature = 1, value = 3.5}) (Branch (CSplit {feature = 1, value = 1.5}) (Branch (CSplit {feature = 0, value = 6.5}) (Leaf "blue") (Leaf "green")) (Branch (CSplit {feature = 1, value = 2.5}) (Leaf "green") (Branch (CSplit {feature = 0, value = 3.5}) (Leaf "blue") (Leaf "green")))) (Leaf "blue")) (Leaf "green")) (Leaf "green")) (Branch (CSplit {feature = 0, value = 3.5}) (Leaf "green") (Branch (CSplit {feature = 0, value = 6.5}) (Branch (CSplit {feature = 0, value = 5.5}) (Leaf "blue") (Leaf "green")) (Leaf "blue")))

exSet4TargetTree :: DTree
exSet4TargetTree = Branch (CSplit {feature = 1, value = 4.5}) (Branch (CSplit {feature = 0, value = 9.0}) (Branch (CSplit {feature = 0, value = 2.0}) (Branch (CSplit {feature = 1, value = 1.5}) (Leaf "blue") (Leaf "green")) (Leaf "blue")) (Leaf "green")) (Branch (CSplit {feature = 1, value = 5.5}) (Branch (CSplit {feature = 0, value = 6.5}) (Branch (CSplit {feature = 0, value = 3.5}) (Leaf "green") (Leaf "blue")) (Leaf "green")) (Leaf "green"))

irisFullTargetTree :: DTree
irisFullTargetTree = Branch (CSplit {feature = 2, value = 2.45}) (Leaf "Iris-setosa") (Branch (CSplit {feature = 3, value = 1.75}) (Branch (CSplit {feature = 2, value = 4.95}) (Branch (CSplit {feature = 3, value = 1.65}) (Leaf "Iris-versicolor") (Leaf "Iris-virginica")) (Branch (CSplit {feature = 3, value = 1.55}) (Leaf "Iris-virginica") (Branch (CSplit {feature = 0, value = 6.95}) (Leaf "Iris-versicolor") (Leaf "Iris-virginica")))) (Branch (CSplit {feature = 2, value = 4.85}) (Branch (CSplit {feature = 0, value = 5.95}) (Leaf "Iris-versicolor") (Leaf "Iris-virginica")) (Leaf "Iris-virginica")))
