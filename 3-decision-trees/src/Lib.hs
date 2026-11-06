{-|
    Module      : Lib
    Description : Checkpoint voor V2DEP: decision trees
    Copyright   : (c) Nick Roumimper, 2021
    License     : BSD3
    Maintainer  : nick.roumimper@hu.nl

    In dit practicum schrijven we een algoritme om de decision tree te bouwen voor een gegeven dataset.
    Meer informatie over het principe achter decision trees is te vinden in de stof van zowel DEP als CM.
    Let op dat we hier naar een simpele implementatie van decision trees toewerken; parameters zoals maximale
    diepte laten we hier expres weg. Deze code blijft splitsen tot er geen verbetering meer mogelijk is.
    De resulterende boom zal dus "overfit" zijn, maar dat is de verwachting.
    In het onderstaande commentaar betekent het symbool ~> "geeft resultaat terug";
    bijvoorbeeld, 3 + 2 ~> 5 betekent "het uitvoeren van 3 + 2 geeft het resultaat 5 terug".
-}

module Lib where

import Data.List (group, sort)

-- Allereerst definiëren we een datatype voor één enkele rij uit onze traindataset. (CRecord, voor Classification Record.)
-- Een rij uit onze dataset bestaat uit 
--     1) een lijst van numerieke eigenschappen van onze meeteenheid (properties);
--     2) een label voor de klasse waar deze meeteenheid toe behoort (label).
-- Bij het bouwen van onze boom weten we ook het label; ons doel is voor nieuwe data om op basis van de properties
-- te voorspellen welk label erbij hoort - maar dat implementeren we pas helemaal aan het eind.

data CRecord = CRecord { properties :: [Float]
                       , label :: String
                       }
  deriving (Show, Eq)

-- Onze dataset (CDataset, voor Classification Dataset) is dus simpelweg een lijst van CRecords.

type CDataset = [CRecord]

-- Bijgevoegd een paar simpele datasets, die je kunt gebruiken om mee te testen.

miniSet1 :: CDataset
miniSet1 = [CRecord [1,1] "blue", CRecord [2,2] "green", CRecord [3,3] "pink", CRecord [4,4] "purple", CRecord [5,5] "gray"]

miniSet2 :: CDataset
miniSet2 = [CRecord [1,1] "pink", CRecord [2,2] "pink", CRecord [3,3] "purple", CRecord [4,4] "blue", CRecord [5,5] "blue"]

miniSet3 :: CDataset
miniSet3 = [CRecord [1,1] "blue", CRecord [1,2] "green", CRecord [2,1] "green", CRecord [2,2] "green", CRecord [3,1] "orange", CRecord [3,2] "orange"] 


-- ..:: Sectie 1: Het bepalen van de Gini impurity ::..
-- De Gini impurity meet of de dataset rijen bevat uit maar één klasse ("puur"),
--                       of veel rijen uit allerlei verschillende klassen ("impuur").
-- Dit getal zit tussen de 0 en de 1, waar 0 zo puur mogelijk is en 1 zo impuur mogelijk.
-- Als we een splitsing maken in onze boom, willen we de Gini impurity zo laag mogelijk krijgen.

-- Bij het bepalen van de Gini impurity kijken we alleen naar de labels van de data.
-- TODO: schrijf en becommentarieer de functie getLabels die een lijst van alle labels in een dataset teruggeeft.


-- | deze functie geeft een lijst van alle labels in een data set
-- >> getLabels [CRecord [1,1] "blue", CRecord [2,2] "green", CRecord [3,3] "pink", CRecord [4,4] "purple", CRecord [5,5] "gray"] -> ["blue","green","pink","purple","gray"]
getLabels :: CDataset -> [String]
getLabels [] = []
getLabels (CRecord _ label :xs) = label : getLabels xs




-- Om de Gini impurity te bepalen, willen we weten hoe vaak alle labels voorkomen.
-- Voorbeeld: ["a", "b", "a", "c", "c", "a"] wordt [("a", 3), ("b", 1), ("c", 2)]
-- We hebben de volgende twee hulpfuncties geïmporteerd:
--     group :: Eq a => [a] -> [[a]]
--        ^ zet alle gelijke waarden naast elkaar in een eigen lijst.
--          Voorbeeld: [1,1,2,2,2,1] => [[1,1],[2,2,2],[1]] 
--     sort :: Ord a => [a] -> [a]
--        ^ sorteert de lijst.
-- TODO: schrijf en becommentarieer de functie countLabels die telt hoe vaak alle labels in de dataset voorkomen.

-- |deze functie berekent hoevaak elke label voor komt in een data set
--
-- >> countLabels miniSet1 -> [("blue",1),("gray",1),("green",1),("pink",1),("purple",1)]
countLabels :: CDataset -> [(String, Int)]
countLabels set = countlabels sortedList
    where 
        sortedList = group(sort(getLabels(set))) -- pak alle labels uit de set, vervolgens sorteren en grouperen. 
        countlabels :: [[String]] -> [(String, Int)]
        countlabels [] = []
        countlabels (x:xs) = (head x, length x) : countlabels xs -- we gebruiken nu head om de label naam te krijgen en lenght voor het aantal. vervolgens lopen we alle recusive door alle lijsten



-- Voor toekomstig gebruik willen we alvast een functie hebben die het meest voorkomende label
-- van een dataset geeft. Bij gelijkspel mag je eender welk teruggeven.
-- TODO: schrijf en becommentarieer de functie mostFrequentLabel op basis van countLabels.
-- HINT: gebruik een functie uit de Prelude. We gebruiken mostFrequentLabel pas in de laatste sectie!
mostFrequentLabel :: CDataset -> String
mostFrequentLabel set = findLabel (maximum (map snd counts)) counts -- eerst zoek het hoogste aantal. countLabels geeft [(label, aantal)..] met 'map snd' kijken we binnen alle sets in de lijst naar het tweede item(aantal). en met maximun krijgen we het hoogste aantal
    where
        counts = countLabels set

        findLabel :: Int -> [(String, Int)] -> String
        findLabel n ((name, amount):xs)
            | n == amount = name -- n is het hoogste aantal, als we de set tegen komen waar N == aantal geven we de bij behorende label terug
            
            | otherwise = findLabel n xs -- Anders zoek verder in de rest van de lijst op zoek naar het hoogste aantal


-- We definiëren de volgende hulpfunctie (fd, voor "Float Division") om twee Ints te delen als twee Floats.
-- Voorbeeld: fd 3 4 ~> 0.75 (een Float), i.p.v. 0 (een Int).
fd :: Int -> Int -> Float
fd x y = (/) (fromIntegral x) (fromIntegral y)

-- De Gini impurity van één dataset is:
--     de som van de kansen voor elke klasse dat
--         ik, uit alle rijen, willekeurig een rij uit die klasse trek én
--         ik, uit alle rijen, willekeurig een rij uit een andere klasse trek.
-- Zie ook de stof van CM en de Canvas van DEP voor meer context.
-- TODO: schrijf en becommentarieer de functie gini die de Gini impurity van één dataset bepaalt.


-- | deze functie berekent de gini impuritry van een dataset
--
-- >> gini miniSet1 -> 0.800
gini :: CDataset -> Float
gini set = 1 - sum [(pi * pi) | pi <- pis] -- voor elke pi in pis doe pi * zichzelf. tel al die waardes bij elkaar op en doe 1-sum om een waarde tussen 0 en 1 te krijgen
    where 
        labelCounts = countLabels set 
        totalLength = length set  
        pis = createPI labelCounts totalLength

createPI :: [(String, Int)] -> Int -> [Float]
createPI [] _ = []
-- int is het aantal dat een label voor komt 
-- we gebruiken fd om int en totalaantal te delen
createPI ((_, int):xs) totalLength = (fd int totalLength) : createPI xs totalLength


-- De gecombineerde Gini impurity van twee datasets (in ons geval: na een splitsing)
-- is de gewogen som van de Gini impurity van beide sets.
-- Voorbeeld: mijn splitsing leidt tot
--     1) een dataset met 3 rijen en een Gini impurity van 0.2;
--     2) een dataset met 2 rijen en een Gini impurity van 0.1.
-- Dan is de gecombineerde Gini impurity (0.2 * (3/5)) + (0.1 * (2/5)) = 0.16.
-- TODO: schrijf en becommentarieer de functie giniAfterSplit die de gecombineerde Gini impurity van twee datasets bepaalt.

-- | deze functie bepaalt de gecombineerde gini impurity van 2 datasets
--
-- >> giniAfterSplit miniSet1 miniSet2 -> 0.720

-- eerst kijken we hoe land de datasets samen zijn
-- dan bereken we los de gini
-- dan bereken we de weight door de lengte van het dataset door de totaal lengte te doen
-- Dan word de gecombineerde gini op deze manier berekent (0.2 * (3/5)) + (0.1 * (2/5)) = 0.16
giniAfterSplit :: CDataset -> CDataset -> Float
giniAfterSplit data1 data2 = (gini1  * weight1) + (gini2 * weight2) -- (0.2 * (3/5)) + (0.1 * (2/5))
    where 
        totalLength =  length data1 + length data2 -- 5: lengthe dataset
        gini1 = gini data1 -- 0.2
        gini2 = gini data2 -- 0.1
        weight1 = fd (length data1) totalLength -- 3/5: % van de data set
        weight2 = fd (length data2) totalLength -- 2/5



-- ..:: Sectie 2 - Het genereren van alle mogelijke splitsingen in de dataset ::..
-- Bij het genereren van onze decision tree kiezen we telkens de best mogelijke splitsing in de data.
-- In deze simpele implementatie doen we dat brute force: we genereren alle mogelijke splitsingen
-- in de data, en checken ze allemaal. Hier beginnen we door de splitsingen te genereren.

-- We slaan elke mogelijke splitsing op in het datatype CSplit (voor Classification Split). Deze bestaat uit:
--     1) de eigenschap waarop gesplitst wordt, opgeslagen als de index in de lijst van properties (feature);
--     2) de waarde van deze feature waarop we splitsen - ofwel kleiner-gelijk-aan, ofwel groter dan (value).
-- Let op: feature refereert aan de positie in de lijst van properties van een CRecord.
-- Oftewel: als we het hebben over feature 1 van CRecord [8.0, 5.0, 3.0] "x", bedoelen we 5.0.
data CSplit = CSplit { feature :: Int
                     , value :: Float   
                     }
    deriving (Show, Eq, Ord)

-- Allereerst willen we alle waarden van een bepaalde feature in een aparte lijst hebben.
-- TODO: schrijf en becommentarieer de functie getFeature, die gegeven een feature (index in de lijst properties) en een dataset,
--       een lijst teruggeeft van alle waarden van die feature.

-- | deze functie geeft een lijst van alle waarden van een gegeven feature
--
-- >> getFeature 1 miniSet1 -> [1.0,2.0,3.0,4.0,5.0]

getFeature :: Int -> CDataset -> [Float]
getFeature _ [] = []
getFeature index ((CRecord properties _) : xs) = (properties !! index) : getFeature index xs -- uit CDataset haal de properties uit CRecord return de waarde op de mee gegeven index. gebruik recusie voor de overige record in de dataset

-- Als we een lijst van waarden hebben, hoeven we alleen naar de unieke waarden te kijken.
-- Tegelijkertijd is het wel zo makkelijk als de unieke waarden alvast zijn gesorteerd.
-- TODO: schrijf en becommentarieer de functie getUniqueValuesSorted, die uit een lijst van Floats de unieke waarden gesorteerd teruggeeft.
-- HINT: gebruik de hulpfuncties uit de vorige sectie.

-- | deze functie geeft een gesorteerde lijst terug met alle unique waardes
--
-- getUniqueValuesSorted [2.0, 4.0, 3.0, 2.0, 1.0] -> [1.0, 2.0, 3.0, 4.0]

-- we maken gebruik van sort zo dat de lijst al gesorteerd is
-- dan gebruiken we group om alle bij elkaar horende getallen te verzamelin in een lijst -> [[1],[22],[3]]
-- vervolgens pakken we uit al deze sub lijsten alleen de eerste zo dat we een lijst hebben met alle unique waardes
getUniqueValuesSorted :: [Float] -> [Float]
getUniqueValuesSorted set = getUnique sortedGroupList
    where 
        sortedGroupList = group(sort(set))
        getUnique :: [[Float]] -> [Float]
        getUnique [] = []
        getUnique (x:xs) = head x: getUnique xs



-- Als we de dataset splitsen, doen we er verstandig aan om niet precies op een waarde uit de dataset te splitsen.
-- In plaats daarvan splitsen we op het gemiddelde van alle twee naast elkaar gelegen waarden.
-- Voorbeeld: getAverageValues [2.0, 3.0, 5.0, 9.0] ~> [2.5, 4.0, 7.0]
-- Voor de traindata maakt dat geen verschil, maar voor het voorspellen van nieuwe waarden wel.
-- TODO: schrijf en becommentarieer de functie getAverageValues, die de gemiddelden bepaalt van alle paren van twee waarden in een lijst.

-- | deze fucntie pakt steeds de eerste 2 waarden in een lijst en berekent hier het gemiddelde van.
-- | en returned dan een lijst met al deze gemiddelde waarden
--
-- >> getAverageValues [1.0, 4.0, 10.0, 20.0] -> [2.5, 7.0, 15.0]

-- we pakken de eerste 2 waardes in een lijst
-- daar me bereken we het gemiddelde
-- met recursie lopen we door de lijst, we geven het 2de getal mee omdat anders wordt deze waarde niet mee genomen in de volgende berkening
getAverageValues :: [Float] -> [Float]
getAverageValues [] = []  
getAverageValues [_] = [] 
getAverageValues (x:y:xs) = ((x + y) / 2.0) : getAverageValues (y:xs)



-- Met deze functies kunnen we alle mogelijke CSplits bepalen voor één gegeven feature.
-- Voorbeeld: een dataset met in feature 2 de waarden [9.0, 2.0, 5.0, 3.0] wordt
--            [CSplit 2 2.5, CSplit 2 4.0, CSplit 7.0].
-- TODO: schrijf en becommentarieer de functie getFeatureSplits, die alle mogelijke CSplits bepaalt voor een gegeven feature.

-- | deze functie alle CSplits pakt voor een gegeven feature
--
-- >> getFeatureSplits 1 miniSet3 [CSplit {feature = 1, value = 1.5}]


-- eerst worden alle features voor een bepaalde index gepakt
-- vervolgens word deze lijst gesorteerd en alle dubbele waardes er uit gehaadl
-- dan word de averageValue berekent
-- en vervolgens word die waarden in een lijst van CSplit's gezet
getFeatureSplits :: Int -> CDataset -> [CSplit]
getFeatureSplits index set = createCSplit (getAverageValues (getUniqueValuesSorted(getFeature index set))) index
    where
        createCSplit :: [Float] -> Int -> [CSplit]
        createCSplit [] _ = [] 
        createCSplit (x : xs) featureIndex = (CSplit featureIndex x) : createCSplit xs featureIndex -- we gaan door alle features heen maak een CSplit met index en de berekende split waarde

-- Door getFeatureSplits toe te passen voor alle mogelijke features, kunnen we alle mogelijke CSplits bepalen.
-- TODO: schrijf en becommentarieer de functie getAllFeatureSplits, die alle mogelijke CSplits van een dataset bepaalt.

-- | deze functie berekent de CSplit voot alle features van een dataset
--
-- >> getAllFeatureSplits [CRecord [1,1] "blue", CRecord [1,2] "green", CRecord [2,1] "green", CRecord [2,2] "green", CRecord [3,1] "orange", CRecord [3,2] "orange"] -> [CSplit {feature = 0, value = 1.5},CSplit {feature = 0, value = 2.5},CSplit {feature = 1, value = 1.5}]
getAllFeatureSplits :: CDataset -> [CSplit]
getAllFeatureSplits [] = []
getAllFeatureSplits set@(CRecord properties _ : _) = calSplits indexList set
  where
    numFeatures = length properties -- Het aantal features in de dataset
    indexList = [0 .. numFeatures - 1] -- Lijst van indices voor alle features

    calSplits :: [Int] -> CDataset -> [CSplit]
    calSplits [] _ = []
    calSplits (x : xs) dataset = getFeatureSplits x dataset ++ calSplits xs dataset -- recusive pak voor X de feature splits als list, met ++ voeg alle lists samen om de alle splits in de dataset te krijgen

-- ..:: Sectie 3 - Het vinden van de beste splitsing ::..
-- Nu we alle splitsingen hebben gegenereerd, rest ons nog de taak de best mogelijke te vinden.
-- Hiervoor moeten we eerst de functies schrijven om één CDataset, op basis van een CSplit,
-- te splitsen in twee CDatasets.

-- Allereerst schrijven we de functie waarmee we bepalen in welke dataset een CRecord belandt
-- gegeven een bepaalde splitsing. Deze functie moet True teruggeven als de waarde van de feature  
-- kleiner-gelijk-aan is aan de splitswaarde, en False als deze groter is dan de splitswaarde.
-- Voorbeelden: gegeven CSplit 1 3.0 en CRecord [4.0, 2.0, 9.0] "x", is het resultaat True.
--              gegeven CSplit 1 1.0 en CRecord [4.0, 2.0, 9.0] "x", is het resultaat False.
-- TODO: schrijf en becommentarieer de functie splitSingleRecord, die voor een enkel CRecord True of False teruggeeft.
splitSingleRecord :: CSplit -> CRecord -> Bool
splitSingleRecord (CSplit feature value) (CRecord properties _) = properties !! feature <= value -- uit Csplit pakken we de index, !! zorgt dat we uit CRecord de waarde op die index krijgen, vervolgengs vergelijken we die waarde met value(uit Csplit)

-- Nu kunnen we de functie schrijven die één dataset opsplitst in twee, op basis van een CSplit object.
-- TODO: schrijf en becommentarieer de functie splitOnFeature, die één dataset opsplitst in twee.
-- HINT: gebruik een functie uit de Prelude. Onthoud dat CDataset = [CRecord]!
splitOnFeature :: CDataset -> CSplit -> (CDataset, CDataset)
splitOnFeature set split = splitRecords set
    where
        splitRecords :: CDataset -> (CDataset, CDataset)
        splitRecords [] = ([], []) -- lege dataset

        splitRecords (x : xs) -- x eerste item xs rest van de list
            | splitSingleRecord split x = (x : left, right) -- gebruik splitSingleRecord, if true gaat de waarde in list 1
            | otherwise = (left, x : right) -- else 2de list

            where
                (left, right) = splitRecords xs -- recusie

-- Nu kunnen we:
--     1) alle splitsingen genereren voor een CDataset, met behulp van Sectie 2;
--     2) de datasets die resulteren bij elk van die splitsingen genereren.
-- Wel is het van belang dat we onthouden welke splitsing bij welke twee datasets hoort.
-- TODO: schrijf en becommentarieer de functie generateAllSplits, die voor een gegeven dataset alle mogelijke splitsingen "uitprobeert".
generateAllSplits :: CDataset -> [(CSplit, CDataset, CDataset)]
generateAllSplits set = createSplits (getAllFeatureSplits set)
    where
        createSplits :: [CSplit] -> [(CSplit, CDataset, CDataset)]
        createSplits [] = []

        -- Verwerk de eerste splitsing en de resterende splitsingen.
        createSplits (split : rest) =
            -- Bewaar de splitsing samen met de twee resulterende datasets.
            (split, left, right) : createSplits rest
            where
                -- Splits de volledige dataset met de huidige splitsing.
                (left, right) = splitOnFeature set split

-- De laatste stap van deze sectie combineert Sectie 1 en Sectie 3:
--     1) Genereer alle mogelijke splits;
--     2) Bepaal welke van deze splitsingen het beste resultaat geeft - oftewel, de laagste Gini impurity.
-- Hierbij willen we graag zowel de Gini impurity als de splitsing zelf onthouden.
-- TODO: schrijf en becommentarieer de functie findBestSplit, die voor een dataset de best mogelijke splitsing vindt.
-- HINT: gebruik een functie uit de Prelude. Hoe werkt "kleiner dan" voor tupels?
findBestSplit :: CDataset -> (Float, CSplit)
findBestSplit set = minimum (map calculateGini (generateAllSplits set)) -- generateAllSplits set geeft alle mogelijke splits, map calculateGini maakt hier een lijst van tuples van, minimum pakt de laagste/beste score omdat gini de eerste waarde uit de tuple is
    where
        calculateGini :: (CSplit, CDataset, CDataset) -> (Float, CSplit) -- geeft als tulpe de gini en split terug
        calculateGini (split, left, right) = (giniAfterSplit left right, split) -- giniAfterSplit berekent de gewogen Gini impurity voor de dataset na de splitsing


-- ..:: Sectie 4 - Genereren van de decision tree en voorspellen ::..
-- In deze laatste sectie combineren we alle voorgaande om de decision tree op te bouwen,
-- en deze te gebruiken voor voorspellingen.

-- We introduceren het datatype van onze boom, de DTree (Decision Tree).
-- In de DTree is sprake van twee opties:
--     1) We hebben een blad van de boom bereikt, waarin we een voorspelling doen van het label (Leaf String);
--     2) We splitsen op een bepaalde eigenschap, met twee sub-bomen voor <= en > (Branch CSplit DTree DTree).
-- Zoals je al ziet is de definitie van Branch CSplit DTree DTree recursief; er kan dus een onbepaald aantal
-- vertakkingen zijn, maar uiteindelijk eindigt elke vertakking in een blad (Leaf).
-- Let op: we onthouden niet de records uit de dataset, maar wel waarop we ze gesplitst hebben (CSplit)!
data DTree = Branch CSplit DTree DTree | Leaf String deriving (Show, Eq, Ord)

-- De logica achter het recursief bouwen van een decision tree is als volgt:
--     ALS de Gini impurity van de dataset 0.0 is (perfect gesplitst)
--     OF de Gini impurity wordt zelfs met de best mogelijke splitsing niet beter
--         DAN geef ik een Leaf terug met daarin het vaakst voorkomende label;
--     ZO NIET,
--         DAN geef ik een Branch terug met daarin de best mogelijke splitsing
--         en de decision trees (sub-bomen) op basis van de twee datasets na die splitsing.
-- TODO: schrijf en becommentarieer de functie buildDecisionTree.
buildDecisionTree :: CDataset -> DTree
buildDecisionTree [] = Leaf ""
buildDecisionTree set
    | gini set == 0.0 = Leaf (mostFrequentLabel set) -- als gini 0.0 is (perfect) maak een blad met meest voorkomende label
    | bestGini >= gini set = Leaf (mostFrequentLabel set) -- als bij de beste split nog geen verbetering is maak een blad met meest voorkomende label
    | otherwise = Branch split (buildDecisionTree left) (buildDecisionTree right) -- zo niet maak een branch met beste splitsing en decision trees (sub-bomen) op basis van de twee datasets na de splitsing

    where
        (bestGini, split) = findBestSplit set -- vind beste Gini impurity en splits
        (left, right) = splitOnFeature set split -- split de dataset op in 2 datasets(branches)

-- Tot slot, bij het voorspellen weten we alleen de eigenschappen ([Float]), niet het label.
-- TODO: schrijf en becommentarieer de functie predict, die op basis van een boom en de gegeven eigenschappen het label voorspelt.
predict :: DTree -> [Float] -> String
predict (Leaf label) _ = label
predict (Branch split left right) properties
    -- splitSingleRecord geeft een true of false (links of rechts) 
    -- de functie verwacht een CRecord, dus maken we tijdelijk een CRecord met de gegeven eigenschappen en een leeg label
    | splitSingleRecord split (CRecord properties "") = predict left properties -- if true ga verder met linker subboom
    | otherwise = predict right properties -- if false ga verder met rechter subboom
