{-# LANGUAGE OverloadedStrings, TypeApplications #-}

import Control.Lens (_3, view)
import Data.ByteString.Lazy hiding (putStrLn, take)
import Data.Csv hiding (lookup)
import Data.Either (rights)
import Data.Foldable
import Data.Function (on)
import Data.List (sortBy)
import Data.Map as Map ((!), fromListWith, keys, lookup, Map)
import Data.Vector hiding ((!), mapM_, sum, take, toList)
import Prelude hiding (readFile)
import Text.Printf

-- path = "2020_03_29/hospitalization_all_locs_corrected.csv
-- path = "2020_03_30/Hospitalization_all_locs.csv"
path = "2020_03_31.1/Hospitalization_all_locs.csv"

data State
  = State
    { stateName :: String
    , statePop :: Integer
    , deaths :: Integer
    } deriving Show

stateInfo :: [State]
stateInfo =
  [ State "Alabama" 4908621 4
  , State "Alaska" 734002 3
  , State "Arizona" 7378494 17
  , State "Arkansas" 3038999 6
  , State "California" 39937489 123
  , State "Colorado" 5845526 47
  , State "Connecticut" 3563077 34
  , State "Delaware" 982895 6
  , State "District of Columbia" 720687 9
  , State "Florida" 21992985 59
  , State "Georgia" 10736059 80
  , State "Hawaii" 1412687 0
  , State "Idaho" 1826156 6
  , State "Illinois" 12659682 65
  , State "Indiana" 6745354 35
  , State "Iowa" 3179849 4
  , State "Kansas" 2910357 6
  , State "Kentucky" 4499692 9
  , State "King and Snohomish Counties (excluding Life Care Center), WA" 1 0
  , State "Life Care Center, Kirkland, WA" 1 0
  , State "Louisiana" 4645184 151
  , State "Maine" 1345790 3
  , State "Maryland" 6083116 15
  , State "Massachusetts" 6976597 48
  , State "Michigan" 10045029 132
  , State "Minnesota" 5700671 9
  , State "Mississippi" 2989260 14
  , State "Missouri" 6169270 12
  , State "Montana" 1086759 2
  , State "Nebraska" 1952570 2
  , State "Nevada" 3139658 15
  , State "New Hampshire" 1371246 3
  , State "New Jersey" 8936574 161
  , State "New Mexico" 2096640 2
  , State "New York" 19440469 965
  , State "North Carolina" 10611862 5
  , State "North Dakota" 761723 1
  , State "Ohio" 11747694 29
  , State "Oklahoma" 3954821 16
  , State "Oregon" 4301089 13
  , State "Other Counties, WA" 1 0
  , State "Pennsylvania" 12820878 38
  , State "Puerto Rico" 3032165 6
  , State "Rhode Island" 1056161 3
  , State "South Carolina" 5210095 16
  , State "South Dakota" 903027 1
  , State "Tennessee" 6897576 7
  , State "Texas" 29472295 34
  , State "United States of America" 1 0
  , State "Utah" 3282115 2
  , State "Vermont" 628061 12
  , State "Virginia" 8626207 25
  , State "Washington" 7797095 195
  , State "West Virginia" 1778070 0
  , State "Wisconsin" 5851754 13
  , State "Wyoming" 567025 0 ]

data Rec = Rec
  { v1 :: String
  , location :: String
  , date :: String
  , allbed_mean :: Double
  , allbed_lower :: Double
  , allbed_upper :: Double
  , icubed_mean :: Double
  , icubed_lower :: Double
  , icubed_upper :: Double
  , invVen_mean :: Double
  , invVen_lower :: Double
  , invVen_upper :: Double
  , deaths_mean :: Double
  , deaths_lower :: Double
  , deaths_upper :: Double
  , admis_mean :: Double
  , admis_lower :: Double
  , admis_upper :: Double
  , newICU_mean :: Double
  , newICU_lower :: Double
  , newICU_upper :: Double
  , totdea_mean :: Double
  , totdea_lower :: Double
  , totdea_upper :: Double
  , bedover_mean :: Double
  , bedover_lower :: Double
  , bedover_upper :: Double
  , icuover_mean :: Double
  , icuover_lower :: Double
  , icuover_upper :: Double
  , location_name :: String
  } deriving Show

instance FromNamedRecord Rec where
  parseNamedRecord m =
    Rec <$>
      m .: "V1" <*>
      m .: "location" <*>
      m .: "date" <*>
      m .: "allbed_mean" <*>
      m .: "allbed_lower" <*>
      m .: "allbed_upper" <*>
      m .: "ICUbed_mean" <*>
      m .: "ICUbed_lower" <*>
      m .: "ICUbed_upper" <*>
      m .: "InvVen_mean" <*>
      m .: "InvVen_lower" <*>
      m .: "InvVen_upper" <*>
      m .: "deaths_mean" <*>
      m .: "deaths_lower" <*>
      m .: "deaths_upper" <*>
      m .: "admis_mean" <*>
      m .: "admis_lower" <*>
      m .: "admis_upper" <*>
      m .: "newICU_mean" <*>
      m .: "newICU_lower" <*>
      m .: "newICU_upper" <*>
      m .: "totdea_mean" <*>
      m .: "totdea_lower" <*>
      m .: "totdea_upper" <*>
      m .: "bedover_mean" <*>
      m .: "bedover_lower" <*>
      m .: "bedover_upper" <*>
      m .: "icuover_mean" <*>
      m .: "icuover_lower" <*>
      m .: "icuover_upper" <*>
      m .: "location_name"

main = do
  bs <- readFile path
  case decodeByName bs of
    Left s -> putStrLn s
    Right (h, v) -> do
      let mp :: Map String [Rec]
          mp = fromListWith (<>) (fmap (\r -> (location_name r, [r])) (toList v))
      putStrLn "\nPredicted Deaths per 1M\n"
      mapM_ (\(s, deaths, dfactor) -> putStrLn (printf "%.0f" (dfactor * 1000000.0)  <> " - " <>
                                     stateName s <>
                                     " (# deaths: " <> printf "%.0f" deaths <> ")"))
            (sortBy (compare `on` view _3) (rights (fmap (doState mp deathsPerCapita) stateInfo)))
      putStrLn "\nDate of Predicted Peak Ventilator Use\n"
      mapM_ (\(s, r) -> putStrLn (date r <> " - " <> stateName s))
            (sortBy (compare `on` (date . snd)) (rights (fmap (doState mp ventilatorPeak) stateInfo)))
  where
    doState :: Map String [Rec] -> (State -> [Rec] -> r) -> State -> Either String r
    doState mp f s =
      case Map.lookup (stateName s) mp of
        Nothing -> Left ("Unknown state: " <> show s)
        Just rs -> Right (f s rs)
    deathsPerCapita :: State -> [Rec] -> (State, Double, Double)
    deathsPerCapita s rs =
      (s, deaths, dfactor)
      where
        name = stateName s
        deaths = sum (fmap deaths_mean rs)
        dfactor = deaths / fromIntegral (statePop s)
    ventilatorPeak :: State -> [Rec] -> (State, Rec)
    ventilatorPeak s rs = do
      case sortBy (flip (compare `on` invVen_mean)) rs of
        (r : _) -> (s, r)
