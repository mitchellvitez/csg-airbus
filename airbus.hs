{- Airbus convert to SQL
 - Mitchell Vitez
 - 2016
 -
 - Reads in an input file with airbus schedules and outputs
 - SQL to upload to the database.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import System.Exit
import Data.Attoparsec.Text hiding (take)
import Data.List (find)
import Data.Text as T (Text, toLower, pack, unpack, length)
import Data.Text.IO as T (readFile, putStrLn)
import Data.Text.Read as T (decimal)
import Data.Monoid ((<>))
import Control.Monad (when)

data Season = Season [SeasonVar] [Day] deriving Show
data SeasonVar = SeasonVar Text Text deriving Show
data Day = Day Date [Trip] deriving Show
data Date = Date Text Text Text deriving Show
data Trip = EastboundTrip Int Int Time Time Time Time 
  | WestboundTrip Int Time Time Time deriving Show
data Direction = Westbound | Eastbound
data Time = Time Int Int Char deriving Show
data SeasonRecord = 
  SeasonRecord { seasonId :: Text
               , direction :: Text
               , reservationsOpen :: Text
               }

-- PARSING --

parseSeason :: Parser Season
parseSeason = do
  seasonVars <- many' parseSeasonVar
  many' endOfLine
  days <- many' $ parseDay 
    (if T.toLower (getSeasonVar "direction" seasonVars) == "westbound" 
    then Westbound else Eastbound)
  return $ Season seasonVars days

parseSeasonVar :: Parser SeasonVar
parseSeasonVar = do
  key <- many' $ notChar '='
  char '='
  val <- many' $ notChar '\n'
  many' endOfLine
  return $ SeasonVar (T.pack key) (T.pack val)

parseDate :: Parser Date
parseDate = do
  y  <- count 4 digit
  char '-'
  m <- count 2 digit
  char '-'
  d <- count 2 digit
  many' endOfLine
  return $ Date (T.pack y) (T.pack m) (T.pack d)

parseTime :: Parser Time
parseTime = do
  space
  hour <- many' digit
  char ':'
  minute <- many' digit
  ampm <- satisfy $ inClass "ap"
  return $ Time (read hour) (read minute) ampm

parseDay :: Direction -> Parser Day
parseDay dir = do
  date <- parseDate
  trips <- many' $ parseTrip dir
  return $ Day date trips

parseTrip :: Direction -> Parser Trip
parseTrip Westbound = do
  tripNumber <- many' digit
  takeTill $ inClass "ap"
  satisfy $ inClass "ap"
  north <- parseTime
  mcnamara <- parseTime
  annArbor <- parseTime
  many' $ notChar '\n'
  many' endOfLine
  return $ WestboundTrip (read tripNumber) north mcnamara annArbor
parseTrip Eastbound = do
  tripNumber <- many' digit
  space
  blockNumber <- many' digit
  takeTill $ inClass "ap"
  satisfy $ inClass "ap"
  bursley <- parseTime
  hill <- parseTime
  state <- parseTime
  airport <- parseTime
  many' $ notChar '\n'
  many' endOfLine
  return $ EastboundTrip (read tripNumber) (read blockNumber) 
    bursley hill state airport 

-- CONVERSION --

showDate :: Date -> Text
showDate (Date y m d) = y <> "-" <> m <> "-" <> d

sixPmDayBefore :: Date -> Text
sixPmDayBefore (Date y m d) = y <> "-" <> m <> "-" <> day <> " 18:00:00"
  where day = (T.pack . show . subtract 1 . read . T.unpack) d

twoDigits :: Int -> Text
twoDigits n | (T.length . T.pack . show) n == 1 = "0" <> T.pack (show n)
        | otherwise = T.pack $ show n

formatTime :: Time -> Text
formatTime (Time h m ampm) =
  T.pack (show (if ampm == 'a' || h >= 12 then h else h + 12))
    <> ":" <> twoDigits m 

tripToSql :: SeasonRecord -> Date -> Trip -> Text
tripToSql season date (WestboundTrip tripNumber north mcnamara annArbor) =
  "INSERT INTO `orms_westbound`" <> 
    "(`number`, `date`, `north`, `mcnamara`, `annarbor`, `seasonId`) VALUES (" <>
    T.pack (show tripNumber) <> ",\"" <> showDate date <> "\",\"" <> formatTime north <>
    "\",\"" <> formatTime mcnamara <> "\",\"" <> formatTime annArbor <> "\"," <>
    seasonId season <> ");"
tripToSql season date (EastboundTrip tripNumber blockNumber
    bursley hill state airport) =
  "INSERT INTO `orms_trips`" <> 
  "(`id`, `seasonId`, `number`, `direction`, `date`, `capacity`, `blockNumber`," <>
  " `notes`, `reservationsOpen`, `reservationsClose`, `bursley`, `hill`," <>
  " `state`, `airport`) VALUES (NULL," <> seasonId season <> "," <>
  T.pack (show tripNumber) <> ",\"" <> direction season <> "\",\"" <>
  showDate date <> "\"," <> T.pack (show 48) <> "," <>
  T.pack (show blockNumber) <> "," <> "\"\"" <> ",\"" <>
  reservationsOpen season <> "\",\"" <> sixPmDayBefore date <> "\",\"" <>
  formatTime bursley <> "\",\"" <> formatTime hill <> "\",\"" <>
  formatTime state <> "\",\"" <> formatTime airport <> "\");"

dayToSql :: SeasonRecord -> Day -> [Text]
dayToSql sr (Day date trips) = map (tripToSql sr date) trips

toSql :: Either String Season -> [Text]
toSql (Left x) = [T.pack x]
toSql (Right (Season seasonVars days)) =
  let sr = seasonVarsToRecord seasonVars
  in concatMap (dayToSql sr) days

getSeasonVar :: Text -> [SeasonVar] -> Text
getSeasonVar s lst = val seasonVar
  where pred (SeasonVar k _) = s == k
        seasonVar = find pred lst
        val Nothing = ""
        val (Just (SeasonVar _ v)) = v

seasonVarsToRecord :: [SeasonVar] -> SeasonRecord
seasonVarsToRecord seasonVars =
  let seasonId = getSeasonVar "seasonid" seasonVars
      direction = getSeasonVar "direction" seasonVars
      reservationsOpen = getSeasonVar "reservationsopen" seasonVars
  in SeasonRecord { seasonId = seasonId
                  , direction = direction
                  , reservationsOpen = reservationsOpen
                  }

main :: IO ()
main = do
  args <- getArgs
  when (Prelude.length args /= 1) $
    die "Error: Please supply a filename for conversion"
  file <- T.readFile $ head args
  mapM_ T.putStrLn $ toSql $ parseOnly parseSeason file

