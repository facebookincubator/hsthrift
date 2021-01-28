-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.TimeSec
  ( Time(..)
  , TimeSpan(..)
  , TimeRange(..)
  , fromUTCTime
  , toUTCTime
  , addTime, subTime, timeDiff
  , addTimeSpan
  , now, timeAgo
  , minutes, hours, days, weeks
  , minute, hour, day, week
  , toSeconds, toMinutes, toHours, toDays
  , trLength
  , trLast
  , ppUTCTime, ppDate, ppTime, ppTimeSpan, ppTimeSpanWithGranularity
  , PPTimeSpanGranularity(..)
  ) where

import Database.MySQL.Simple.Param
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Maybe (catMaybes)
import TextShow

import GHC.Generics
import Control.DeepSeq
import Data.Hashable

-- | Unix timestamp. Time in seconds
newtype Time = Time { timeInSeconds :: Int }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, TextShow, Num, Enum)

instance NFData   Time
instance Hashable Time

instance Param Time where
  render = render . timeInSeconds

-- | 'TimeSpan' means a time difference, duration in seconds,
-- as opposed to 'Time', which is a specific point in time
-- (a unix timestamp).
newtype TimeSpan = TimeSpan { timeSpanInSeconds :: Int }
                 deriving (Show, Eq, Ord)

data TimeRange = TimeRange
  { start :: Time
  , end :: Time
  } deriving (Show, Eq)

fromUTCTime :: UTCTime -> Time
fromUTCTime = Time . truncate . utcTimeToPOSIXSeconds

toUTCTime :: Time -> UTCTime
toUTCTime (Time t) = posixSecondsToUTCTime $ fromIntegral t

addTime :: Time -> TimeSpan -> Time
addTime (Time t) (TimeSpan d) = Time (t + d)

subTime :: Time -> TimeSpan -> Time
subTime (Time t) (TimeSpan d) = Time (t - d)

timeDiff :: Time -> Time -> TimeSpan
timeDiff (Time e) (Time s) = TimeSpan (e - s)

negateTimeSpan :: TimeSpan -> TimeSpan
negateTimeSpan TimeSpan{..} = TimeSpan (- timeSpanInSeconds)

addTimeSpan :: TimeSpan -> TimeSpan -> TimeSpan
addTimeSpan t1 t2 = TimeSpan $ timeSpanInSeconds t1 + timeSpanInSeconds t2

now :: IO Time
now = fromUTCTime <$> getCurrentTime

timeAgo :: TimeSpan -> IO Time
timeAgo d = (`subTime` d) <$> now

minutes, hours, days, weeks :: Int -> TimeSpan
minutes = TimeSpan . (60 *)
hours = TimeSpan . (3600 *)
days = TimeSpan . (86400 *)
weeks = TimeSpan . (604800 *)

minute, hour, day, week :: TimeSpan
minute = minutes 1
hour = hours 1
day = days 1
week = weeks 1

trLength :: TimeRange -> TimeSpan
trLength (TimeRange s e) = timeDiff e s

trLast :: TimeSpan -> IO TimeRange
trLast d = do
  t <- now
  return TimeRange
    { start = t `subTime` d
    , end = t
    }

-- Some generally useful pretty printing functions -----------------------------

ppUTCTime :: FormatTime t => t -> Text
ppUTCTime t = ppDate t <> " at " <> ppTime t

ppDate :: FormatTime t => t -> Text
ppDate = Text.pack . formatTime defaultTimeLocale "%F"  -- e.g. 2017-07-20

ppTime :: FormatTime t => t -> Text
ppTime = Text.pack . formatTime defaultTimeLocale "%T UTC"  -- e.g. 14:18:45

data PPTimeSpanGranularity = Day | Hour | Minute | Second

secondsInOneDay :: Int
secondsInOneDay = 3600 * 24

toDays, toHours, toMinutes, toSeconds :: TimeSpan -> Int
toDays TimeSpan{..}  = timeSpanInSeconds `div` secondsInOneDay
toHours TimeSpan{..}  = (timeSpanInSeconds `mod` secondsInOneDay) `div` 3600
toMinutes TimeSpan{..} = (timeSpanInSeconds `mod` 3600) `div` 60
toSeconds TimeSpan{..} = timeSpanInSeconds `mod` 60

plural :: Int -> Text -> Maybe Text
plural 0 _       = Nothing
plural 1 unitStr = Just $ "1 " <> unitStr
plural n unitStr = Just $ showt n <> " " <> unitStr <> "s"

joinTimeSectionString :: Text -> [Maybe Text] -> Text
joinTimeSectionString unitStr maybeSecs = case catMaybes maybeSecs of
  [] -> "0" <> unitStr <> "s"
  s  -> Text.intercalate ", " s

ppTimeSpanWithGranularity :: PPTimeSpanGranularity -> TimeSpan -> Text
ppTimeSpanWithGranularity g ts | timeSpanInSeconds ts < 0 =
  "-" <> ppTimeSpanWithGranularity g (negateTimeSpan ts)
ppTimeSpanWithGranularity Day ts
  | timeSpanInSeconds ts < secondsInOneDay = ppTimeSpanWithGranularity Hour ts
  | otherwise = joinTimeSectionString "day" [ plural (toDays ts) "day" ]
ppTimeSpanWithGranularity Hour ts
  | timeSpanInSeconds ts < 3600 = ppTimeSpanWithGranularity Minute ts
  | otherwise = joinTimeSectionString "hour"
    [ plural (toDays ts) "day"
    , plural (toHours ts) "hour"
    ]
ppTimeSpanWithGranularity Minute ts
  | timeSpanInSeconds ts < 60 = ppTimeSpanWithGranularity Second ts
  | otherwise = joinTimeSectionString "minute"
    [ plural (toDays ts) "day"
    , plural (toHours ts) "hour"
    , plural (toMinutes ts) "minute"
    ]
ppTimeSpanWithGranularity Second ts =
  joinTimeSectionString "second"
    [ plural (toDays ts) "day"
    , plural (toHours ts) "hour"
    , plural (toMinutes ts) "minute"
    , plural (toSeconds ts) "second"
    ]

ppTimeSpan :: TimeSpan -> Text
ppTimeSpan = ppTimeSpanWithGranularity Second
