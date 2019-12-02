module Today (currentDayOfMonth) where

import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime, localDay)
import Data.Time (utc, localTimeToUTC, addUTCTime, utcToLocalTime, toGregorian)
import Data.Time.Clock (nominalDay)

currentDayOfMonth = dayOfMonth <$> puzzleTime

dayOfMonth localTime = day
  where (_, _, day) = toGregorian $ localDay $ localTime

puzzleTime = addLocalTime unlockTimeDiff <$> zonedTimeToLocalTime <$> getZonedTime
  where addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc
        unlockTimeDiff = (-1 * nominalDay / 4)