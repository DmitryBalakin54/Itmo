module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay cur = case cur of
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday


afterDays :: Natural -> Day -> Day
afterDays n = afterDays_ (mod n 7)

afterDays_ :: Natural -> Day -> Day
afterDays_ 0 day = day
afterDays_ n day = afterDays (n - 1) $ nextDay day

isWeekend :: Day -> Bool
isWeekend Sunday   = True
isWeekend Saturday = True
isWeekend _        = False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day    = (+) 1 $ daysToParty $ nextDay day
