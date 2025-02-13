module HW6.T3
  ( Config(..)
  , Cell(..)
  , CellState(..)
  , Comonad19Grid
  , simulate
  , configure
  ) where

import System.Random (StdGen, mkStdGen, random)
import Data.Grid (Grid(..), insertGrid, shifts)
import Control.Comonad (Comonad(..))
import Data.ListZipper (shift)

type Comonad19Grid = Grid Cell

createRandomGenerator :: (Int -> Int) -> Cell -> Cell
createRandomGenerator modifier (Cell state randGen) =
  let (num, _) = random randGen
      newGen = mkStdGen (modifier num)
  in Cell state newGen

createGrid :: Config -> Int -> Comonad19Grid
createGrid config seed = insertGrid initialInfectedCell grid
  where
    zeroCell = Cell Healthy (mkStdGen seed)
    
    zeroRow = shift leftShift rightShift zeroCell
    grid = Grid $ shift upShift downShift zeroRow
    
    leftShift = createRandomGenerator (+ 1)
    rightShift = createRandomGenerator (+ 2)
    upShift = fmap (createRandomGenerator (+ 3))
    downShift = fmap (createRandomGenerator (+ 4))
    
    initialInfectedCell = zeroCell { cellState = Infected (incubationPeriod config) }

data Config = Config
  { probability      :: Double
  , incubationPeriod :: Int
  , illnessDuration  :: Int
  , immunityDuration :: Int
  } deriving (Show)

verifyConfig :: Double -> Int -> Int -> Int -> Bool
verifyConfig p iDur illDur immDur = all (>= 0) [p] 
                                    && p <= 1
                                    && all (> 0) [iDur, illDur, immDur]

configure :: Double -> Int -> Int -> Int -> Maybe Config
configure p iDur illDur immDur 
  | verifyConfig p iDur illDur immDur = Just $ Config p iDur illDur immDur
  | otherwise = Nothing

data CellState = Healthy | Infected Int | Ill Int | Immune Int

instance Show CellState where
  show Healthy      = "_"
  show (Infected _) = "i"
  show (Ill _)      = "#"
  show (Immune _)   = "@"

data Cell = Cell
  { cellState :: CellState
  , cellRand  :: StdGen
  }

instance Show Cell where
  show = show . cellState

isCellDangerous :: Cell -> Bool
isCellDangerous (Cell (Infected _) _) = True
isCellDangerous (Cell (Ill _) _)      = True
isCellDangerous _                     = False

updateCellState :: Config -> Comonad19Grid -> Cell -> Cell
updateCellState config grid (Cell state randGen) = 
  case state of
    Healthy    -> contact config grid randGen
    Infected 1 -> Cell (Ill $ illnessDuration config) randGen
    Infected n -> Cell (Infected (n - 1)) randGen
    Ill 1      -> Cell (Immune $ immunityDuration config) randGen
    Ill n      -> Cell (Ill (n - 1)) randGen
    Immune 1   -> Cell Healthy randGen
    Immune n   -> Cell (Immune (n - 1)) randGen

isCellInDanger :: Comonad19Grid -> Bool
isCellInDanger grid = any (isCellDangerous . (\arrow -> extract $ arrow grid)) shifts

contact :: Config -> Comonad19Grid -> StdGen -> Cell
contact config grid randGen = 
  let (probabilityOfInfection, newRandGen) = random randGen
      isInfected = isCellInDanger grid && probabilityOfInfection <= probability config
  in if isInfected
       then Cell (Ill $ incubationPeriod config) newRandGen
       else Cell Healthy newRandGen

propagate :: Config -> Comonad19Grid -> Comonad19Grid
propagate config = extend updateCell
  where
    updateCell g = updateCellState config g (extract g)

simulate :: Config -> Int -> [Comonad19Grid]
simulate config seed = iterate (propagate config) initialGrid
  where
    initialGrid = createGrid config seed
