module Everything where
import Control.Monad
import Control.Monad.State
import Data.Random
import Data.Random.Extras
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (get)
import System.Random

start :: IO ()
start = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
  return w # set title "Tester"

  buttons <- nextBtn
  getBody w #+ map element buttons

nextBtn :: UI [Element]
nextBtn = do
  btn <- UI.button #+ [string "Next"]
  view <- UI.p #+ [element btn]

  output <- UI.p #+ [string "Press Next"]

  on UI.click btn $ \_ -> do
    str <- getNext
    newOutput <- UI.p #+ [string str]
    element output # set children [newOutput]

  return [output, view]

getNext :: UI String
getNext = pick [sumOfGeoSeries, sumOfInfiniteGeoSeries]

pick :: [UI String] -> UI String
pick list = do
  gen <- liftIO newStdGen
  let [I n] = genRandomNums [((0, length list - 1), I)] gen
  list !! n

sumOfGeoSeries :: UI String
sumOfGeoSeries = do
  gen <- liftIO newStdGen
  let [I n, I a, D r] = genRandomNums [((1, 10), I), ((0, 25), I), ((1, 500), (\x -> D $ fromIntegral x/100))] gen
  return ("Find the sum of the first " ++ show n ++ " numbers in the geometric series if a1 = " ++ show a ++ " and r = " ++ show r)

sumOfInfiniteGeoSeries :: UI String
sumOfInfiniteGeoSeries = do
  gen <- liftIO newStdGen
  let [I a, D r] = genRandomNums [((1, 25), I), ((-9, 9), (\x -> D $ fromIntegral x/10))] gen
  return ("Find the sum of the infinite geometric series if a1 = " ++ show a ++ " and r = " ++ show r)

data Number = I Int | D Double deriving Show

genRandomNums :: [((Int, Int), (Int -> Number))] -> StdGen -> [Number]
genRandomNums list = evalState (mapM (\(range, f) -> do
                                     n <- genRandom range
                                     return $ f n) list)

genRandom :: (Random a) => (a, a) -> State StdGen a
genRandom range = do
  gen <- get
  let (a, g) = randomR range gen
  put g
  return a