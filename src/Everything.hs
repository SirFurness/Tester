module Everything where
import Control.Monad
import Control.Monad.State
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
    gen <- liftIO newStdGen
    let str = evalState getNext gen
    newOutput <- UI.p #+ [string str]
    element output # set children [newOutput]

  return [output, view]

type Rand a = State StdGen a

getNext :: Rand String
getNext = pick [sumOfGeoSeries, sumOfInfiniteGeoSeries]

pick :: [Rand String] -> Rand String
pick list = do
  n <- genRandomR (0, length list - 1)
  list !! n

sumOfGeoSeries :: Rand String
sumOfGeoSeries = do
  n <- genRandomR (1, 10)
  a <- genRandomR (0, 25)
  r <- genRandomDecR (1, 500) (/100)
  return $ "Find the sum of the first " ++ show n ++ " numbers in the geometric series if a1 = " ++ show a ++ " and r = " ++ show r

sumOfInfiniteGeoSeries :: Rand String
sumOfInfiniteGeoSeries = do
  a <- genRandomR (1, 25)
  r <- genRandomDecR (-9, 9) (/10)
  return $ "Find the sum of the infinite geometric series if a1 = " ++ show a ++ " and r = " ++ show r

genRandomDecR :: (Int, Int) -> (Double -> Double) -> Rand Double
genRandomDecR range f = do
  n <- genRandomR range
  return $ f $ fromIntegral n

genRandomR :: (Int, Int) -> Rand Int
genRandomR range = do
  gen <- get
  let (a, g) = randomR range gen
  put g
  return a
