module Main where
import Control.Monad
import Control.Monad.State
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (get)
import System.Random

main :: IO ()
main = startGUI defaultConfig setup

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
getNext = sumOfGeoSeries

sumOfGeoSeries :: UI String
sumOfGeoSeries = do
  gen <- liftIO newStdGen
  let ((n, a, r), _) = runState genThreeRandom gen
  return ("Find the sum of the first " ++ show n ++ " numbers in the geometric series if a1 = " ++ show a ++ " and r = " ++ show r)

genThreeRandom :: State StdGen (Int, Int, Float)
genThreeRandom = do
  n <- genRandom (1, 10)
  a <- genRandom (0, 25)
  r <- genRandom (1, 500)
  return (n, a, fromInteger r/100)

genRandom :: (Random a) => (a, a) -> State StdGen a
genRandom range = do
  gen <- get
  let (a, g) = randomR range gen
  put g
  return a
 
