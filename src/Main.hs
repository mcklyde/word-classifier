module Main where

import NLP.Hext.NaiveBayes
import Data.Char
import qualified Data.Text.Lazy as T

---- TEST DATA ----
data Class = Positive | Negative deriving (Eq, Ord, Show)


-------------------

classifiedDocs :: IO [(String, Class)]
classifiedDocs = do
                words <- readFile "src/positivewords"
                let pos = zip (lines words) (cycle [Positive])
                negwords <- readFile "src/negativewords"
                let neg = zip (lines negwords) (cycle [Negative])
                return (pos ++ neg)




main :: IO ()
main = do
        docs <- classifiedDocs
        let teachModel = foldl (\md (sample, cl) -> teach (T.pack sample) cl md) emptyModel
        putStrLn "Enter a sentence to classify whether it is positive or a negative."
        review <- getLine
        let result = runBayes (teachModel docs) review
        putStrLn $ show result
