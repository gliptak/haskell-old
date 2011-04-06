module NLPWP.NGrams.Markov (
  pTransition
) where

import Data.List (tails)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

pTransition :: (Ord a, Integral n, Fractional f) =>
  M.Map [a] n -> a -> a -> f
pTransition ngramFreqs state nextState = fromMaybe 0.0 $ do
  stateFreq <- M.lookup [state] ngramFreqs
  transFreq <- M.lookup [state, nextState] ngramFreqs
  return $ (fromIntegral transFreq) / (fromIntegral stateFreq)

ngrams :: Int -> [b] -> [[b]]
ngrams n = filter ((==) n . length) . map (take n) . tails

pMarkov :: (Ord a, Integral n, Fractional f) =>
  M.Map [a] n -> [a] -> f
pMarkov ngramFreqs =
  product . map (\[s1,s2] -> pTransition ngramFreqs s1 s2) . ngrams 2
