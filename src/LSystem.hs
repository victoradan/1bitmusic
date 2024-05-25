{-# LANGUAGE TupleSections #-}

module LSystem where


import           Data.List
import           System.Random

data Rule a = Rule {lhs :: a, rhs :: a} deriving (Eq, Ord, Show)

data Grammar a = Grammar a (Rules a) deriving (Show)

type Prob = Double

data Rules a = Uni [Rule a] | Sto [(Rule a, Prob)] deriving (Eq, Ord, Show)

type Rand = Double

type ReplFun a = [[(Rule a, Prob)]] -> (a, [Rand]) -> (a, [Rand])

gen :: (Ord a) => ReplFun a -> Grammar a -> Int -> [a]
gen f (Grammar s rules) seed =
  let Sto newRules = toStoRules rules
      rands = randomRs (0.0, 1.0) (mkStdGen seed)
   in if checkProbs newRules
        then generate f newRules (s, rands)
        else error "Stochastic rule-set is malformed"

toStoRules :: (Ord a, Eq a) => Rules a -> Rules a
toStoRules (Sto rs) = Sto rs
toStoRules (Uni rs) =
  let rs' = groupBy (\r1 r2 -> lhs r1 == lhs r2) (sort rs)
   in Sto (concatMap insertProb rs')

insertProb :: [a] -> [(a, Prob)]
insertProb rules =
  let prb = 1.0 / fromIntegral (length rules)
   in map (, prb) rules

checkProbs :: (Ord a, Eq a) => [(Rule a, Prob)] -> Bool
checkProbs rs = all checkSum (groupBy sameLHS (sort rs))

checkSum :: [(Rule a, Prob)] -> Bool
checkSum rules =
  let mySum = sum (map snd rules)
      eps = 0.001
   in abs (1.0 - mySum) <= eps

sameLHS :: (Eq a) => (Rule a, Prob) -> (Rule a, Prob) -> Bool
sameLHS (r1, fl) (r2, f2) = lhs r1 == lhs r2

generate :: (Eq a) => ReplFun a -> [(Rule a, Prob)] -> (a, [Rand]) -> [a]
generate f rules xs =
  let newRules = map probDist (groupBy sameLHS rules)
      probDist rrs =
        let (rs, ps) = unzip rrs
         in zip rs (tail (scanl (+) 0 ps))
   in map fst (iterate (f newRules) xs)

data LSys a
  = N a
  | LSys a :+ LSys a
  | LSys a :. LSys a
  | Id
  deriving (Eq, Ord, Show)

replFun :: (Eq a) => ReplFun (LSys a)
replFun rules (s, rands) =
  case s of
    a :+ b ->
      let (a', rands') = replFun rules (a, rands)
          (b', rands'') = replFun rules (b, rands')
       in (a' :+ b', rands'')
    a :. b ->
      let (a', rands') = replFun rules (a, rands)
          (b', rands'') = replFun rules (b, rands')
       in (a' :. b', rands'')
    Id -> (Id, rands)
    N x -> (getNewRHS rules (N x) (head rands), tail rands)

getNewRHS :: (Eq a) => [[(Rule a, Prob)]] -> a -> Rand -> a
getNewRHS rrs ls rand =
  let loop ((r, p) : rs) = if rand <= p then rhs r else loop rs
      loop []            = error "getNewRHS anomaly"
   in case find (\((r, p) : _) -> lhs r == ls) rrs of
        Just rs -> loop rs
        Nothing -> error "No rule match"

