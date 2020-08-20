import Data.Semigroup

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where sumProbs = sum probs
          normalizedProbs = map (\x -> x/sumProbs) probs

showEntry :: String -> Double -> String
showEntry event prob = mconcat [event, " | ", show prob, "\n"]

instance Show PTable where
    show (PTable events probs) = mconcat entries
        where entries = zipWith showEntry events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
    where combiner = (\x y -> mconcat[x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable e3 p3
                                         where e3 = combineEvents e1 e2
                                               p3 = combineProbs p1 p2

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

coin :: PTable
coin = createPTable ["Head", "Tail"] [0.5, 0.5]