allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func a = pure func <*> a

example1 :: Int
example1 = (*) ((+) 2 4) 6

example2 :: Maybe Int
example2 = pure (*) <*> pure ((+) 2 4) <*> pure 6

beerToBuy :: Int
beerToBuy = toBuy
    where bought = [6, 12]
          drankWithFriend = [2]
          afterFriend = pure (-) <*> bought <*> drankWithFriend
          numOfGuestsExpected = [2, 3]
          eachGuestWillDrink = [3, 4]
          guestsWillDrink = pure (*) <*> numOfGuestsExpected <*> eachGuestWillDrink
          expectedOutcomes = pure (-) <*> afterFriend <*> guestsWillDrink
          onlyShortage = filter (< 0) expectedOutcomes
          toBuy =
              if length onlyShortage == 0
                  then 0
                  else (abs . minimum) onlyShortage
