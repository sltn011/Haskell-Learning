maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM p = maxVal
    where x = fst <$> p
          y = snd <$> p
          maxVal = pure max <*> x <*> y 