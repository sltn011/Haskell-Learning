allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func ma = ma >>= (\x -> return (func x))

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mfunc ma = mfunc >>= (\func -> allFmapM func ma)

allBind :: Maybe a -> (a -> Maybe b) -> Maybe b
allBind Nothing _ = Nothing
allBind (Just a) func = func a