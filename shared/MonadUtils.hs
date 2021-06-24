module MonadUtils (
orError,
applyRight,
applyCollapse,
) where

orError :: Maybe a -> b -> Either b a
orError (Just val) _ = Right val
orError Nothing err = Left err

applyRight :: (a -> Either c b) -> Either c a -> Either c b
applyRight f (Left l) = Left l
applyRight f (Right val) = f val

applyCollapse :: (a -> Maybe b) -> Either b a -> Maybe b
applyCollapse f (Left l) = Just l
applyCollapse f (Right r) = f r
