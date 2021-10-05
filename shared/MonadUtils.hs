module MonadUtils (
orError,
applyRight,
applyCollapse,
orElse,
orElseTry,
firstOf,
) where

orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing x = x

orElseTry :: Maybe a -> Maybe a -> Maybe a
orElseTry (Just x) _ = Just x
orElseTry Nothing (Just x) = Just x
orElseTry Nothing Nothing = Nothing

orError :: Maybe a -> b -> Either b a
orError (Just val) _ = Right val
orError Nothing err = Left err

firstOf :: [Maybe a] -> a -> a
firstOf [] a = a
firstOf (m:ms) a = m `orElse` firstOf ms a

applyRight :: (a -> Either c b) -> Either c a -> Either c b
applyRight f (Left l) = Left l
applyRight f (Right val) = f val

applyCollapse :: (a -> Maybe b) -> Either b a -> Maybe b
applyCollapse f (Left l) = Just l
applyCollapse f (Right r) = f r
