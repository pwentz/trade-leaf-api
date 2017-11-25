module Utils where

sHead :: [a] -> Maybe a
sHead []    = Nothing
sHead (x:_) = Just x

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
