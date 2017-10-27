module Utils where

sHead :: [a] -> Maybe a
sHead []    = Nothing
sHead (x:_) = Just x
