module MyJust
  (fromJust, fromMaybe)
where
--Precon: El dato dado no es Nothing.
--O(1)
fromJust :: Maybe a -> a
fromJust (Just x) = x

--O(1)
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe x (Just y) = y
