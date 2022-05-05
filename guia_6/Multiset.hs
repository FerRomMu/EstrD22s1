import Map
{--
module MultiSet
  (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS,
  multiSetToList)
where
--}
data MultiSet a = MS (Map a Int)

--O(1)
emptyMS :: MultiSet a
emptyMS = MS (emptyM)
