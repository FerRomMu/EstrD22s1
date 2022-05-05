import Map
{--
module MultiSet
  (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS,
  multiSetToList)
where
--}
data MultiSet a = MS (Map a Int)

emptyMS :: MultiSet a
emptyMS = MS (emptyM)
