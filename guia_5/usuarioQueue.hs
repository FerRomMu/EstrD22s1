import Queue

--O(n) Queue
--O(n²) Queue2
lengthQ :: Queue a -> Int
lengthQ q =
  if(isEmptyQ q)
    then 0
    else 1 + lengthQ (dequeue q)

--O(n) Queue
--O(n²) Queue2
queueToList :: Queue a -> [a]
queueToList q =
  if (isEmptyQ q)
    then []
    else firstQ q : queueToList (dequeue q)

--O(n²) Queue
--O(n²) Queue2
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 =
  if(isEmptyQ q1)
    then q2
    else queue (firstQ q1) (unionQ (dequeue q1) q2)

queue1 = queue 2 (queue 3 (queue 4 (queue 5 ( emptyQ))))
queue2 = queue 343 (queue 2 (queue 2 (emptyQ)))



