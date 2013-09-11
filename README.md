An immutable Heap (a.k.a. Priority Queue) implementation.

Benefits over `scala.collection.mutable.PriorityQueue`
-----

* Supports `decreaseKey` operation for efficiently decreasing the key associated with a given value. Useful in e.g. Dijsktra's shortest path algorithm, Prim's minimum spanning tree algorithm.

* `extractMin` returns an `Option`. Specifically, in the case where the heap is empty, it returns `None` rather than throwing an exception.

* Immutable. (Uses a `Vector` and a `Map` to hold its internal state.)

Performance
-----

extractMin = O(log(n))
insert = O(log(n))
decreaseKey = O(log(n))
