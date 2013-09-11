package com.github.cb372.collection

/** A heap entry (key-value pair) */
case class Entry[K, V](key: K, value: V)

/** A Heap, a.k.a. priority queue */
trait Heap[K, V] {
  /** Is the heap empty */
  def isEmpty: Boolean

  /** The number of elements in the heap */
  def size: Int

  /** Insert the given key-value pair into the heap */
  def insert(key: K, value: V): Heap[K, V]

  /** Remove the smallest element from the heap (sorted by key) */
  def extractMin: (Option[Entry[K, V]], Heap[K, V])

  /** 
   * Update the key for the given value.
   * If the given key is >= than the current one,
   * no work is done and the heap is left unchanged.
   */
  def decreaseKey(value: V, newKey: K): Heap[K, V]

}

object Heap {

  /**
   * Create a heap containing the given entries
   */
  def apply[K : Ordering, V](entries: (K, V)*): Heap[K, V] = VectorHeap(entries: _*)

}

