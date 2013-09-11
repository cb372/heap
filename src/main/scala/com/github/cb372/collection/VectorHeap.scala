package com.github.cb372.collection

import annotation.tailrec

/** An immutable heap implementation based on Vector */
class VectorHeap[K : Ordering, V] private (vector: Vector[Entry[K, V]], map: Map[V, Int]) extends Heap[K, V] {
  private[this] val keyOrdering = implicitly[Ordering[K]]

  private type State = (Vector[Entry[K,V]], Map[V, Int])
  private val state = (vector, map)
  private def updated(state: State): VectorHeap[K, V] = new VectorHeap(state._1, state._2)

  def isEmpty: Boolean = (size == 0)

  def size: Int = vector.size

  def insert(key: K, value: V): Heap[K, V] = {
    if (map contains value) {
      // All values must be unique.
      // If value already exists in heap, do nothing.
      return this
    }

    val entry = Entry(key, value)
    // Add entry to end of last level of tree (this may break heap property)
    val (insertedIndex, stateAfterAppend) = append(entry, state)

    // Bubble-up until heap property is restored
    val stateAfterBubbling = bubbleUp(insertedIndex, entry, stateAfterAppend)

    updated(stateAfterBubbling)
  }

  def extractMin: (Option[Entry[K, V]], Heap[K, V]) = {
    if (isEmpty) return (None, this)
    
    // Swap the root and the last leaf
    val stateAfterSwap = swap(0, vector.size - 1, state)

    // Remove the old root, which is now at the end of the array
    val (root, stateAfterRemove) = remove(vector.size - 1, stateAfterSwap)

    // Bubble-down the new root until heap property is restored
    val stateAfterBubbleDown = {
      val oldVector = stateAfterRemove._1
      if (!oldVector.isEmpty)
        bubbleDown(0, oldVector(0), stateAfterRemove)
      else
        stateAfterRemove
    }

    // Return the old root
    (Some(root), updated(stateAfterBubbleDown))
  }

  def decreaseKey(value: V, newKey: K): Heap[K, V] = {
    map.get(value) map { index =>
      val entry = vector(index)
      
      // Don't do anything if the key has not actually decreased
      if (keyOrdering.compare(newKey, entry.key) >= 0)
        return this

      // update the entry's key
      val updatedEntry = entry.copy(key=newKey)
      val updatedVector = vector.updated(index, updatedEntry)

      // Because key has got smaller, bubble-up the entry to its new home
      val stateAfterBubbleUp = bubbleUp(index, updatedEntry, (updatedVector, map))

      updated(stateAfterBubbleUp)
    } getOrElse {
      // Unknown value. Return the original heap
      this
    }
  }
  
  override def toString = s"VectorHeap(${vector.mkString(", ")})"

  private def append(entry: Entry[K, V], state: State): (Int, State) = {
    val (oldVector, oldMap) = state
    (oldVector.size, ((oldVector :+ entry), oldMap + (entry.value -> vector.size)))
  }

  private def remove(index: Int, state: State): (Entry[K, V], State) = {
    val (oldVector, oldMap) = state
    val removedEntry = oldVector(index)
    (removedEntry, (oldVector.patch(index, Nil, 1), oldMap - removedEntry.value))
  }

  /**
   * Keep swapping the given element with its parent until the heap property is restored,
   * i.e. parent key <= child key.
   */
  @tailrec
  private def bubbleUp(index: Int, entry: Entry[K, V], state: State): State = {
    if (hasParent(index)) {
      val parentIndex = getParentIndex(index)
      val parent = state._1(parentIndex)
      if (keyOrdering.compare(parent.key, entry.key) > 0) {
        return bubbleUp(parentIndex, entry, swap(index, parentIndex, state))
      } else {
        state
      }
    } else {
      state
    }
  }

  /**
   * Keep swapping the given element with the smaller of its children
   * until the heap property is restored, i.e. parent key <= child key.
   */
  @tailrec
  private def bubbleDown(index: Int, entry: Entry[K, V], state: State): State = {
    val (oldVector, oldMap) = state
    val childIndices = getChildIndices(index, oldVector)
    if (heapPropertyViolated(entry.key, childIndices, oldVector)) {
      val minChildIndex = childIndices.minBy(oldVector(_).key)
      val stateAfterSwap = swap(index, minChildIndex, state)
      bubbleDown(minChildIndex, entry, stateAfterSwap)
    } else {
      state
    }
  }

  private def heapPropertyViolated(parentKey: K, childIndices: Seq[Int], vector: Vector[Entry[K, V]]): Boolean = {
    val childSmallerThanParent = childIndices.find {
      case index => keyOrdering.compare(parentKey, vector(index).key) > 0
    }
    childSmallerThanParent.isDefined
  }

  private def hasParent(index: Int) = index > 0
  private def getParentIndex(index: Int) = (Math.floor((index - 1) / 2)).toInt
  private def getChildIndices(index: Int, vector: Vector[_]) = 
    List(index * 2 + 1, index * 2 + 2).filter(_ < vector.size)

  private def swap(i: Int, j: Int, state: State): State = {
    val (oldVector, oldMap) = state
    val updatedVector = oldVector.updated(i, oldVector(j)).updated(j, oldVector(i))
    val updatedMap = oldMap + (oldVector(i).value -> j) + (oldVector(j).value -> i)
    (updatedVector, updatedMap)
  }

}

object VectorHeap {

  /**
   * Create a heap containing the given entries
   */
  def apply[K : Ordering, V](entries: (K, V)*): Heap[K, V] = {
    entries.foldLeft[Heap[K, V]] (new VectorHeap(Vector[Entry[K,V]](), Map[V, Int]())) {
      case (heap, (key, value)) => heap.insert(key, value)
    }
  }

}
