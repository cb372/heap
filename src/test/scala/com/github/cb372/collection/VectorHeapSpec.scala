package com.github.cb372.collection

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import util.Random

class VectorHeapSpec extends FunSpec with ShouldMatchers {

  describe("a heap") {

    describe("when empty") {
      val emptyHeap = VectorHeap[Int, String]()

      it("should be empty") {
        emptyHeap.isEmpty should be(true)
      }

      it("should have size 0") {
        emptyHeap should have size(0)
      }

      it("should support insert") {
        emptyHeap.insert(1, "first element") should have size(1)
      }

      it("should have no minimum element") {
        val (minimum, newHeap) = emptyHeap.extractMin
        minimum should be(None)
        newHeap should equal(emptyHeap)
      }

      it("should return the unchanged heap when decreaseKey is called") {
        emptyHeap.decreaseKey("not present", 5) should equal(emptyHeap)
      }
    
    }

    describe("when not empty") {
      val entries = Random.shuffle(1 to 1000).map(key => (key, 1000 - key))
      val heap = VectorHeap[Int, Int](entries: _*)

      it("should not be empty") {
        heap.isEmpty should be(false)
      }

      it("should have correct size") {
        heap should have size(1000)
      }

      it("should support insert") {
        heap.insert(1, 99999) should have size(1001)
      }

      it("should have a minimum element") {
        var (entry, newHeap) = heap.extractMin
        entry should equal(Some(Entry(1, 999)))
        newHeap should have size(999)
      }

    }

    describe("duplicate keys") {
      val heap = {
        var h = VectorHeap[Int, Int]((1, 10))
        for (i <- 20 to 29) {
          h = h.insert(2, i)
        }
        h.insert(3, 30)
      } 

      it("should return values with the same key in arbitrary order") {
        val (firstEntry, firstHeap) = heap.extractMin
        firstEntry should equal(Some(Entry(1, 10)))
        var latestHeap = firstHeap
        for (i <- 20 to 29) {
          var (entry, nextHeap) = latestHeap.extractMin
          entry.get.key should be(2)
          entry.get.value should (be >= 20 and be <= 29)
          latestHeap = nextHeap
        }
        val (finalEntry, finalHeap) = latestHeap.extractMin
        finalEntry should equal(Some(Entry(3, 30)))
        finalHeap should have size(0)
      }
    }

    describe("duplicate values") {
      val heap = VectorHeap[Int, Int]((10, 1), (20, 2))

      it("should return the unchanged heap if an existing value is inserted again") {
        heap.insert(1, 1) should equal(heap)
      }
    }

    describe("extractMin") {
      val entries = Random.shuffle(1 to 1000).map(key => (key, 1000 - key))
      val heap = VectorHeap[Int, Int](entries: _*)

      it("should return values in increasing key order") {
        var latestHeap = heap
        for (i <- 1 to 1000) {
          var (entry, newHeap) = latestHeap.extractMin
          entry should equal(Some(Entry(i, 1000 - i)))
          latestHeap = newHeap
        }
      }
    }

    describe("decreaseKey") {
      val entries = (1 to 3).map(v => (v * 10, v))
      val heap = VectorHeap[Int, Int](entries: _*)

      it("should decrease the key of the given value") {
        val afterDecrease = heap.decreaseKey(2, 5)
        val (minimum, _) = afterDecrease.extractMin 
        minimum should be(Some(Entry(5, 2)))
      }

      it("should return the heap unchanged if the given value is not in the heap") {
        heap.decreaseKey(9999, 5) should equal(heap)
      }
    }

    describe("custom ordering") {
      val ordering = implicitly[Ordering[Int]].reverse

      it("should support a custom ordering for keys") {
        val entries = (1 to 3).map(v => (v * 10, v))
        val heap = VectorHeap[Int, Int](entries: _*)(ordering)
        val (min1, heap1) = heap.extractMin
        val (min2, heap2) = heap1.extractMin
        val (min3, _) = heap2.extractMin
        min1 should be(Some(Entry(30, 3)))
        min2 should be(Some(Entry(20, 2)))
        min3 should be(Some(Entry(10, 1)))
      }

    }

    describe("type") {
      it("should be covariant") {
        // TODO
      }
    }

  }

}
