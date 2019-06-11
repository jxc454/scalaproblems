package jcc

import org.scalatest._

class ProblemsTest extends FunSpec with MustMatchers {
  it("last in list") {
    Problems.last(List(1, 2, 3)) must be(Some(3))
  }
  it("last in empty list") {
    Problems.last(List()) must be(None)
  }
  it("penultimate in empty list") {
    Problems.penultimate(List()) must be(None)
  }
  it("penultimate in list of 1") {
    Problems.penultimate(List(1)) must be(None)
  }
  it("penultimate in list of more than 1") {
    Problems.penultimate(List(1, 2)) must be(Some(1))
  }
  it("atIndex in empty list") {
    Problems.atIndex(Nil, 0) must be(None)
  }
  it("atIndex in full list") {
    Problems.atIndex(List(1), 0) must be(Some(1))
  }
  it("atIndex index is too large") {
    Problems.atIndex(List(1), 1) must be(None)
  }
  it("length of empty list") {
    Problems.length(Nil) must be(0)
  }
  it("length of a full list") {
    Problems.length(List(1, 2, 3)) must be (3)
  }
  it("reverses an empty list") {
    Problems.reverse(Nil) must be(Nil)
  }
  it("reverses a full list") {
    Problems.reverse(List(6, 5, 4)) must be(List(4, 5, 6))
  }
  it("isPalindrome detects a palindome") {
    Problems.isPalindrome(List(1, 2, 2, 1)) must be(true)
    Problems.isPalindrome(List(1, 2, 3, 2, 1)) must be(true)
    Problems.isPalindrome(List()) must be(true)
    Problems.isPalindrome(List(1, 2)) must be(false)
  }
  it("isPalindrome2 detects a palindome") {
    Problems.isPalindrome2(List(1, 2, 2, 1)) must be(true)
    Problems.isPalindrome2(List(1, 2, 3, 2, 1)) must be(true)
    Problems.isPalindrome2(List()) must be(true)
    Problems.isPalindrome2(List(1, 2)) must be(false)
  }
  it("isPalindrome3 detects a palindome") {
    Problems.isPalindrome3(List(1, 2, 2, 1)) must be(true)
    Problems.isPalindrome3(List(1, 2, 3, 2, 1)) must be(true)
    Problems.isPalindrome3(List()) must be(true)
    Problems.isPalindrome3(List(1, 2)) must be(false)
  }
  it("flattens flattens a List of Lists by one level") {
    Problems.flatten(List(List(1), List(2))) must be(List(1, 2))
    Problems.flatten(List(List(List(1)), List(List(2)))) must be(List(List(1), List(2)))
  }
  it("eliminates consecutive dupes in a string") {
    Problems.removeConsecutiveDupes("") must be ("")
    Problems.removeConsecutiveDupes("a") must be ("a")
    Problems.removeConsecutiveDupes("abbbbabbcc") must be ("ababc")
  }
  it("eliminates consecutive dupes in a list") {
    Problems.removeConsecutiveDupes(List()) must be(Nil)
    Problems.removeConsecutiveDupes(List(1, 2, 3)) must be(List(1, 2, 3))
    Problems.removeConsecutiveDupes(List(1, 4, 4, 7, 7, 7)) must be(List(1, 4, 7))
  }
  it("packs consecutive values into sublists") {
    Problems.consecutiveSubLists(List()) must be(List())
    Problems.consecutiveSubLists(List(1)) must be(List(List(1)))
    Problems.consecutiveSubLists(List(3, 3, 4, 4, 4)) must be(List(List(3, 3), List(4, 4, 4)))
  }

}
