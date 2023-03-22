package workshop.prime

import spinal.core._
import spinal.lib._


object Prime{
  //Pure scala function which return true when the number is prime
  def apply(n : Int) =  ! ((2 until n-1) exists (n % _ == 0))

  //Should return True when the number is prime.
  def apply(n : UInt) : Bool = {
    //TODO
    val res = False
    for(value <- 0 to Integer.parseInt("1f", 16)) {
      if(Prime(value)) {
        when(n === value) {
          res :=  True
        }
      }
    }
    return res
  }
}


