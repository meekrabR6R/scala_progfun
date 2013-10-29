package newton

object pract {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(74); 
  def abs(x: Double) = if (x < 0) -x else x;System.out.println("""abs: (x: Double)Double""");$skip(132); 
  
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x);System.out.println("""sqrtIter: (guess: Double, x: Double)Double""");$skip(89); 
  
  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) / x < 0.001;System.out.println("""isGoodEnough: (guess: Double, x: Double)Boolean""");$skip(73); 
  
  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2;System.out.println("""improve: (guess: Double, x: Double)Double""");$skip(46); 
    
  def sqrt(x: Double) = sqrtIter(1.0, x);System.out.println("""sqrt: (x: Double)Double""");$skip(19); val res$0 = 
  
  
  sqrt(1e60);System.out.println("""res0: Double = """ + $show(res$0));$skip(14); val res$1 = 
  sqrt(1e-60);System.out.println("""res1: Double = """ + $show(res$1))}
  
}
