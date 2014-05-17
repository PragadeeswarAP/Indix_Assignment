//Question 1
def last(input : List[Int]): Int = {
  input match{
    case Nil => throw new NoSuchElementException
    case a :: Nil => a
    case a :: b => last(b)
  }
}
//Question 2
def penultimate(input : List[Int]): Int = {
  input match{
    case Nil => throw new NoSuchElementException
    case a :: Nil => throw new NoSuchElementException
    case a :: b :: Nil => a
    case a :: b => penultimate(b)
  }
}
//Question 3
def reverse(input : List[Int]): List[Int] = {
  input match{
    case a :: Nil => List(a)
    case a :: b =>
      reverse(b) ++ List(a)
  }
}
//Question 4
def compress(input : List[Symbol]): List[Symbol] = {
  input match{
    case a :: b :: Nil =>
      if(a == b)
        List(a)
      else
        List(a,b)
    case a :: b :: c =>
      if(a == b)
        compress(List(a)++c)
      else
        List(a)++compress(List(b)++c)
  }
}
//Question 5
def duplicateN(n : Int,input : List[Symbol]): List[Symbol]={
  val nlist = (1 to n).toList
  input match{
    case a::Nil => NduplicatesOfOneSym(nlist,a)
    case a :: b => NduplicatesOfOneSym(nlist,a) ++ duplicateN(n,b)
  }
}
def NduplicatesOfOneSym(nlist : List[Int], sym :Symbol): List[Symbol]={
  nlist match{
    case a::Nil => List(sym)
    case a :: b => List(sym) ++ NduplicatesOfOneSym(b,sym)
  }
}
//Question 6
def drop(n:Int,input:List[Symbol]):List[Symbol]={
  val ntmp = n
  finddrop(n,ntmp,input)
}
def finddrop(n:Int,ntmp:Int,input:List[Symbol]): List[Symbol]={
  (input,ntmp) match{
    case (a::Nil,_)=>List(a)
    case (a::b,1) =>finddrop(n,n,b)
    case (a::b,_) =>List(a) ++ finddrop(n,ntmp-1,b)
  }
}
//Question 7
def slice(begin:Int, end:Int, input : List[Symbol]):List[Symbol] = {
  val xs = (0 to end-1).toList
  findslice(input,xs)
}
def findslice(input : List[Symbol], xs : List[Int]): List[Symbol] = {
  xs match{
    case (c::Nil) => List(input.head)
    case (c :: d) =>
      if (c >= 3)
        List(input.head) ++ findslice(input.tail,d)
      else
        findslice(input.tail,d)
  }
}
//Question 8
def listPrimesinRange(begin:Int,end:Int):List[Int] = {
  val output = (begin to end).toList
  val divisor =(2 to end/2).toList
  primecheck(output,divisor)
}
def primecheck(output : List[Int],divisor : List[Int]): List[Int]={
  divisor match{
    case Nil => output
    case a :: b =>
      val c =output.filter(x => x % a != 0 | x == a)
      primecheck(c,b)
  }
}
//Question 9
def multiplesum(limit:Int,n1:Int,n2:Int):Int = {
  val output = (0 to limit).toList
  val c = output.filter(x => x % n1 == 0 | x % n2 == 0)
  c.foldLeft(0)((x,y) => x+y)
}
//Question 10
def convert_to_english(n:Int): String = {
    val n0 = n % 10
    if (n == 0)
      ""
    else
      n0 match {
        case 0 => convert_to_english(n/10) + "zero-"
        case 1 => convert_to_english(n/10) + "one-"
        case 2 => convert_to_english(n/10) + "two-"
        case 3 => convert_to_english(n/10) + "three-"
        case 4 => convert_to_english(n/10) + "four-"
        case 5 => convert_to_english(n/10) + "five-"
        case 6 => convert_to_english(n/10) + "six-"
        case 7 => convert_to_english(n/10) + "seven-"
        case 8 => convert_to_english(n/10) + "eight-"
        case 9 => convert_to_english(n/10) + "nine-"
      }
}
//Question 11
def denom_factorial(n:Long):Long = {
  if(n <= 1)
    1
  else
    n*denom_factorial(n-1)
}
def num_lattice(current:Long,n:Long):Long = {
  if(current > n)
    current*num_lattice(current-2,n)
  else
    1
}
def powlong(value:Long,power:Long):Long = {
  if(power > 0)
    value * powlong(value,power-1)
  else
    1
}
def lattice(n:Long):Long ={
  powlong(2,n/2)*num_lattice(2*n-1,n)/denom_factorial(n/2)
}
//Question 12
def factorial(n:Int):Int = {
  if(n <= 1)
    1
  else
    n*factorial(n-1)
}
def lexo(n:Int):Int = {
  val digits:Int = 10
  val index = (0 to digits-1).toList
  findLexo(n,digits,index)
}
def findLexo(n:Int,digits:Int,index:List[Int]):Int={
  val fact = factorial(digits-1)
  var tmp = (n-1)/fact
  val index_new = dropByIndex(index,tmp,0)
  if(digits > 1)
    (index(tmp)*pow(10,digits-1)) + findLexo(n-(tmp*fact),digits-1,index_new)
  else
    index(0)
}
def dropByIndex(index:List[Int],value : Int,count:Int):List[Int]={
  index match{
    case a::Nil =>
      if(count == value)
        List()
      else
        List(a)
    case a::b =>
      if(count == value)
        dropByIndex(b,value,count+1)
      else
        List(a)++dropByIndex(b,value,count+1)
  }
}
def pow(value:Int,power:Int):Int = {
  if(power > 0)
    value * pow(value,power-1)
  else
    1
}
//Test cases
last(List(1,2,3,4,5,6,7))
penultimate(List(1,2,3,4,5,6,7))
reverse(List(1,2,3,4,5))
compress(List('a,'a,'a,'a,'b,'c,'c,'c,'d,'d,'e,'e,'e,'e))
duplicateN(3,List('a,'b,'c,'c,'d))
drop(3,List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k))
slice(3,7,List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k))
listPrimesinRange(7,31)
multiplesum(1000,3,5)
convert_to_english(125)
lattice(20)
lexo(100000)
