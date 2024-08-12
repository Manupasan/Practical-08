import scala.io.StdIn._

@main def main() =
  println("========== Question 01 ==========")
  mainMenu()
  println("========== Question 02 ==========")
  checkNumber()

def caesarCipherEncryption(word:String, shiftNum:Int) : String = {
  word.map { letter =>
    if (letter.isLetter) {
      val base = if (letter.isUpper) 'A' else 'a'
      (( letter - base + shiftNum) % 26 + base).toChar
    }
    else {
      letter
    }
  }
}

def caesarCipherDecryption(word:String, shiftNum:Int) : String = {
  word.map { letter =>
    if (letter.isLetter) {
      val base = if (letter.isUpper) 'A' else 'a'
      ((letter - base - shiftNum + 26) % 26 + base).toChar
    }
    else {
      letter
    }
  }
}


def mainMenu()=

  println("To Encrypt : Press 1\nTo Decrypt : Press 2")
  print("Select one of these: ")
  val select = readInt()
  if(select==1)
    println("Enter a text to encrypt")
    val word = readLine()
    println("Enter shift number")
    val shift = readInt()
    println(caesarCipherEncryption(word, shift))

  else
    println("Enter a text to decrypt")
    val word = readLine()
    println("Enter shift number")
    val shift = readInt()
    println(caesarCipherDecryption(word, shift))

def checkNumber()=
  println("Enter a number : ")
  val num = readInt()

  num match
    case num if isMultipleOfThree(num) && isMultipleOfFive(num) => println("Multiple of Both Three and Five")
    case num if isMultipleOfThree(num) => println("Multiple of Three")
    case num if isMultipleOfFive(num) => println("Multiple of Five")
    case _ => println("Not a Multiple of Three or Five")


def isMultipleOfThree(num: Int): Boolean =
  num % 3 == 0

def isMultipleOfFive(num: Int): Boolean =
  num % 5 == 0


