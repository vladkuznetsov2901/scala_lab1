import Logic.given_Conversion_Boolean_Logic

import scala.util.control.Breaks.break

@main
def main(): Unit =
  while (true) {
    println("Выберите категорию:")
    println("1. Функции IntHelper")
    println("2. Функции Lists")
    println("3. Функции Logic")
    println("4. Робот-симулятор")
    println("5. Двоичное дерево поиска")
    println("6. Закончить")


    val choice = scala.io.StdIn.readInt()

    if (choice == 1) {
      val intHelper = IntHelper
//      println(intHelper.isPrime(12))
//      println(intHelper.gcd(12, 6))
//      println(intHelper.isCoprime(12, 6))
//      println(intHelper.primeFactors(12))
//      println(intHelper.listPrimesInRange(6 to 12))
//      println(intHelper.goldbach(12))
      intHelper.descriminant(2, 10, 2)
    } else if (choice == 2) {
      val lists = Lists()

      val list1 = List(1, 2, 3, 4, 5)
      val list2 = List(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5)
      println("--------------------List1: " + list1 + " --------------------")
      println("Last Element: " + lists.last(list1))
      println("Penultimate Element: " + lists.penultimate(list1))
      println("Nth Element: " + lists.nth(2, list1))
      println("Length of List: " + lists.len(list1))
      println("Reversed List: " + lists.reverse(list1))
      println("Is Palindrome: " + lists.isPalindrome(list1))
      println("--------------------List2: " + list2 + " --------------------")

      println("Compressed List: " + lists.compress(list2))
      println("Packed List: " + lists.pack(list2))
      println("RLE Encoded List: " + lists.rleEncode(list2))
      println("Encoded Modified List: " + lists.encodeModified(list2))
      println("RLE Decoded List: " + lists.rleDecode(List((4, 1), (2, 2), (3, 3), (1, 1), (4, 5))))
      println("--------------------List1: " + list1 + " --------------------")
      println("Duplicated Elements: " + lists.duplicate(list1))
      println("Duplicated N Times: " + lists.duplicateN(3, list1))
      println("List after dropping every 3rd element: " + lists.dropN(3, list1))
      println("Split List: " + lists.split(2, list1))
      println("Sliced List: " + lists.slice(1, 4, list1))
      println("Range of Integers: " + lists.range(3, 8))
      println("Randomly Selected Elements: " + lists.randomSelect(3, list1))
      println("Lotto: " + lists.lotto(3, 10))
    } else if (choice == 3) {
      val a = Logic(true)
      val b = Logic(false)

      val result = a and (a or b)
      println(result)

      Logic.truthTable((x, y) => x or y)
    } else if (choice == 4) {
      println("Введите начальные координаты X и Y для робота:")
      val startX = scala.io.StdIn.readInt()
      val startY = scala.io.StdIn.readInt()

      val robotSimulator = new RobotSimulator(startX, startY)

      println("Введите команды для робота (например, 'RGLGR'): ")
      val commands = scala.io.StdIn.readLine()

      val finalPosition = robotSimulator.walk(commands)
      println(s"Финальное положение робота: $finalPosition")
      println(s"Финальное направление робота: ${robotSimulator.direction}")
    } else if (choice == 5) {
      val tree = Tree.makeBalanced(List(5, 3, 7, 1, 4, 6, 8))
      println("Симметричное дерево: " + tree.isSymmetric)
      println("Добавление значения в дерево: " + tree.addValue(9))
      println("Поиск значения в дереве: " + tree.find(7))
      println("Удаление значения из дерева: " + tree.remove(5))
    } else if (choice == 6) {
      break
    } else {
      println("Выбран неверный вариант.")
    }
  }


end main
