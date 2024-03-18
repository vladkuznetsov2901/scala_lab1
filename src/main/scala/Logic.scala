
/**
 * Определите в объекте ниже функции not, and, or, nand, nor, xor, impl и equ,
 * которые возвращают true или false в зависимости от результата их соответствующих операций.
 *
 * Например and(A, B) истинно тогда и только тогда, когда и A, и B истинны.
 *
 * (!) При реализации функций запрещено использовать стандартные логическии операции &&, || (и т.д.),
 * а также операции сравнения на равенство или неравенство
 */
object Logic:

    /** Логическое отрицание */
    def not(a: Boolean): Boolean = !a

    /** Логическое И */
    def and(a: Boolean, b: Boolean): Boolean = a && b

    /** Логическое ИЛИ */
    def or(a: Boolean, b: Boolean): Boolean = !(not(a) and not(b))

    /** Логическое НЕ И */
    def nand(a: Boolean, b: Boolean): Boolean = not(a and b)

    /** Логическое исключающее ИЛИ */
    def xor(a: Boolean, b: Boolean): Boolean = (a or b) and nand(a, b)

    /** Операция импликации */
    def impl(a: Boolean, b: Boolean): Boolean = not(a) or b

    /** Логическая эквавалентность */
    def equ(a: Boolean, b: Boolean): Boolean = (a and b) or (nand(a, b) and nand(not(a), not(b)))


    /** Печатает таблицу истинности логической функции
     *
     * Напишите функцию, которая печатает таблицу истинности заданной логической функции двух переменных.
     *
     * Например:
     * {{{
     *     scala> truthTable(and)
     *     A     B     RES
     *     true  true  true
     *     true  false true
     *     false true  false
     *     false false false
     * }}}
     *
     * @param func Логическая функция
     */
    def truthTable(func: (Boolean, Boolean) => Boolean): Unit = {
        println("A\tB\tRES")
        println(s"true\ttrue\t${func(true, true)}")
        println(s"true\tfalse\t${func(true, false)}")
        println(s"false\ttrue\t${func(false, true)}")
        println(s"false\tfalse\t${func(false, false)}")
    }

    /** Неявное преобразование в тип Logic
     *
     * Допишите неявное преобразование типа данных так, чтобы с учетом дополненного ниже класса Logic
     * вы могли пользоваться логическими функциями, объявленными выше, как логическими операторами.
     *
     * Например:
     * {{{
     *     import Logic.given
     *     val (a, b) = (true, false)
     *     print(a and (a or not(b)))
     * }}}
     */
    given Conversion[Boolean, Logic] = Logic(_)

end Logic


class Logic(val a: Boolean):

    infix def and(b: Logic): Boolean = a && b.a
    infix def or(b: Logic): Boolean = a || b.a
    infix def nand(b: Logic): Boolean = !(a && b.a)
    infix def xor(b: Logic): Boolean = (a || b.a) && !(a && b.a)
    infix def impl(b: Logic): Boolean = !a || b.a
    infix def equ(b: Logic): Boolean = (a && b.a) || (!(a && b.a) && !(!a && !b.a))

end Logic
