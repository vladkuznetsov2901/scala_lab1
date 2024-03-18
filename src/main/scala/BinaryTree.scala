
/** Описывает двоичное дерево поиска
 *
 * Реализуйте все нижеприведенные методы классов и объектов, чтобы получить
 * рабочее дерево поиска
 */
sealed abstract class Tree[+T]:

    /** Признак симметричности дерева
     *
     * Вычисляет и возвращает признак симметричности дерева. Для реализации данного метода
     * вы можете создавать вспомогательные методы, помогающие в решении.
     *
     * Например:
     * {{{
     *     scala> Node('a', Node('b'), Node('c')).isSymmetric
     *     res0: Boolean = true
     * }}}
     */
    def isSymmetric: Boolean

    /** Добавляет новый узел в двоичное дерево поиска
     *
     * @param x Значение нового узла дерева
     * @tparam U Тип значения, хранимого в новом узле
     * @return Новый корень дерева поиска, полученного после добавления узла
     */
    def addValue[U >: T: Ordering](x: U): Tree[U]

    /** Находит узел, содержащий указанное значение
     *
     * @param x Искомое значение
     * @tparam U Тип искомого значения
     * @return Узел с искомым значением или None, если такого значения в дереве нет
     */
    def find[U >: T: Ordering](x: U): Option[Node[U]]

    /** Удаляет заданный элемент из дерева поиска
     *
     * @param x Удаляемое значение
     * @tparam U Тип удаляемого значения
     * @return Новый корень полученного дерева поиска
     */
    def remove[U >: T: Ordering](x: U): Tree[U]


object Tree:
    /** Строит сбалансированное дерево поиска по списку значений узлов
     *
     * Сбалансированное двоичное дерево поиска - это дерево где для каждой его вершины высота её
     * двух поддеревьев различается не более чем на единицу.
     *
     * @param values Значения узлов дерева
     * @tparam T Тип значения в узле
     * @return Сбалансированное дерево поиска по переданным значениям
     */
    def makeBalanced[T: Ordering](values: List[T]): Tree[T] = {
        def insertBalanced(tree: Tree[T], value: T): Tree[T] = tree.addValue(value)

        values.foldLeft[Tree[T]](End)(insertBalanced)
    }


case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]:
    override def toString: String = s"T($value, $left, $right)"

    override def isSymmetric: Boolean = {
        def mirror(tree1: Tree[T], tree2: Tree[T]): Boolean = (tree1, tree2) match {
            case (End, End) => true
            case (Node(_, left1, right1), Node(_, left2, right2)) => mirror(left1, right2) && mirror(right1, left2)
            case _ => false
        }

        mirror(left, right)
    }

    def addValue[U >: T : Ordering](x: U): Tree[U] = {
        if (implicitly[Ordering[U]].compare(x, value) < 0) {
            Node(value, left.addValue(x), right)
        } else {
            Node(value, left, right.addValue(x))
        }
    }

    def find[U >: T : Ordering](x: U): Option[Node[U]] = {
        val cmp = implicitly[Ordering[U]].compare(x, value)
        if (cmp == 0) Some(this)
        else if (cmp < 0) left.find(x)
        else right.find(x)
    }

    def remove[U >: T : Ordering](x: U): Tree[U] = {
        def removeNode(node: Node[U]): Tree[U] = (node.left, node.right) match {
            case (End, End) => End
            case (End, _) => node.right
            case (_, End) => node.left
            case _ =>
                val successor = findSuccessor(node.right)
                Node(successor.value, node.left, node.right.remove(successor.value))
        }

        def findSuccessor(tree: Tree[U]): Node[U] = tree match {
            case Node(value, End, _) => Node(value)
            case Node(_, left, _) => findSuccessor(left)
            case _ => throw new NoSuchElementException("Successor not found")
        }

        if (value == x) removeNode(this)
        else if (implicitly[Ordering[U]].compare(x, value) < 0) Node(value, left.remove(x), right)
        else Node(value, left, right.remove(x))
    }


case object End extends Tree[Nothing]:
    override def toString: String = "_"

    override def isSymmetric: Boolean = true

    def addValue[U: Ordering](x: U): Tree[U] = Node(x)

    def find[U: Ordering](x: U): Option[Node[U]] = None

    def remove[U: Ordering](x: U): Tree[U] = End


object Node:
    def apply[T](value: T): Node[T] = Node(value, End, End)
