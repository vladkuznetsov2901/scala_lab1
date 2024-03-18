

/** Допустимые команды, исполняемые роботом */
enum RobotCommand:
    case TurnRight, TurnLeft, Go

/** Направление движения робота */
enum Direction:
    case Up, Down, Left, Right

/** Тип данных, отражающий положение робота на сетке */
type Position = (Int, Int)


/**
 * Напишите симулятор робота.
 *
 * Испытательной лаборатории завода роботов нужна программа для проверки роботов.
 * Роботы могут выполнять три возможные команды: Поверни направо, Поверни налево, Иди вперед.
 *
 * Роботы размещаются на гипотетической бесконечной двухмерной сетке с целочисленными координатами
 * и условными направлениями север (вверх), юг (вниз), запад (лево) и восток (право).
 * Координаты увеличиваются снизу вверх и слева направо.
 *
 * При инициализации робота считать, что изначально он направлен на север (вверх).
 *
 * Вы можете дорабатывать класс добавляя поля, свойства и необходимые методы по своему усмотрению.
 * Удалять или модифицировать объявленные ниже элементы класса нельзя.
 *
 * @param startX Начальная X-координата робота
 * @param startY Начальная Y-координата робота
 */
class RobotSimulator(startX: Int, startY: Int):

    private var currentX: Int = startX
    private var currentY: Int = startY
    private var currentDirection: Direction = Direction.Up

    /** Возвращает текущее положение робота */
    def position: Position = (currentX, currentY)

    /** Возвращает текущее направление робота */
    def direction: Direction = currentDirection

    /** Исполняет команды, перемещая робота по сетке
     *
     * Метод исполняет переданные команды последовательно, изменяя при необходимости текущее положение
     * робота на сетке.
     *
     * @param commands Список команд для исполнения
     * @return Новое положение робота
     */
    def walk(commands: List[RobotCommand]): Position = {
        for (command <- commands) {
            executeCommand(command)
        }
        position
    }

    /** Исполняет команды, перемещая робота по сетке
     *
     * Метод исполняет переданные команды последовательно, изменяя при необходимости текущее положение
     * робота на сетке.
     *
     * @param commands Список команд для исполнения. Команды задаются символами строки:
     *                 R - поверхнуть направо,
     *                 L - повернуть налево,
     *                 G - двигаться вперед.
     * @return Новое положение робота
     */
    def walk(commands: String): Position = {
        val commandList = commands.toList.map(charToCommand)
        walk(commandList)
    }

    private def executeCommand(command: RobotCommand): Unit = {
        command match {
            case RobotCommand.TurnRight => turnRight()
            case RobotCommand.TurnLeft => turnLeft()
            case RobotCommand.Go => moveForward()
        }
    }

    private def turnRight(): Unit = {
        currentDirection = currentDirection match {
            case Direction.Up => Direction.Right
            case Direction.Right => Direction.Down
            case Direction.Down => Direction.Left
            case Direction.Left => Direction.Up
        }
    }

    private def turnLeft(): Unit = {
        currentDirection = currentDirection match {
            case Direction.Up => Direction.Left
            case Direction.Left => Direction.Down
            case Direction.Down => Direction.Right
            case Direction.Right => Direction.Up
        }
    }

    private def moveForward(): Unit = {
        currentDirection match {
            case Direction.Up => currentY += 1
            case Direction.Down => currentY -= 1
            case Direction.Left => currentX -= 1
            case Direction.Right => currentX += 1
        }
    }

    private def charToCommand(char: Char): RobotCommand = {
        char match {
            case 'R' => RobotCommand.TurnRight
            case 'L' => RobotCommand.TurnLeft
            case 'G' => RobotCommand.Go
            case _ => throw new IllegalArgumentException(s"Invalid command character: $char")
        }
    }

end RobotSimulator
