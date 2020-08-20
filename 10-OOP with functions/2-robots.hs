-- Класс Робот - кортеж(имя, атака, здоровье)
killerBot = robot ("Killer", 25, 50)
tankRobot = robot ("Tank", 10, 150)

--Конструктор
robot (name, attack, hp) = \message -> message (name, attack, hp)

--Геттеры
robotName aRobot = aRobot (\(name, _, _) -> name)

robotAttack aRobot = aRobot (\(_, attack, _) -> attack)

robotHP aRobot = aRobot (\(_, _, hp) -> hp)

printRobot aRobot = aRobot (\(name, attack, hp) -> 
    "Name: " ++ name ++ ", Attack: " ++ (show attack) ++ ", HP: " ++ (show hp))
    

--Сеттеры
setRobotName aRobot newName =
    aRobot (\(_, attack, hp) -> robot (newName, attack, hp))

setRobotAttack aRobot newAttack =
    aRobot (\(name, _, hp) -> robot (name, newAttack, hp))

setRobotHP aRobot newHP =
    aRobot (\(name, attack, _) -> robot (name, attack, newHP))


--Методы
damage aRobot recieved =
    aRobot (\(name, attack, hp) -> robot (name, attack, hp - recieved))

fight attacker defender =
    damage defender attackPower
        where attackPower = if robotHP attacker > 10
                            then robotAttack attacker
                            else 0
    