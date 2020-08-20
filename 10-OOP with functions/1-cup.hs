--Объект Кружка, хранящий объем в миллилитрах

cup ml = \message -> message ml --Конструктор объекта Кружка, принимающий её объем в миллилитрах

getMl aCup = aCup (\ml -> ml)

drink aCup mlDrank =
    if afterDrinking > 0
        then cup afterDrinking
        else cup 0
    where afterDrinking = (getMl aCup) - mlDrank

isCupEmpty aCup =
    getMl aCup == 0

coffeeCup = cup 300

afterFiveSips = foldl drink coffeeCup [30, 25, 30, 32, 27]
