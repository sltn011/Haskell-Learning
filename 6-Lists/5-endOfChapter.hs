--repeat n повторяет n бесконечное число раз
--напишем его аналог

--repeat создает бесконечный список, cycle создает зацикленный список!!!!

myRepeat n =
    cycle [n]

subseq list from to =
    take (to - from) (drop from list)

inFirstHalf list n =
    elem n (take (div (length list) 2) list)
