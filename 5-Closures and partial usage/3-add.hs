add4 a b c d =
    a + b + c + d

addXto3 x = 
    (\a b c -> add4 a b c x)

mystery = add4 5 --то же что и addXto3 5 -> нет нужды в доп функции addXto3