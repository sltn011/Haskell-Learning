counter_bad x =
    let x = x + 1 in
        let x = x + 1 in
            x

counter_good x =
    (\x -> (\x -> x) x + 1) x + 1