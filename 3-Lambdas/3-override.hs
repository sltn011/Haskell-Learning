override_v1 x =
    let x = 1 in
        let x = 2 in
            let x = 3 in
                x

override_v2 x =
    (\x -> 
        (\x -> 
            (\x -> x) 3) 2) 1