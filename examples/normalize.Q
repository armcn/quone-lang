# Z-score normalization: (x - mean) / std_dev


std_dev : Vector Double -> Double
std_dev xs <-
    let
        avg <- mean xs
        n <- to_double (length xs)
        sum_sq <- map (\x -> (x - avg) * (x - avg)) xs |> sum
    in
    sqrt (sum_sq / n)


z_score : Vector Double -> Vector Double
z_score xs <-
    let
        avg <- mean xs
        sd <- std_dev xs
    in
    map (\x -> (x - avg) / sd) xs


scores : Vector Double
scores <-
    [55.0, 70.0, 85.0, 100.0, 40.0]


result : Vector Double
result <-
    z_score scores
