# Root Mean Square Error

rmse : Vector Double -> Vector Double -> Double
rmse predicted actual <-
    map2 (\p a -> p - a) predicted actual
    |> map (\e -> e * e)
    |> mean
    |> sqrt

predicted : Vector Double
predicted <-
    [2.5, 0.0, 2.1, 1.6]

actual : Vector Double
actual <-
    [3.0, 0.5, 2.0, 1.0]

result : Double
result <-
    rmse predicted actual

