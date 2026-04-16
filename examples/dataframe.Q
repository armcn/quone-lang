# Simple dataframe with vector columns

import round : Double -> Double


type alias Scores <-
    dataframe { name : Vector Character, score : Vector Double }


scores : Scores
scores <-
    dataframe { name = ["Alice", "Bob", "Carol"], score = [85.0, 92.0, 78.0] }


names : Vector Character
names <-
    scores.name


mean_score : Double
mean_score <-
    mean scores.score


total : Double
total <-
    sum scores.score


rounded_mean : Double
rounded_mean <-
    round (mean scores.score)


n : Integer
n <-
    length scores.name
