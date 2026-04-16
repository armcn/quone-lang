# Dataframe manipulation with dplyr

library dplyr


import desc : a -> a
import n : a -> Integer
import paste : Character -> Character -> Character


type alias Students <-
    dataframe
        { name : Vector Character
        , score : Vector Double
        , dept : Vector Character
        }


students : Students
students <-
    dataframe
        { name = ["Alice", "Bob", "Carol", "Dan", "Eve"]
        , score = [92.0, 78.0, 85.0, 64.0, 91.0]
        , dept = ["math", "cs", "math", "cs", "math"]
        }


names_and_scores <-
    students
    |> select name score

high_scorers <-
    students
    |> filter (score >= 80.0)

with_grade <-
    students
    |> mutate { grade = score / 100.0, pass = score >= 70.0 }

dept_stats <-
    students
    |> group_by dept
    |> summarize { avg_score = mean score, count = n () }

result <-
    students
    |> filter (score > 70.0)
    |> mutate { pct = score / 100.0 }
    |> arrange (desc score)

