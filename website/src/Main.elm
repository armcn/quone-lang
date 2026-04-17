module Main exposing (main)

{-| Landing page for the Quone programming language.

    Inspired by elm-lang.org for structure and roc-lang.org for the playful,
    interactive "click a line to see what it compiles to" feel.

-}

import Browser
import Html exposing (Html, a, button, cite, div, footer, h1, h2, h3, h4, li, node, p, section, span, text, ul)
import Html.Attributes as A
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { heroTab : HeroTab
    , showHeroR : Bool
    , example : ExampleId
    , selectedChunk : Int
    , hoveredChunk : Maybe Int
    }


type HeroTab
    = TabDplyr
    | TabRmse
    | TabTypes
    | TabRepl


type ExampleId
    = ExDplyr
    | ExRmse


init : Model
init =
    { heroTab = TabDplyr
    , showHeroR = False
    , example = ExDplyr
    , selectedChunk = 0
    , hoveredChunk = Nothing
    }



-- UPDATE


type Msg
    = SelectHeroTab HeroTab
    | ToggleHeroR
    | SelectExample ExampleId
    | SelectChunk Int
    | HoverChunk (Maybe Int)


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectHeroTab tab ->
            { model | heroTab = tab, showHeroR = False }

        ToggleHeroR ->
            { model | showHeroR = not model.showHeroR }

        SelectExample ex ->
            { model | example = ex, selectedChunk = 0, hoveredChunk = Nothing }

        SelectChunk i ->
            { model | selectedChunk = i }

        HoverChunk h ->
            { model | hoveredChunk = h }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNav
        , viewHero model
        , viewFeatures
        , viewExplorer model
        , viewQuote
        , viewInstall
        , viewFooter
        ]



-- NAV


viewNav : Html Msg
viewNav =
    node "nav"
        [ A.class "nav" ]
        [ div [ A.class "container nav-inner" ]
            [ a [ A.href "#top", A.class "logo" ]
                [ span [ A.class "logo-mark" ] [ text "Q" ]
                , text "quone"
                ]
            , div [ A.class "nav-links" ]
                [ a [ A.href "#features", A.class "hide-mobile" ] [ text "Features" ]
                , a [ A.href "#playground", A.class "hide-mobile" ] [ text "Playground" ]
                , a [ A.href "#install" ] [ text "Install" ]
                , a
                    [ A.href "https://github.com/armcn/quone-lang"
                    , A.class "github"
                    , A.target "_blank"
                    , A.rel "noopener"
                    ]
                    [ githubIcon, text "GitHub" ]
                ]
            ]
        ]


githubIcon : Html msg
githubIcon =
    node "svg"
        [ A.attribute "width" "16"
        , A.attribute "height" "16"
        , A.attribute "viewBox" "0 0 24 24"
        , A.attribute "fill" "currentColor"
        ]
        [ node "path"
            [ A.attribute "d" "M12 .5C5.73.5.67 5.57.67 11.86c0 5.01 3.24 9.26 7.74 10.76.57.1.78-.25.78-.55v-2c-3.15.69-3.81-1.36-3.81-1.36-.52-1.31-1.26-1.66-1.26-1.66-1.03-.7.08-.69.08-.69 1.13.08 1.73 1.16 1.73 1.16 1.01 1.74 2.66 1.24 3.31.95.1-.74.4-1.24.72-1.53-2.52-.29-5.17-1.26-5.17-5.6 0-1.24.44-2.25 1.16-3.04-.12-.29-.5-1.44.11-3.01 0 0 .94-.3 3.1 1.16.9-.25 1.87-.38 2.83-.38.96 0 1.93.13 2.83.38 2.16-1.46 3.1-1.16 3.1-1.16.61 1.57.23 2.72.11 3.01.72.79 1.16 1.8 1.16 3.04 0 4.35-2.66 5.3-5.19 5.58.41.35.78 1.05.78 2.11v3.13c0 .3.21.66.79.55 4.5-1.5 7.73-5.75 7.73-10.76C23.33 5.57 18.27.5 12 .5z" ]
            []
        ]



-- HERO


viewHero : Model -> Html Msg
viewHero model =
    section
        [ A.id "top", A.class "hero" ]
        [ div [ A.class "container" ]
            [ div [ A.class "eyebrow" ] [ text "Statically typed · compiles to R" ]
            , h1 [ A.class "hero-title" ]
                [ text "A small "
                , span [ A.class "accent" ] [ text "typed" ]
                , text " language for the "
                , span [ A.class "underline" ] [ text "R ecosystem" ]
                , text "."
                ]
            , p [ A.class "hero-sub" ]
                [ text "Quone is a small, statically typed functional language in the ML family that compiles to idiomatic R. You get Hindley-Milner type inference, algebraic data types and a friendly REPL — plus dplyr baked directly into the language, checked by the type system like everything else."
                ]
            , div [ A.class "hero-cta" ]
                [ a [ A.class "btn btn-primary", A.href "#install" ]
                    [ text "Install Quone", span [ A.class "arrow" ] [ text "→" ] ]
                , a [ A.class "btn btn-ghost", A.href "#playground" ]
                    [ text "Try the playground" ]
                ]
            , viewHeroCard model
            ]
        ]



-- HERO CODE CARD


viewHeroCard : Model -> Html Msg
viewHeroCard model =
    let
        sample =
            heroSampleFor model.heroTab model.showHeroR

        toggleClass =
            if model.showHeroR then
                "output-toggle on"

            else
                "output-toggle"
    in
    div [ A.class "code-wrap" ]
        [ div [ A.class "code-card" ]
            [ div [ A.class "code-tabs" ]
                [ div [ A.class "code-tabs-left" ]
                    [ heroTabButton model TabDplyr "dplyr.q"
                    , heroTabButton model TabRmse "rmse.q"
                    , heroTabButton model TabTypes "types.q"
                    , heroTabButton model TabRepl "repl"
                    ]
                , button
                    [ A.class toggleClass
                    , onClick ToggleHeroR
                    , A.disabled (model.heroTab == TabRepl)
                    , A.title "Toggle between Quone source and the R code it compiles to"
                    ]
                    [ span [ A.class "dot" ] []
                    , text
                        (if model.showHeroR then
                            "Showing compiled R"

                         else
                            "Show compiled R"
                        )
                    ]
                ]
            , div [ A.class "code-body" ]
                (List.map viewLine sample)
            ]
        ]


heroTabButton : Model -> HeroTab -> String -> Html Msg
heroTabButton model tab label =
    let
        cls =
            if model.heroTab == tab then
                "code-tab active"

            else
                "code-tab"
    in
    button [ A.class cls, onClick (SelectHeroTab tab) ] [ text label ]


viewLine : List Token -> Html msg
viewLine tokens =
    let
        content =
            case tokens of
                [] ->
                    [ text "\u{00A0}" ]

                _ ->
                    List.map viewToken tokens
    in
    span [ A.class "code-line" ] content


viewToken : Token -> Html msg
viewToken tok =
    case tok of
        Plain s ->
            text s

        Comment s ->
            span [ A.class "tok-comment" ] [ text s ]

        Keyword s ->
            span [ A.class "tok-keyword" ] [ text s ]

        Ty s ->
            span [ A.class "tok-type" ] [ text s ]

        Str s ->
            span [ A.class "tok-string" ] [ text s ]

        Num s ->
            span [ A.class "tok-num" ] [ text s ]

        Op s ->
            span [ A.class "tok-op" ] [ text s ]

        Fn s ->
            span [ A.class "tok-fn" ] [ text s ]



-- TOKEN TYPES


type Token
    = Plain String
    | Comment String
    | Keyword String
    | Ty String
    | Str String
    | Num String
    | Op String
    | Fn String


heroSampleFor : HeroTab -> Bool -> List (List Token)
heroSampleFor tab showR =
    case tab of
        TabDplyr ->
            if showR then
                dplyrR

            else
                dplyrQ

        TabRmse ->
            if showR then
                rmseR

            else
                rmseQ

        TabTypes ->
            if showR then
                typesR

            else
                typesQ

        TabRepl ->
            replSample


dplyrQ : List (List Token)
dplyrQ =
    [ [ Comment "-- Type-checked dplyr pipelines" ]
    , []
    , [ Keyword "library", Plain " dplyr" ]
    , []
    , [ Keyword "type alias", Plain " ", Ty "Students", Plain " ", Op "<-" ]
    , [ Plain "    ", Keyword "dataframe" ]
    , [ Plain "        { name  : ", Ty "Vector", Plain " ", Ty "Character" ]
    , [ Plain "        , score : ", Ty "Vector", Plain " ", Ty "Double" ]
    , [ Plain "        , dept  : ", Ty "Vector", Plain " ", Ty "Character" ]
    , [ Plain "        }" ]
    , []
    , [ Plain "result ", Op "<-" ]
    , [ Plain "    students" ]
    , [ Plain "        ", Op "|>", Plain " ", Fn "filter", Plain " (score ", Op ">", Plain " ", Num "70.0", Plain ")" ]
    , [ Plain "        ", Op "|>", Plain " ", Fn "mutate", Plain " { pct = score ", Op "/", Plain " ", Num "100.0", Plain " }" ]
    , [ Plain "        ", Op "|>", Plain " ", Fn "arrange", Plain " (desc score)" ]
    ]


dplyrR : List (List Token)
dplyrR =
    [ [ Fn "library", Plain "(dplyr)" ]
    , []
    , [ Plain "students ", Op "<-", Plain " ", Fn "data.frame", Plain "(" ]
    , [ Plain "  name = ", Fn "c", Plain "(", Str "\"Alice\"", Plain ", ", Str "\"Bob\"", Plain ", ", Str "\"Carol\"", Plain ", ", Str "\"Dan\"", Plain ", ", Str "\"Eve\"", Plain ")," ]
    , [ Plain "  score = ", Fn "c", Plain "(", Num "92.0", Plain ", ", Num "78.0", Plain ", ", Num "85.0", Plain ", ", Num "64.0", Plain ", ", Num "91.0", Plain ")," ]
    , [ Plain "  dept = ", Fn "c", Plain "(", Str "\"math\"", Plain ", ", Str "\"cs\"", Plain ", ", Str "\"math\"", Plain ", ", Str "\"cs\"", Plain ", ", Str "\"math\"", Plain ")" ]
    , [ Plain ")" ]
    , []
    , [ Plain "result ", Op "<-", Plain " students ", Op "|>" ]
    , [ Plain "  ", Fn "dplyr::filter", Plain "(score ", Op ">", Plain " ", Num "70.0", Plain ") ", Op "|>" ]
    , [ Plain "  ", Fn "dplyr::mutate", Plain "(pct = score ", Op "/", Plain " ", Num "100.0", Plain ") ", Op "|>" ]
    , [ Plain "  ", Fn "dplyr::arrange", Plain "(", Fn "desc", Plain "(score))" ]
    ]


rmseQ : List (List Token)
rmseQ =
    [ [ Comment "-- Root mean square error, auto-vectorised" ]
    , []
    , [ Fn "rmse", Plain " : ", Ty "Vector", Plain " ", Ty "Double", Plain " ", Op "->", Plain " ", Ty "Vector", Plain " ", Ty "Double", Plain " ", Op "->", Plain " ", Ty "Double" ]
    , [ Fn "rmse", Plain " predicted actual ", Op "<-" ]
    , [ Plain "    ", Fn "map2", Plain " (", Op "\\", Plain "p a ", Op "->", Plain " p ", Op "-", Plain " a) predicted actual" ]
    , [ Plain "        ", Op "|>", Plain " ", Fn "map", Plain " (", Op "\\", Plain "e ", Op "->", Plain " e ", Op "*", Plain " e)" ]
    , [ Plain "        ", Op "|>", Plain " ", Fn "mean" ]
    , [ Plain "        ", Op "|>", Plain " ", Fn "sqrt" ]
    ]


rmseR : List (List Token)
rmseR =
    [ [ Comment "#' @param predicted [Vector Double]" ]
    , [ Comment "#' @param actual [Vector Double]" ]
    , [ Comment "#' @return [Double]" ]
    , [ Plain "rmse ", Op "<-", Plain " ", Keyword "function", Plain "(predicted, actual) {" ]
    , [ Plain "  (predicted ", Op "-", Plain " actual) ", Op "|>" ]
    , [ Plain "    (", Op "\\", Plain "(.x) .x ", Op "*", Plain " .x)() ", Op "|>" ]
    , [ Plain "    ", Fn "mean", Plain "() ", Op "|>" ]
    , [ Plain "    ", Fn "sqrt", Plain "()" ]
    , [ Plain "}" ]
    ]


typesQ : List (List Token)
typesQ =
    [ [ Comment "-- ADTs & Hindley-Milner inference" ]
    , []
    , [ Keyword "type", Plain " ", Ty "Shape" ]
    , [ Plain "    ", Op "=", Plain " ", Fn "Circle", Plain " ", Ty "Double" ]
    , [ Plain "    ", Op "|", Plain " ", Fn "Rect", Plain "   ", Ty "Double", Plain " ", Ty "Double" ]
    , []
    , [ Fn "area", Plain " ", Op "<-", Plain " ", Op "\\", Plain "shape ", Op "->" ]
    , [ Plain "    ", Keyword "case", Plain " shape ", Keyword "of" ]
    , [ Plain "        ", Fn "Circle", Plain " r ", Op "->", Plain " ", Num "3.14159", Plain " ", Op "*", Plain " r ", Op "*", Plain " r" ]
    , [ Plain "        ", Fn "Rect", Plain " w h ", Op "->", Plain " w ", Op "*", Plain " h" ]
    ]


typesR : List (List Token)
typesR =
    [ [ Plain "area ", Op "<-", Plain " ", Keyword "function", Plain "(shape) {" ]
    , [ Plain "  ", Keyword "if", Plain " (shape$tag == ", Str "\"Circle\"", Plain ") {" ]
    , [ Plain "    r ", Op "<-", Plain " shape$values[[1]]" ]
    , [ Plain "    ", Num "3.14159", Plain " ", Op "*", Plain " r ", Op "*", Plain " r" ]
    , [ Plain "  } ", Keyword "else", Plain " ", Keyword "if", Plain " (shape$tag == ", Str "\"Rect\"", Plain ") {" ]
    , [ Plain "    w ", Op "<-", Plain " shape$values[[1]]" ]
    , [ Plain "    h ", Op "<-", Plain " shape$values[[2]]" ]
    , [ Plain "    w ", Op "*", Plain " h" ]
    , [ Plain "  }" ]
    , [ Plain "}" ]
    ]


replSample : List (List Token)
replSample =
    [ [ Op "quone>", Plain " students ", Op "<-", Plain " ", Fn "dataframe", Plain " { x = [", Num "1", Plain ", ", Num "2", Plain ", ", Num "3", Plain "] }" ]
    , [ Ty "  it : dataframe { x : Vector Integer }" ]
    , []
    , [ Op "quone>", Plain " students ", Op "|>", Plain " ", Fn "mutate", Plain " { doubled = x ", Op "*", Plain " ", Num "2", Plain " }" ]
    , [ Ty "  it : dataframe { x : Vector Integer, doubled : Vector Integer }" ]
    , []
    , [ Op "quone>", Plain " students ", Op "|>", Plain " ", Fn "mutate", Plain " { oops = x ", Op "+", Plain " ", Str "\"!\"", Plain " }" ]
    , [ Comment "   Error: cannot add Integer and Character" ]
    , [ Comment "   in the expression  x + \"!\"" ]
    ]



-- FEATURES


viewFeatures : Html msg
viewFeatures =
    section [ A.id "features", A.class "section" ]
        [ div [ A.class "container" ]
            [ div [ A.class "section-eyebrow" ] [ text "What you get" ]
            , h2 [ A.class "section-title" ]
                [ text "A small language with a "
                , span [ A.class "accent" ] [ text "serious" ]
                , text " type system."
                ]
            , p [ A.class "section-lede" ]
                [ text "Quone borrows from Elm, Haskell and R. The surface syntax stays friendly, the compiler does the heavy lifting, and the generated R looks like code you would have written by hand." ]
            , div [ A.class "features" ]
                [ featureCard "τ"
                    "Hindley-Milner inference"
                    "Write without annotations. Quone infers principal types for expressions, records and dataframes, and surfaces them in your editor on hover."
                , featureCard "λ"
                    "Algebraic data types"
                    "Union types with exhaustive pattern matching. Model your domain precisely, then let the compiler find the missing branches for you."
                , featureCard "⊕"
                    "Auto-vectorisation"
                    "Arithmetic on Vector Double lowers to plain R vector ops. No map over a length-n list unless you actually want one."
                , featureCard "λ→R"
                    "Readable R output"
                    "Every program compiles to a single .R file. You can eyeball it, share it with colleagues, and read it back months later without a runtime to learn."
                , featureCard "≡"
                    "dplyr in the language"
                    "filter, select, mutate, summarize and group_by are first-class — schema-aware, composable, and checked against your dataframe's columns at compile time."
                , featureCard "∷"
                    "Editor + REPL"
                    "An LSP ships diagnostics, hover types and formatting. The REPL evaluates R under the hood with multi-line input and syntax highlighting."
                ]
            ]
        ]


featureCard : String -> String -> String -> Html msg
featureCard icon title body =
    div [ A.class "feature" ]
        [ span [ A.class "feature-icon" ] [ text icon ]
        , h3 [] [ text title ]
        , p [] [ text body ]
        ]



-- INTERACTIVE EXPLORER


viewExplorer : Model -> Html Msg
viewExplorer model =
    let
        ex =
            exampleData model.example

        active =
            model.selectedChunk

        highlighted =
            model.hoveredChunk |> Maybe.withDefault active
    in
    section [ A.id "playground", A.class "section" ]
        [ div [ A.class "container" ]
            [ div [ A.class "section-eyebrow" ] [ text "Click to compile" ]
            , h2 [ A.class "section-title" ]
                [ text "You write "
                , span [ A.class "accent" ] [ text "Quone" ]
                , text ". Your colleagues read R."
                ]
            , p [ A.class "section-lede" ]
                [ text "Every Quone program compiles to a single .R file. Pick an example, then click any highlighted group on the left to see exactly what the compiler produces — and why." ]
            , div [ A.class "explorer-wrap" ]
                [ div [ A.class "explorer-controls" ]
                    [ div [ A.class "explorer-tabs" ]
                        [ exampleTab model ExDplyr "dplyr pipeline"
                        , exampleTab model ExRmse "rmse function"
                        ]
                    , span [ A.class "explorer-hint" ]
                        [ span [ A.class "pulse" ] []
                        , text "Click any group to see how it compiles"
                        ]
                    ]
                , div [ A.class "explorer" ]
                    [ viewPane "quone" (ex.label ++ ".q") "Quone" ex.chunks highlighted False
                    , viewPane "r" (ex.label ++ ".R") "Generated R" ex.chunks highlighted True
                    ]
                , viewExplanation (chunkAt ex.chunks active)
                ]
            ]
        ]


exampleTab : Model -> ExampleId -> String -> Html Msg
exampleTab model id label =
    let
        cls =
            if model.example == id then
                "explorer-tab active"

            else
                "explorer-tab"
    in
    button [ A.class cls, onClick (SelectExample id) ] [ text label ]


viewPane : String -> String -> String -> List Chunk -> Int -> Bool -> Html Msg
viewPane kind filename badgeLabel chunks highlighted isR =
    div [ A.class ("pane " ++ kind) ]
        [ div [ A.class "pane-head" ]
            [ span [] [ text filename ]
            , span [ A.class "badge" ] [ text badgeLabel ]
            ]
        , div [ A.class "pane-body" ]
            (List.map (viewChunk highlighted isR) chunks)
        ]


viewChunk : Int -> Bool -> Chunk -> Html Msg
viewChunk highlighted isR chunk =
    let
        lines =
            if isR then
                chunk.r

            else
                chunk.quone

        isActive =
            chunk.id == highlighted

        classes =
            if isActive then
                "chunk active"

            else
                "chunk dimmed"
    in
    div
        [ A.class classes
        , onClick (SelectChunk chunk.id)
        , onMouseEnter (HoverChunk (Just chunk.id))
        , onMouseLeave (HoverChunk Nothing)
        ]
        (List.map viewChunkLine lines)


viewChunkLine : List Token -> Html msg
viewChunkLine tokens =
    let
        content =
            case tokens of
                [] ->
                    [ text "\u{00A0}" ]

                _ ->
                    List.map viewToken tokens
    in
    span [ A.class "chunk-line" ] content


viewExplanation : Chunk -> Html msg
viewExplanation chunk =
    div [ A.class "explanation" ]
        [ span [ A.class "explanation-tag" ] [ text chunk.tag ]
        , div [ A.class "explanation-body" ]
            (renderExplain chunk.explain)
        ]


{-| Render an explanation string, turning `tokens in backticks` into <code>.
-}
renderExplain : String -> List (Html msg)
renderExplain s =
    let
        parts =
            splitOn "`" s

        render idx str =
            if modBy 2 idx == 1 then
                Html.code [] [ text str ]

            else
                text str
    in
    List.indexedMap render parts


splitOn : String -> String -> List String
splitOn sep s =
    String.split sep s



-- CHUNK DATA


type alias Chunk =
    { id : Int
    , tag : String
    , quone : List (List Token)
    , r : List (List Token)
    , explain : String
    }


type alias Example =
    { id : ExampleId
    , label : String
    , chunks : List Chunk
    }


chunkAt : List Chunk -> Int -> Chunk
chunkAt chunks idx =
    case List.head (List.drop idx chunks) of
        Just c ->
            c

        Nothing ->
            case List.head chunks of
                Just c ->
                    c

                Nothing ->
                    { id = 0, tag = "—", quone = [], r = [], explain = "" }


exampleData : ExampleId -> Example
exampleData id =
    case id of
        ExDplyr ->
            dplyrExample

        ExRmse ->
            rmseExample


dplyrExample : Example
dplyrExample =
    { id = ExDplyr
    , label = "pipeline"
    , chunks =
        [ { id = 0
          , tag = "Pipe start"
          , quone =
                [ [ Plain "result ", Op "<-" ]
                , [ Plain "    students" ]
                ]
          , r =
                [ [ Plain "result ", Op "<-", Plain " students ", Op "|>" ]
                ]
          , explain = "Every Quone `|>` lowers to R's native pipe. The compiler already knows `students` is a dataframe with columns `name`, `score`, `dept`, so later stages of the pipeline get a real schema to type-check against — not just a vague `data.frame`."
          }
        , { id = 1
          , tag = "filter"
          , quone =
                [ [ Plain "        ", Op "|>", Plain " ", Fn "filter", Plain " (score ", Op ">", Plain " ", Num "70.0", Plain ")" ]
                ]
          , r =
                [ [ Plain "  ", Fn "dplyr::filter", Plain "(score ", Op ">", Plain " ", Num "70.0", Plain ") ", Op "|>" ]
                ]
          , explain = "`filter` becomes `dplyr::filter`. The predicate `score > 70.0` is type-checked against the row type of `students`, so a typo like `scroe` or comparing a `Character` to a `Double` is a compile-time error, not a mysterious warning at runtime."
          }
        , { id = 2
          , tag = "mutate"
          , quone =
                [ [ Plain "        ", Op "|>", Plain " ", Fn "mutate", Plain " { pct = score ", Op "/", Plain " ", Num "100.0", Plain " }" ]
                ]
          , r =
                [ [ Plain "  ", Fn "dplyr::mutate", Plain "(pct = score ", Op "/", Plain " ", Num "100.0", Plain ") ", Op "|>" ]
                ]
          , explain = "`mutate` takes a record of new columns. After this step the inferred type of the pipeline is `{ name, score, dept, pct : Vector Double }` — downstream stages see `pct` as a real column, so you can chain off of it safely."
          }
        , { id = 3
          , tag = "arrange"
          , quone =
                [ [ Plain "        ", Op "|>", Plain " ", Fn "arrange", Plain " (desc score)" ]
                ]
          , r =
                [ [ Plain "  ", Fn "dplyr::arrange", Plain "(", Fn "desc", Plain "(score))" ]
                ]
          , explain = "`arrange` takes any sortable column expression. `desc` is imported from the `dplyr` library binding, so it stays fully qualified in the R output and doesn't accidentally shadow anything in your global namespace."
          }
        ]
    }


rmseExample : Example
rmseExample =
    { id = ExRmse
    , label = "rmse"
    , chunks =
        [ { id = 0
          , tag = "Type signature"
          , quone =
                [ [ Fn "rmse", Plain " : ", Ty "Vector", Plain " ", Ty "Double", Plain " ", Op "->", Plain " ", Ty "Vector", Plain " ", Ty "Double", Plain " ", Op "->", Plain " ", Ty "Double" ]
                ]
          , r =
                [ [ Comment "#' @param predicted [Vector Double]" ]
                , [ Comment "#' @param actual [Vector Double]" ]
                , [ Comment "#' @return [Double]" ]
                ]
          , explain = "Type annotations become Roxygen comments in the generated R. You get first-class documentation that stays in sync with the types — if you change the signature, the Roxygen block updates automatically."
          }
        , { id = 1
          , tag = "Definition"
          , quone =
                [ [ Fn "rmse", Plain " predicted actual ", Op "<-" ]
                ]
          , r =
                [ [ Plain "rmse ", Op "<-", Plain " ", Keyword "function", Plain "(predicted, actual) {" ]
                ]
          , explain = "A curried-looking Quone definition compiles to a plain R function with positional arguments. Under the hood Quone *is* curried, but the emitted R stays idiomatic — no partial-application gymnastics unless you ask for them."
          }
        , { id = 2
          , tag = "Vectorised subtract"
          , quone =
                [ [ Plain "    ", Fn "map2", Plain " (", Op "\\", Plain "p a ", Op "->", Plain " p ", Op "-", Plain " a) predicted actual" ]
                ]
          , r =
                [ [ Plain "  (predicted ", Op "-", Plain " actual) ", Op "|>" ]
                ]
          , explain = "The compiler spots that the lambda is just pointwise subtraction on two `Vector Double`s and lowers the whole thing to R's native vector subtraction. No `map2`, no allocation per element — the output is what you'd have written by hand."
          }
        , { id = 3
          , tag = "Generic map"
          , quone =
                [ [ Plain "        ", Op "|>", Plain " ", Fn "map", Plain " (", Op "\\", Plain "e ", Op "->", Plain " e ", Op "*", Plain " e)" ]
                ]
          , r =
                [ [ Plain "    (", Op "\\", Plain "(.x) .x ", Op "*", Plain " .x)() ", Op "|>" ]
                ]
          , explain = "When a `map` can't be cleanly fused into a vector op, Quone falls back to an R anonymous function. Still pure R syntax — no runtime library needed, no magic to debug."
          }
        , { id = 4
          , tag = "Aggregate"
          , quone =
                [ [ Plain "        ", Op "|>", Plain " ", Fn "mean" ]
                , [ Plain "        ", Op "|>", Plain " ", Fn "sqrt" ]
                ]
          , r =
                [ [ Plain "    ", Fn "mean", Plain "() ", Op "|>" ]
                , [ Plain "    ", Fn "sqrt", Plain "()" ]
                ]
          , explain = "Pipelines of unary calls compile one-to-one. `mean` and `sqrt` are just R's base functions, so there's nothing new for a colleague reading the output to learn."
          }
        ]
    }



-- QUOTE


viewQuote : Html msg
viewQuote =
    section [ A.class "section" ]
        [ div [ A.class "narrow" ]
            [ div [ A.class "section-eyebrow" ] [ text "About the name" ]
            , h2 [ A.class "section-title" ]
                [ span [ A.class "accent" ] [ text "quone" ]
                , text " · verb · to place a thing where it belongs"
                ]
            , p [ A.class "section-lede" ]
                [ text "S begat R. R begat Quone. The name is a nod to a running Seinfeld joke about an imaginary dictionary entry — fitting for a language that exists because someone decided to invent one." ]
            , div [ A.class "quote" ]
                [ text "\u{201C}I'm pretty sure quoning is an actual word.\u{201D}"
                , cite []
                    [ text "George Costanza · "
                    , a
                        [ A.href "https://www.youtube.com/watch?v=fzPy8kSn7o0"
                        , A.target "_blank"
                        , A.rel "noopener"
                        ]
                        [ text "The Bookstore (Seinfeld, 1998) ↗" ]
                    ]
                ]
            ]
        ]



-- INSTALL


viewInstall : Html msg
viewInstall =
    section [ A.id "install", A.class "section" ]
        [ div [ A.class "container" ]
            [ div [ A.class "install" ]
                [ div [ A.class "install-inner" ]
                    [ div [ A.class "section-eyebrow" ] [ text "Quick start" ]
                    , h2 []
                        [ text "Up and "
                        , span [ A.class "accent" ] [ text "running" ]
                        , text " in a minute."
                        ]
                    , p [] [ text "Quone is built in Haskell. Install the compiler with cabal, then drop into the REPL — or grab the R package if you'd rather stay inside RStudio." ]
                    , div [ A.class "terminal" ]
                        [ div [ A.class "terminal-head" ]
                            [ span [ A.class "dot" ] []
                            , span [ A.class "dot" ] []
                            , span [ A.class "dot" ] []
                            ]
                        , div [ A.class "terminal-body" ]
                            [ span [ A.class "code-line" ]
                                [ span [ A.class "prompt" ] [ text "$" ]
                                , span [ A.class "cmd" ] [ text "git clone https://github.com/armcn/quone-lang.git" ]
                                ]
                            , span [ A.class "code-line" ]
                                [ span [ A.class "prompt" ] [ text "$" ]
                                , span [ A.class "cmd" ] [ text "cd quone-lang && cabal install" ]
                                ]
                            , span [ A.class "code-line" ]
                                [ span [ A.class "prompt" ] [ text "$" ]
                                , span [ A.class "cmd" ] [ text "quone repl" ]
                                ]
                            , span [ A.class "out" ] [ text "  Quone REPL. Type :help for commands." ]
                            , span [ A.class "code-line" ]
                                [ span [ A.class "prompt" ] [ text "quone>" ]
                                , span [ A.class "cmd" ] [ text " 1 + 1" ]
                                ]
                            , span [ A.class "out" ] [ text "  it : Integer" ]
                            , span [ A.class "out" ] [ text "  2" ]
                            ]
                        ]
                    , p []
                        [ text "Prefer R? "
                        , a
                            [ A.href "https://github.com/armcn/quone-lang/tree/main/r-package"
                            , A.target "_blank"
                            , A.rel "noopener"
                            , A.style "color" "#ff8b7a"
                            , A.style "border-bottom" "1px solid rgba(245,158,11,0.4)"
                            ]
                            [ text "install the quone R package" ]
                        , text " and call quone::repl() from your console."
                        ]
                    ]
                ]
            ]
        ]



-- FOOTER


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ A.class "container" ]
            [ div [ A.class "footer-grid" ]
                [ div [ A.class "footer-about" ]
                    [ a [ A.href "#top", A.class "logo" ]
                        [ span [ A.class "logo-mark" ] [ text "Q" ]
                        , text "quone"
                        ]
                    , p []
                        [ text "A statically typed functional language that compiles to R. Built as an open-source experiment in ML-style ergonomics for data analysis." ]
                    ]
                , footerCol "Learn"
                    [ internal "#features" "Features"
                    , internal "#playground" "Playground"
                    , external "https://github.com/armcn/quone-lang/tree/main/examples" "Example gallery"
                    , placeholder "Tutorial"
                    , placeholder "Language guide"
                    ]
                , footerCol "Tooling"
                    [ internal "#install" "Install"
                    , external "https://github.com/armcn/quone-lang/tree/main/r-package" "R package"
                    , external "https://github.com/armcn/quone-lang/tree/main/editors" "Editor support"
                    , placeholder "Online playground"
                    , placeholder "Package registry"
                    ]
                , footerCol "Community"
                    [ external "https://github.com/armcn/quone-lang" "GitHub"
                    , external "https://github.com/armcn/quone-lang/issues" "Issues"
                    , external "https://github.com/armcn/quone-lang/discussions" "Discussions"
                    , placeholder "Discord"
                    , placeholder "Mailing list"
                    ]
                ]
            , div [ A.class "footer-bottom" ]
                [ div []
                    [ text "Quone is an experimental, AI-assisted project. Expect rough edges." ]
                , div []
                    [ text "Inspired by "
                    , a [ A.href "https://elm-lang.org/", A.target "_blank", A.rel "noopener" ] [ text "elm-lang.org" ]
                    , text " & "
                    , a [ A.href "https://www.roc-lang.org/", A.target "_blank", A.rel "noopener" ] [ text "roc-lang.org" ]
                    , text ". Built with "
                    , a [ A.href "https://elm-lang.org/", A.target "_blank", A.rel "noopener" ] [ text "Elm" ]
                    , text "."
                    ]
                ]
            ]
        ]


footerCol : String -> List (Html msg) -> Html msg
footerCol title links =
    div [ A.class "footer-col" ]
        [ h4 [] [ text title ]
        , ul [] (List.map (\l -> li [] [ l ]) links)
        ]


internal : String -> String -> Html msg
internal href label =
    a [ A.href href ] [ text label ]


external : String -> String -> Html msg
external href label =
    a [ A.href href, A.target "_blank", A.rel "noopener" ] [ text label ]


placeholder : String -> Html msg
placeholder label =
    a
        [ A.href "#"
        , A.class "placeholder"
        , A.title "Not available yet — coming soon."
        ]
        [ text label ]
