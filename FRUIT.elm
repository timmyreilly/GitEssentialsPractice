module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)




main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { fruitLeft : Int
    , message : String
    , purchaseFruit : Int
    , sevToShow : List String

    }


model : Model
model = Model 5 "" 0 []



-- VIEW

type alias Entry =
    { severity : String
    , value : String
    }

testList : List Entry
testList = [ Entry "A" "Other info"
           , Entry "E" "Other info"
           , Entry "C" "Other info"
           , Entry "V" "Other info"
           , Entry "A" "Other info"
           ]


stringList : List String
stringList = ["A", "B", "C", "A", "E", "A", "C"]

highSeverity : List String
highSeverity = ["A", "C"]

isHighSeverity : Entry -> Maybe Entry
isHighSeverity n =
    if List.member (.severity n) highSeverity then
        Just n
    else
        Nothing

onlyHighSev : List Entry
onlyHighSev = List.filterMap isHighSeverity testList

--onlyHighSev = List.filterMap is testList

toLi : Entry -> Html msg
toLi s =
      li[] [text ((.severity s) ++ " --- " ++ (.value s))]

toHtmlList : List Entry -> Html msg
toHtmlList strings =
    ul[ style [("list-style-type", "none")]] (List.map toLi strings)


view model =
    div []
        [ h1 [style[ ("color", "SlateBlue")]]
            [ text ("Fruit to eat: " ++ (toString model.fruitLeft)) ]
       , input [ style[("width", "200")], type_ "text", placeholder "How much fruit do you want?", onInput Purchase ] []
       , button [ onClick Increment] [text "Buy some fruit!"]
       , br [] []
       , button [ onClick Decrement] [text "I ate some fruit!"]
       , h2 [style[("color", "red")]] [ text model.message ]
       , h3 [ style[("color", "dark gray")] ] [text "These are fruits:"]
           , ul []
               [ li [style[ ("backgroundColor", "red")]] [text "Apple"]
               , li [style[ ("backgroundColor", "orange")]] [text "Orange"]
               , li [style[ ("backgroundColor", "yellow")]] [text "Banana"]
               , li [style[ ("backgroundColor", "green")]] [text "Avocado"]
               , li [style[ ("backgroundColor", "blue")]] [text "Blueberry"]
               , li [style[ ("backgroundColor", "purple")]] [text "Plum"]
               ]

        ]

checkbox : msg -> String -> Html msg
checkbox msg name =
  label
    [ style [("padding", "20px")]
    ]
    [ input [ type_ "checkbox", onClick msg ] []
    , text name
    ]




-- MESSAGE


type Msg
    = Decrement
    | Increment
    | Purchase String
 --   | EnteredSev String
    | ShowA
    | ShowC


notA n =
    if n /= "A" then
        Just n
    else
        Nothing

remove : String -> String -> Maybe String
remove a b =
    if a /= b then
        Just a
    else
        Nothing


--UPDATE

update msg model =
    case msg of
        Purchase amount ->
            {model | purchaseFruit = (Result.withDefault 0 (String.toInt amount))}

        Decrement ->
            if model.fruitLeft >= 1 then
                {model | fruitLeft = model.fruitLeft - 1, message = ""}
            else
                {model | message = "There's no fruit left to eat!"}
        Increment ->
            if model.fruitLeft + model.purchaseFruit > 5 then
                {model | fruitLeft = 5, message = "You have too much fruit!  Eat some!"}
            else
                {model | fruitLeft = model.fruitLeft + model.purchaseFruit, message = ""}
        ShowA ->
           if List.member "A" model.sevToShow then
                {model | sevToShow = List.filterMap
                    (\n ->
                        if n /= "A" then
                            Just n
                        else
                            Nothing)
                    model.sevToShow}
           else
                {model | sevToShow = List.append model.sevToShow ["A"]}
        ShowC ->
            if List.member "C" model.sevToShow then
                {model | sevToShow = List.filterMap
                    (\n ->
                        if n /= "C" then
                            Just n
                        else
                            Nothing)
                    model.sevToShow}
           else
                {model | sevToShow = List.append model.sevToShow ["C"]}

     --   EnteredSev amount->
     --       {model | fruitLeft = 0}