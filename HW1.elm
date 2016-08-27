module HW1 exposing (..)

import String
import Html.App
import Html
import Random
import Time

digitsOfInt : Int -> List(Int)
digitsOfInt int =
  if int > 0 then
    let
      length =
        lengthOfPositiveInt int
    in
      getDigits length int
  else if int == 0 then
    [0]
  else
    []

lengthOfPositiveInt : Int -> Int
lengthOfPositiveInt int =
    String.length (toString int)

getDigits : Int -> Int -> List(Int)
getDigits length int =
    let
      list =
        List.repeat length int
    in
      List.reverse (List.indexedMap (\index int -> (int // 10^index) % 10) list)

-- 1.1.2

rooter : Int -> Int -> (Int, Int)
rooter int count =
  let
    digits =
      digitsOfInt int
  in
    case (lengthOfPositiveInt int) of
      1 ->
        (int, count)
      _ ->
        rooter (List.sum digits) (count + 1)

additivePersistence : Int -> Int
additivePersistence int =
  snd (rooter int 0)

digitalRoot : Int -> Int
digitalRoot int =
  fst (rooter int 0)

-- 1.1.3

--subsequences : List a -> List (List a)
--subsequences list =
--  case list of
--    [] ->
--      []

--    first :: rest ->
--      List.map (\a b ->  ) rest

--  let
--    length =
--      List.length list

--    lists =
--      List.map (\a -> [a]) list

--    original =
--      [list]

--    empty =
--      [[]]

--  in
--    original ++ lists ++ empty

-- 1.1.4

take : Int -> List a -> Result String (List a)
take int list =
  let
    intResult =
      if int < 0 then
        Result.Err "negative index"
      else if List.length list < int then
        Result.Err "not enough elements"
      else
        Result.Ok int

    listResult =
        Result.Ok list
  in
    Result.map2 List.take intResult listResult


-- 1.2.1

type alias Point = { x:Float, y:Float }
type alias State = ((Int, List Point), (Int, List Point))
type alias Model = State

initialModel : State
initialModel = ((0, []),(0, []))

main : Program Never
main = Html.App.program {init = init, update = update, subscriptions = subscriptions, view = view}

init : (Model, Cmd msg)
init = (initialModel, Cmd.none)

upstate : Point -> State -> State
upstate point state =
  let
    hits = fst state

    hitsCounter = fst hits

    hitsList = snd hits

    misses = snd state

    missesCounter = fst misses

    missesList = snd misses

  in
    if point == {x = 0, y = 0} then
      ((hitsCounter + 1, hitsList ++ [point]), misses)
    else
      (hits, (missesCounter + 1, missesList ++ [point]))


type Msg
  = GenPoint Time.Time
  |  NewPoint (Float, Float)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenPoint newTime ->
      (model, Random.generate NewPoint (Random.pair (Random.float -10 10) (Random.float -10 10)))

    NewPoint (x, y) ->
      let
        point =
          Point x y

        newModel =
          upstate point model
      in
        (newModel, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second GenPoint

view : Model -> Html.Html msg
view model =
  Html.div
  []
  [ Html.div []
    [Html.text "hits:"
    , Html.text (toString (fst (fst model)))]
    , Html.div []
    [ Html.text "misses:"
    , Html.text (toString (fst (snd model)))]
    , Html.div []
    [ Html.text "points:"
    , Html.text (toString (snd (snd model)))]
  ]

