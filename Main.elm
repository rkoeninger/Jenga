import Html exposing (Html, Attribute, text, toElement, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, keyCode)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String
import List
import Dict exposing (Dict)
import Json.Decode as JD
import Result

main = StartApp.start { model = ("", "", initEnv), view = view, update = update }

update newState oldState = newState

view address (expr, output, env) =
    let parsedExpr =
        String.words expr
        |> List.filter (\s -> not (String.isEmpty s))
        |> List.map parse in
    let reStringedExpr =
        parsedExpr
        |> List.map toString
        |> String.join " | " in
    div []
        [ input
            [ placeholder "Enter a Jenga Expression"
            , value expr
            , on "input" (JD.map (\v -> (v, output, env)) targetValue) (Signal.message address)
            , onEnter address env parsedExpr
            , myStyle
            ]
            []
        , div [ myStyle ] [ text reStringedExpr ]
        , div [ myStyle ] [ text output ]
        ]

onEnter address env parsedExpr =
    on "keydown"
        (JD.customDecoder keyCode is13)
        (\_ ->
            let (newEnv, newStack) = eval env [] parsedExpr in
                Signal.message address ("", toString newEnv ++ toString newStack, newEnv))

is13 code = if code == 13 then Ok () else Err "Not the right code"

myStyle =
    style
        [ ("width", "100%")
        , ("height", "40px")
        , ("padding", "10px 0")
        , ("font-size", "2em")
        , ("text-align", "center")
        ]

type JValue = JNil
            | JBool Bool
            | JInt Int
            | JSym String
            | JCons (JValue, JValue)

type Verb = Primitive (Stack -> Stack)
          | Defined Line

type alias Stack = List JValue

type alias Line = List JValue

type alias Env = Dict String Verb

eval : Env -> Stack -> Line -> (Env, Stack)
eval env stack line =
    case line of
        JSym ":" :: JSym x :: xs -> (define env x xs, stack)
        JSym "if" :: restLine ->
            case stack of
                JBool True :: restStack -> eval env restStack (ifTrue restLine)
                JBool False :: restStack -> eval env restStack (ifFalse restLine)
                _ -> Debug.crash "If expects Bool on top of stack"
        JNil :: xs -> eval env (JNil :: stack) xs
        JInt x :: xs -> eval env (JInt x :: stack) xs
        JBool x :: xs -> eval env (JBool x :: stack) xs
        JSym x :: xs -> eval env (apply env stack (lookup env x)) xs
        [] -> (env, stack)
        _ -> Debug.crash "??? Not implemented"

apply : Env -> Stack -> Verb -> Stack
apply env stack verb =
    case verb of
        Primitive f -> f stack
        Defined line ->
            case eval env stack line of
                (_, newStack) -> newStack

define : Env -> String -> Line -> Env
define env name line = Dict.insert name (Defined line) env

parse s =
    case s of
        "nil"   -> JNil
        "true"  -> JBool True
        "false" -> JBool False
        _       -> String.toInt s |> Result.toMaybe
                                  |> Maybe.map JInt
                                  |> Maybe.withDefault (JSym s)

lookup : Env -> String -> Verb
lookup env sym =
    case Dict.get sym env of
        Just f -> f
        Nothing -> Debug.crash "Symbol not defined"

initEnv : Env
initEnv =
    [ ("dup",   dup)
    , ("drop",  drop)
    , ("swap",  swap)
    , ("cons",  cons)
    , ("consp", consp)
    , ("hd",    hd)
    , ("tl",    tl)
    , ("+",     add)
    ]
    |> List.map (\(sym, f) -> (sym, Primitive f))
    |> Dict.fromList

isElseSym x =
    case x of
        JSym "else" -> True
        _ -> False

isThenSym x =
    case x of
        JSym "then" -> True
        _ -> False

takeUntil f list =
    case list of
       [] -> []
       x :: xs -> if f x then [] else x :: takeUntil f xs

takeAfter f list =
    case list of
        [] -> []
        x :: xs -> if f x then xs else takeAfter f xs

-- TODO: this won't (?) work with nested if's
ifTrue line = takeUntil isElseSym line ++ takeAfter isThenSym line

-- TODO: this won't (?) work with nested if's
ifFalse line = takeAfter isElseSym (takeUntil isThenSym line) ++ takeAfter isThenSym line

dup stack =
    case stack of
        x :: xs -> x :: x :: xs
        _ -> Debug.crash "Stack must have at least 1 item"

drop stack =
    case stack of
        _ :: xs -> xs
        _ -> Debug.crash "Stack must have at least 1 item"

swap stack =
    case stack of
        x :: y :: xs -> y :: x :: xs
        _ -> Debug.crash "Stack must have at least 2 items"

cons stack =
    case stack of
        x :: y :: xs -> JCons (x, y) :: xs
        _ -> Debug.crash "Stack must have at least 2 items"

consp stack =
    case stack of
        JCons _ :: xs -> JBool True :: xs
        _ :: xs -> JBool False :: xs
        _ -> Debug.crash "Stack must have at least 1 item"

hd stack =
    case stack of
        JCons (x, _) :: xs -> x :: xs
        _ -> Debug.crash "Stack must have a Cons on top"

tl stack =
    case stack of
        JCons (_, x) :: xs -> x :: xs
        _ -> Debug.crash "Stack must have a Cons on top"

add stack =
    case stack of
        JInt x :: JInt y :: xs -> JInt (x + y) :: xs
        _ -> Debug.crash "Stack must have 2 numbers on top"
