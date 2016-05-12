import Html exposing (Html, Attribute, text, toElement, div, input, table, tr, td, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, keyCode)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String
import List
import Dict exposing (Dict)
import Json.Decode as JD
import Result

main = StartApp.start { model = ("", [], initEnv), view = view, update = update }

update newState oldState = newState

view address (expr, stack, env) =
    let parsedExpr =
        String.words expr
        |> List.filter (\s -> not (String.isEmpty s))
        |> List.map parse in
    let reStringedExpr =
        parsedExpr
        |> List.map showValue
        |> String.join " | " in
    div []
        [ input
            [ placeholder "Enter a Jenga Expression"
            , value expr
            , on "input" (JD.map (\v -> (v, stack, env)) targetValue) (Signal.message address)
            , onEnter address env stack parsedExpr
            , inputStyle
            ]
            []
        , div [ inputStyle ] [ text reStringedExpr ]
        , div [ statusStyle ] [ showEnv env ]
        , div [ statusStyle ] [ showStack stack ]
        ]

onEnter address env stack parsedExpr =
    on "keydown"
        (JD.customDecoder keyCode is13)
        (\_ ->
            let (newEnv, newStack) = eval env stack parsedExpr in
                Signal.message address ("", newStack, newEnv))

is13 code = if code == 13 then Ok () else Err "Not the right code"

inputStyle =
    style
        [ ("width", "100%")
        , ("height", "40px")
        , ("padding", "10px 0")
        , ("font-size", "2em")
        , ("text-align", "center")
        ]

statusStyle =
    style
        [ ("width", "45%")
        , ("padding", "10px")
        , ("font-size", "2em")
        , ("display", "inline-block")
        ]

showVerb verb =
    case verb of
        Primitive _ -> "<Primitive>"
        Defined line -> line |> List.map showValue |> String.join " "

showEnv env =
    let envItem (name, verb) = tr [] [td [] [text name], td [] [text (showVerb verb)]] in
    table [] (Dict.toList env |> List.map envItem)

showStack stack =
    let stackItem value = li [] [text (showValue value)] in
    ul [] (List.map stackItem stack)

showValue value =
    case value of
        JNil -> "nil"
        JBool x -> if x then "true" else "false"
        JInt x -> toString x
        JSym name -> name
        JCons (x, y) -> "(" ++ showValue x ++ " " ++ showValue y ++ ")"

type JValue =
      JNil
    | JBool Bool
    | JInt Int
    | JSym String
    | JCons (JValue, JValue)

type Verb =
      Primitive (Stack -> Stack)
    | Defined Line

type alias Stack = List JValue

type alias Line = List JValue

type alias Env = Dict String Verb

eval : Env -> Stack -> Line -> (Env, Stack)
eval env stack line =
    case line of
        JSym ":" :: JSym name :: restLine -> (define env name restLine, stack)
        JSym "if" :: restLine ->
            case stack of
                JBool True :: restStack -> eval env restStack (ifTrue restLine)
                JBool False :: restStack -> eval env restStack (ifFalse restLine)
                _ -> Debug.crash "If expects Bool on top of stack"
        JNil :: restLine -> eval env (JNil :: stack) restLine
        JInt x :: restLine -> eval env (JInt x :: stack) restLine
        JBool x :: restLine -> eval env (JBool x :: stack) restLine
        JSym name :: restLine -> eval env (apply env stack (lookup env name)) restLine
        [] -> (env, stack)
        _ -> Debug.crash "??? Not implemented"

apply : Env -> Stack -> Verb -> Stack
apply env stack verb =
    case verb of
        Primitive f -> f stack
        Defined line -> eval env stack line |> snd

define : Env -> String -> Line -> Env
define env name line = Dict.insert name (Defined line) env

parse s =
    case s of
        "nil"   -> JNil
        "true"  -> JBool True
        "false" -> JBool False
        _       ->
            String.toInt s
            |> Result.toMaybe
            |> Maybe.map JInt
            |> Maybe.withDefault (JSym s)

lookup : Env -> String -> Verb
lookup env name =
    case Dict.get name env of
        Just f -> f
        Nothing -> Debug.crash ("Symbol not defined: " ++ name)

initEnv : Env
initEnv =
    [ ("clear", clear)
    , ("dup",   dup)
    , ("drop",  drop)
    , ("swap",  swap)
    , ("swap3", swap3)
    , ("cons",  cons)
    , ("consp", consp)
    , ("hd",    hd)
    , ("tl",    tl)
    , ("nilp",  nilp)
    , ("+",     add)
    , ("-",     sub)
    , ("*",     mul)
    , ("pos",   pos)
    ]
    |> List.map (\(name, f) -> (name, Primitive f))
    |> Dict.fromList

isElseSym = (==) (JSym "else")

isThenSym = (==) (JSym "then")

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

clear stack =
    case stack of
        _ -> []

dup stack =
    case stack of
        x :: restStack -> x :: x :: restStack
        _ -> Debug.crash "Stack must have at least 1 item"

drop stack =
    case stack of
        _ :: restStack -> restStack
        _ -> Debug.crash "Stack must have at least 1 item"

swap stack =
    case stack of
        x :: y :: restStack -> y :: x :: restStack
        _ -> Debug.crash "Stack must have at least 2 items"

swap3 stack =
    case stack of
        x :: y :: z :: restStack -> z :: y :: x :: restStack
        _ -> Debug.crash "Stack must have at least 3 items"

cons stack =
    case stack of
        x :: y :: restStack -> JCons (x, y) :: restStack
        _ -> Debug.crash "Stack must have at least 2 items"

consp stack =
    case stack of
        JCons _ :: restStack -> JBool True :: restStack
        _ :: restStack -> JBool False :: restStack
        _ -> Debug.crash "Stack must have at least 1 item"

hd stack =
    case stack of
        JCons (x, _) :: restStack -> x :: restStack
        _ -> Debug.crash "Stack must have a Cons on top"

tl stack =
    case stack of
        JCons (_, x) :: restStack -> x :: restStack
        _ -> Debug.crash "Stack must have a Cons on top"

nilp stack =
    case stack of
        JNil :: restStack -> JBool True :: restStack
        _ :: restStack -> JBool False :: restStack
        _ -> Debug.crash "Stack must have at least 1 item"

add stack =
    case stack of
        JInt x :: JInt y :: restStack -> JInt (y + x) :: restStack
        _ -> Debug.crash "Stack must have 2 numbers on top"

sub stack =
    case stack of
        JInt x :: JInt y :: restStack -> JInt (y - x) :: restStack
        _ -> Debug.crash "Stack must have 2 numbers on top"

mul stack =
    case stack of
        JInt x :: JInt y :: restStack -> JInt (y * x) :: restStack
        _ -> Debug.crash "Stack must have 2 numbers on top"

pos stack =
    case stack of
        JInt x :: restStack -> JBool (x > 0) :: restStack
        _ -> Debug.crash "Stack must have number on top"
