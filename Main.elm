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

update newState _ = newState

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
        , div [ stackStyle ] [ showStack stack ]
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
        , ("font-size", "1.5em")
        , ("display", "inline-block")
        ]

stackStyle =
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
        JFloat x -> toString x
        JSym name -> name
        JCons (x, y) -> "(" ++ showValue x ++ " " ++ showValue y ++ ")"
        JQuote values -> "[" ++ (values |> List.map (((++) " ") << showValue) |> String.join "") ++ " ]"

type JValue =
      JNil
    | JBool Bool
    | JFloat Float
    | JSym String
    | JCons (JValue, JValue)
    | JQuote Line

type Verb =
      Primitive (Env -> Stack -> (Env, Stack))
    | Defined Line

type alias Stack = List JValue

type alias Line = List JValue

type alias Env = Dict String Verb

eval : Env -> Stack -> Line -> (Env, Stack)
eval env stack line =
    case line of
        JSym ":" :: JSym name :: restLine -> (define env name restLine, stack)
        JSym "[" :: restLine ->
            let (env, restLine, stack) = collectQuote 0 [] env restLine stack in
            eval env stack restLine
        JNil :: restLine -> eval env (JNil :: stack) restLine
        JFloat x :: restLine -> eval env (JFloat x :: stack) restLine
        JBool x :: restLine -> eval env (JBool x :: stack) restLine
        JSym name :: restLine ->
            let (env, stack) = apply env stack (lookup env name) in
            eval env stack restLine
        JQuote xs :: restLine -> eval env (JQuote xs :: stack) restLine
        [] -> (env, stack)
        _ -> Debug.crash "??? Not implemented"

collectQuote depth acc env line stack =
    case line of
        JSym "[" :: restLine ->
            collectQuote (depth + 1) (JSym "[" :: acc) env restLine stack
        JSym "]" :: restLine ->
            if depth == 0 then
                (env, restLine, JQuote (List.reverse acc) :: stack)
            else
                collectQuote (depth - 1) (JSym "]" :: acc) env restLine stack
        x :: restLine -> collectQuote depth (x :: acc) env restLine stack
        _ -> Debug.crash "End quote not found"

apply : Env -> Stack -> Verb -> (Env, Stack)
apply env stack verb =
    case verb of
        Primitive f -> f env stack
        Defined line -> eval env stack line

define : Env -> String -> Line -> Env
define env name line = Dict.insert name (Defined line) env

parse s =
    case s of
        "nil"   -> JNil
        "true"  -> JBool True
        "false" -> JBool False
        _       ->
            String.toFloat s
            |> Result.toMaybe
            |> Maybe.map JFloat
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
    , ("rotr",  rotr)
    , ("rotl",  rotl)
    , ("swap",  swap)
    , ("swap3", swap3)
    , ("cons",  cons)
    , ("consp", consp)
    , ("hd",    hd)
    , ("tl",    tl)
    , ("zerop", zerop)
    , ("nilp",  nilp)
    , ("+",     add)
    , ("-",     sub)
    , ("*",     mul)
    , ("/",     dvd)
    , ("pos",   pos)
    , ("sqrt",  sqroot)
    , ("if",    branch)
    , ("eval",  doEval)
    ]
    |> List.map (\(name, f) -> (name, Primitive f))
    |> Dict.fromList

clear env stack =
    case stack of
        _ -> (env, [])

dup env stack =
    case stack of
        x :: restStack -> (env, (x :: x :: restStack))
        _ -> Debug.crash "Stack must have at least 1 item"

drop env stack =
    case stack of
        _ :: restStack -> (env, restStack)
        _ -> Debug.crash "Stack must have at least 1 item"

rotr env stack =
    case stack of
        x :: y :: z :: restStack -> (env, (z :: x :: y :: restStack))
        _ -> Debug.crash "Stack must have at 3 items"

rotl env stack =
    case stack of
        x :: y :: z :: restStack -> (env, (y :: z :: x :: restStack))
        _ -> Debug.crash "Stack must have at 3 items"

swap env stack =
    case stack of
        x :: y :: restStack -> (env, (y :: x :: restStack))
        _ -> Debug.crash "Stack must have at least 2 items"

swap3 env stack =
    case stack of
        x :: y :: z :: restStack -> (env, (z :: y :: x :: restStack))
        _ -> Debug.crash "Stack must have at least 3 items"

cons env stack =
    case stack of
        x :: y :: restStack -> (env, (JCons (x, y) :: restStack))
        _ -> Debug.crash "Stack must have at least 2 items"

consp env stack =
    case stack of
        JCons _ :: restStack -> (env, (JBool True :: restStack))
        _ :: restStack -> (env, (JBool False :: restStack))
        _ -> Debug.crash "Stack must have at least 1 item"

hd env stack =
    case stack of
        JCons (x, _) :: restStack -> (env, (x :: restStack))
        _ -> Debug.crash "Stack must have a Cons on top"

tl env stack =
    case stack of
        JCons (_, x) :: restStack -> (env, (x :: restStack))
        _ -> Debug.crash "Stack must have a Cons on top"

nilp env stack =
    case stack of
        JNil :: restStack -> (env, (JBool True :: restStack))
        _ :: restStack -> (env, (JBool False :: restStack))
        _ -> Debug.crash "Stack must have at least 1 item"

zerop env stack =
    case stack of
        JFloat 0 :: restStack -> (env, (JBool True :: restStack))
        _ :: restStack -> (env, (JBool False :: restStack))
        _ -> Debug.crash "Stack must have at least 1 item"

add env stack =
    case stack of
        JFloat x :: JFloat y :: restStack -> (env, (JFloat (y + x) :: restStack))
        _ -> Debug.crash "Stack must have 2 numbers on top"

sub env stack =
    case stack of
        JFloat x :: JFloat y :: restStack -> (env, (JFloat (y - x) :: restStack))
        _ -> Debug.crash "Stack must have 2 numbers on top"

mul env stack =
    case stack of
        JFloat x :: JFloat y :: restStack -> (env, (JFloat (y * x) :: restStack))
        _ -> Debug.crash "Stack must have 2 numbers on top"

dvd env stack =
    case stack of
        JFloat x :: JFloat y :: restStack -> (env, (JFloat (y / x) :: restStack))
        _ -> Debug.crash "Stack must have 2 numbers on top"

pos env stack =
    case stack of
        JFloat x :: restStack -> (env, (JBool (x > 0) :: restStack))
        _ -> Debug.crash "Stack must have number on top"

sqroot env stack =
    case stack of
        JFloat x :: restStack -> (env, (JFloat (sqrt x) :: restStack))
        _ -> Debug.crash "Stack must have number on top"

branch env stack =
    case stack of
        _ :: JQuote t :: JBool True :: restStack -> eval env restStack t
        JQuote f :: _ :: JBool False :: restStack -> eval env restStack f
        _ -> Debug.crash "Stack must have Bool|Quote|Quote on top"

doEval env stack =
    case stack of
        JQuote line :: restStack -> eval env restStack line
        _ -> Debug.crash "Stack must have quote on top"
