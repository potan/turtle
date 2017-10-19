module Turtle exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import WebSocket

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Vector3 = ( Float, Float, Float )

type alias Twist = { linear : Vector3, angular : Vector3 }

decodV3 : Decode.Decoder Vector3
decodV3 =
  Decode.map3 (,,)
    (Decode.at [ "x" ] Decode.float)
    (Decode.at [ "y" ] Decode.float)
    (Decode.at [ "z" ] Decode.float)

decodeTwist : Decode.Decoder Twist
decodeTwist =
  Decode.map2 Twist
    (Decode.at [ "linear" ] decodV3)
    (Decode.at [ "angular" ] decodV3)

type alias Publish a = { msg : a, topic : String, op : String }

decodePublish : Decode.Decoder a -> Decode.Decoder (Publish a)
decodePublish decMsg =
  Decode.map3 (\t m o -> { msg = m, topic = t, op = o })
    (Decode.at [ "topic" ] Decode.string)
    (Decode.at [ "msg" ] decMsg)
    (Decode.at [ "op" ] Decode.string)

turtleMove : Twist -> ( Float, Float )
turtleMove t =
  case ( t.linear, t.angular ) of
    ( ( l, _, _ ), ( _, _, a ) )
     -> ( l, a )

subscr : String -> String
subscr topic = "{\"op\":\"subscribe\",\"topic\":\"" ++ topic ++ "\"}"

pub : String -> Float -> Float -> String
pub topic m r =
  "{\"topic\":\""
    ++ topic
    ++ "\",\"msg\":{\"linear\":{\"y\":0.0,\"x\":"
    ++ toString m
    ++ ",\"z\": 0.0},\"angular\":{\"y\":0.0,\"x\":0.0,\"z\":"
    ++ toString r
    ++ "}},\"op\":\"publish\"}"

-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , dir : Float
  , connected : Bool
  , ws : String
  , topic : String
  , input : String
  , messages : List String
  }

init : ( Model, Cmd Msg )
init =
  ( Model 50 50 0 False "ws://192.168.56.101:9090/" "/turtle1/cmd_vel" "" []
  , Cmd.none
  )

-- UPDATE

type Msg
  = Input String
  | Send String
  | NewMessage String
  | EnterUrl String
  | EnterTopic String
  | Connect

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    EnterTopic newInput
     -> ( { model | topic = newInput }, Cmd.none )
    EnterUrl newInput
     -> ( { model | ws = newInput }, Cmd.none )
    Connect
     -> ( { model | connected = True }, WebSocket.send model.ws (subscr model.topic) )
    Input newInput
     -> ( { model | input = newInput }, Cmd.none )
    Send data
     -> ( { model | input = "" }, WebSocket.send model.ws data )
    NewMessage str
     -> case Decode.decodeString (decodePublish decodeTwist) str of
          Err _
           -> ( { model | messages = str :: model.messages }, Cmd.none )
          Ok t
           -> let ( r, a ) = turtleMove t.msg
                  dir = model.dir + a
              in  ( { model
                    | x = model.x + r * sin dir
                    , y = model.y + r * cos dir
                    , dir = dir
                    , messages = str :: model.messages
                    }
                  , Cmd.none
                  )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.connected
  then WebSocket.listen model.ws NewMessage
  else Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    if model.connected
    then let x = toString model.x
             y = toString model.y
             dirx = toString (model.x + 5 * sin model.dir)
             diry = toString (model.y + 5 * cos model.dir)
         in  [ svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
                 [ circle [ cx x, cy y, r "4" ] []
                 , line [ x1 x, y1 y, x2 dirx, y2 diry, stroke "red" ] []
                 ]
             , br [] []
             , button [ onClick <| Send <| pub model.topic 0 1 ]
                 [ Html.text "Left" ]
             , button [ onClick <| Send <| pub model.topic 1 0 ]
                 [ Html.text "Forward" ]
             , button [ onClick <| Send <| pub model.topic -1 0 ]
                 [ Html.text "Back" ]
             , button [ onClick <| Send <| pub model.topic 0 -1 ]
                 [ Html.text "Rigth" ]
             , br [] []
             , input [ Html.Attributes.type_ "textaria", onInput Input ] []
             , button [ onClick (Send model.input) ] [ Html.text "Send" ]
             , div [] (List.map viewMessage model.messages)
             ]
    else [ Html.text "WS: "
         , input
             [ Html.Attributes.type_ "text"
             , Html.Attributes.value model.ws
             , onInput EnterUrl
             ]
             []
         , Html.text "Turtlr topic: "
         , input
             [ Html.Attributes.type_ "text"
             , Html.Attributes.value model.topic
             , onInput EnterTopic
             ]
             []
         , br [] []
         , button [ onClick Connect ] [ Html.text "Connect" ]
         ]

viewMessage : String -> Html msg
viewMessage msg = div [] [ Html.text msg ]
