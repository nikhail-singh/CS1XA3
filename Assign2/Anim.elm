module Main exposing (..)

import Html as Html
import Html.Events exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Keyboard as Key
import Mouse as Mouse
import Html.Events exposing (..) 

type alias Model= {x : Int, y : Int, w: Int, h: Int, rx:Int, ry:Int, fill:String, opacity:Float, key:Int, key1:Int, on:Bool}
type Msg = KeyMsg Key.KeyCode | Tick Float | MouseMsg Mouse.Position

--TODO

init = ({x=300,y=300,w=100,h=100, rx=1, ry=1, fill="green", opacity = 1.0, key =0, key1=192, on=False},Cmd.none)

bounded : Model -> Bool
bounded model = if (model.x+model.w <= 500) && (model.y+model.h) <=300 && (model.w>0) && (model.h>0) &&(model.w<200) && (model.h<150) then True else False

update : Msg -> Model -> (Model,Cmd.Cmd Msg)
update msg model = case msg of 
    (KeyMsg keyCode) -> case keyCode of
                        65 -> if (model.w-10<=0) then (model,Cmd.none) else({model | w=model.w-10},Cmd.none)
                        83 -> if (model.h+10>=200) then (model,Cmd.none) else({model | h=model.h+10},Cmd.none)
                        87 -> if (model.h-10<=0) then (model,Cmd.none) else({model | h=model.h-10},Cmd.none)
                        68 -> if (model.w+10>=300) then (model,Cmd.none) else({model | w=model.w+10},Cmd.none)
                        27 -> ({model|x=300,y=300,w=100,h=100, rx=1, ry=1, fill="green", opacity = 1.0, key=0,on=False},Cmd.none)
                        32 -> case model.key of
                            0 -> ({model|key=1},Cmd.none)
                            1 -> ({model|key=0},Cmd.none)
                            2 -> ({model|key=3},Cmd.none)
                            3 -> ({model|key=2},Cmd.none)
                            _-> (model,Cmd.none)
                        37 -> if(model.rx==0 ||model.ry==0) then (model,Cmd.none) else ({model | rx=model.rx-10,ry=model.ry-10},Cmd.none)
                        39 -> if(model.rx==100 ||model.ry==100) then (model,Cmd.none) else ({model | rx=model.rx+10,ry=model.ry+10},Cmd.none)
                        38 -> case model.key of 
                            0 -> ({model|key=2},Cmd.none)
                            1 -> ({model|key=3},Cmd.none)
                            2 -> ({model|key=0},Cmd.none)
                            3 -> ({model|key=1},Cmd.none)
                            _-> (model,Cmd.none)
                        _ -> (model,Cmd.none)
    MouseMsg ups ->  case model.fill of
      "green" ->({model|fill="blue"},Cmd.none)
      "blue" -> ({model|fill="turquoise"},Cmd.none)
      "turquoise" -> ({model|fill="red"},Cmd.none)
      "red" -> ({model|fill="black"},Cmd.none)
      "black" -> ({model|fill="yellow"},Cmd.none)
      "yellow" ->({model|fill="green"},Cmd.none)
      _->({model|fill="blue"},Cmd.none)
    (Tick time) ->
        case model.key of 
            1 -> 
                let 
                    newOP = 1-sin(time/500)
                in ({model|opacity=newOP},Cmd.none)
            2 ->
                let                       
                    posX = round <|300 - 200*cos (time/1000)
                    posY = round <|300 - 200*sin (time/1000)
                in  ({model|x=posX,y=posY},Cmd.none)
            3 ->
                let
                    posX = round <|300 - 200*cos (time/1000)
                    posY = round <|300 - 200*sin (time/1000)
                    newOP = 1-abs(sin(time/500))
                in  
                ({model|opacity=newOP,x=posX,y=posY},Cmd.none)
            _ -> (model,Cmd.none)
            


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch[Key.downs KeyMsg, Anim.times Tick, Mouse.downs MouseMsg]
 

view : Model -> Html.Html Msg
view model = let
      posX=toString model.x
      posY=toString model.y
      posW =toString model.w
      posH = toString model.h
      posRX = toString model.rx
      posRY = toString model.ry
      mfill =  model.fill
      mOpacity = toString model.opacity
      object = [rect [x posX, y posY, width posW, height posH, rx posRX, ry posRY, stroke "black", strokeWidth "7", fill mfill, opacity mOpacity] []]
      instructions = [ rect [x "965", y "50",width "385",height "700", fill "black"][],
                       text_ [x "1150", y "175", fill "magenta", fontFamily "Impact",textAnchor "middle",fontSize "25"] [text "Controls"],
                       text_ [x "1150", y "200", fill "white", fontFamily "Calibri",textAnchor "middle"] [text "use W A S D to grow or shrink the box"],
                       text_ [x "1150", y "225", fill "white", fontFamily "Calibri",textAnchor "middle"] [text "press spacebar to make the box fade and reappear"],
                       text_ [x "1150", y "250", fill "white", fontFamily "Calibri",textAnchor "middle"] [text "press the up arrow to make it move in a circle"],
                       text_ [x "1150", y "275", fill "white", fontFamily "Calibri",textAnchor "middle"] [text "click the mouse to make it change colour"],
                       text_ [x "1150", y "300", fill "white", fontFamily "Calibri",textAnchor "middle"] [text "use the right arrow to make the box more circular"],
                       text_ [x "1150", y "325", fill "white", fontFamily "Calibri",textAnchor "middle"] [text "use the left arrow to make the box less circular"],
                       text_ [x "1150", y "350", fill "white", fontFamily "Calibri",textAnchor "middle"] [text "press the ESC key to reset the screen"]]
    in Html.div[] [svg [width "1400",height "755"] (object++instructions) ]
        

main : Program Never Model Msg
main = Html.program
        {
         init = init,
         view = view,
         update = update,
         subscriptions = subscriptions
        }