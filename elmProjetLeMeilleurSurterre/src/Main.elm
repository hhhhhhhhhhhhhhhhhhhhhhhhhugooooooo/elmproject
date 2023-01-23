-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

import Browser
import Html exposing (Html, Attribute, div, input, text, li, ul, ol,textarea,output)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (..)
import Random
import Debug exposing (..)




-- MAIN


main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }



-- MODEL


type alias Model =
  { content : String
  , def : String
  , etatInitial : State
  , motDeLaListe : String
  }
  
type State
  = Failure Http.Error
  | Loading
  | Success (List DefDuMot)
  
type alias DefBasic= List DefDuMot

type alias DefDuMot =
    { word : String
    , meanings : List Meaning
    }

type alias Meaning =
    { partOfSpeech : String
    , definitions : List Definition
    }
    
type alias Definition =
    { definition : String
    }
init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" "" Loading  " "
  , getMot
  )
  
motVide:DefDuMot
motVide=
  { word=""
  , meanings=[]
  }




-- UPDATE


type Msg
  = Change String
  | GotText (Result Http.Error (List DefDuMot))
  | Roll
  | NewFace Int
  


update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace (Random.int 1 1000)
      )
    NewFace newFace ->
      ( {model | motDeLaListe=String.fromInt newFace}
      , Cmd.none
      )
    Change newContent ->
      ({ model | content = newContent },Cmd.none)
    GotText result ->
      case result of
        Ok quote ->
          ({model |etatInitial=Success quote, motDeLaListe=(Maybe.withDefault motVide (List.head(quote))).word}, Cmd.none)

        Err e ->
          ({model|etatInitial=Failure e}, Cmd.none)
          
          
          
          
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
  
getMot : Cmd Msg
getMot =
  Http.get
      { url = "https://api.dictionaryapi.dev/api/v2/entries/en/any"
      , expect = Http.expectJson GotText basicDecoder
      }




basicDecoder : Decoder DefBasic
basicDecoder=
  Json.Decode.list textDecoder
    
    
textDecoder : Decoder DefDuMot
textDecoder = 
    map2 DefDuMot
        (field "word" string)
        (field "meanings" <| Json.Decode.list text2Decoder)
        
text2Decoder : Decoder Meaning
text2Decoder = 
    map2 Meaning
        (field "partOfSpeech" string)
        (field "definitions" <| Json.Decode.list text3Decoder)

text3Decoder : Decoder Definition
text3Decoder = 
    Json.Decode.map Definition 
        (field "definition" string)

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Type in your attempt", Html.Attributes.value model.content, onInput Change ] []
    , div [] [ text ("") ]
    ,viewValidation model
    ,viewQuote model
    ]
    
    
viewValidation : Model -> Html msg
viewValidation model =
  if model.content == model.motDeLaListe then
    div [ style "color" "green" ] [ text ("You guessed it right ! It is "++model.motDeLaListe) ]
  else if model.content=="" then
    div [ style "color" "black" ] [ text ("You are currently not submitting an attempt !")]
  else
    div [ style "color" "red" ] [ text "That's not it. Try again !" ]
    
viewQuote : Model -> Html Msg
viewQuote model =
  case model.etatInitial of
    Failure e->
      div []
        [ text (Debug.toString e)
        ]

    Loading ->
      text "Loading..."

    Success fullText ->
      div []
        [ div[] [text ("The word's definition is \n ")]
        , div[]  (displayMeanings (Maybe.withDefault motVide (List.head (fullText))).meanings)
        ]

--displayDesc : DefDuMot -> List ( Html msg )
--displayDesc list = case list of
  --  [] -> []
    --(x::xs) -> [li [] ([text "Meaning"] ++ [ul [] (displayMeanings x.meanings)])] ++ (displayDesc xs)

displayMeanings : List Meaning -> List (Html Msg)
displayMeanings list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.partOfSpeech]] ++ [ol [] (displayDefinitions x.definitions)] ++ (displayMeanings xs)

displayDefinitions : List Definition -> List (Html Msg)
displayDefinitions list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.definition]] ++ (displayDefinitions xs)