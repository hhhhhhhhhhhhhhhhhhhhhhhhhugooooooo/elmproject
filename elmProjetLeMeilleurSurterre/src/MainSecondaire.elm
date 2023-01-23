-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--
module MainSecondaire exposing (..)
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
  , fichierRecup : State2
  , etatInitial : State
  , motDeLaListe : String
  , listeDeMots : List String
  }
  
type State2
  = Failure2
  | Loading2
  | Success2 String
  
type State
  = Failure
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
  ( Model "" "" Loading2 Loading  " "  []
  , getLaListe
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
  | NewWord Int
  | GotListe (Result Http.Error String)
  
 

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
  case msg of
    GotListe result -> 
      case result of
        Ok fullText ->
          ({ model | listeDeMots = String.split " " fullText, fichierRecup =Success2 fullText}, Random.generate NewWord (Random.int 1 1000))
        Err _ ->
                ({ model | fichierRecup = Failure2  } , Cmd.none)
    NewWord id -> 
      case (getIndexMot model.listeDeMots id) of
        Nothing ->
          (model, Cmd.none)
        Just mot -> 
          ({ model | motDeLaListe = mot } , Http.get{ url = "https://api.dictionaryapi.dev/api/v2/entries/en/"++mot, expect = Http.expectJson GotText basicDecoder} )
    
    Change newContent ->
      ({ model | content = newContent },Cmd.none)
    GotText result ->
      case result of
        Ok quote ->
          ({model |etatInitial=Success quote, motDeLaListe=(Maybe.withDefault motVide (List.head(quote))).word}, Cmd.none)

        Err e ->
          ({model|etatInitial=Failure }, Cmd.none)
          
          
          
          
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


getLaListe: Cmd Msg
getLaListe=
  Http.get
  { url="http://localhost:8000/listeMotsELM.txt"
  , expect =Http.expectString GotListe }

  

getIndexMot : List a -> Int -> Maybe a
getIndexMot l i =
    if i < 0 || i >= List.length l then
        Nothing
    else
        List.head (List.drop i l)


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
    ,viewChargeFichier model
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
    Failure ->
      div []
        [ text ("The file cannot be printed")
        ]

    Loading ->
      text "Loading..."

    Success fullText ->
      div []
        [ div[] [text ("The word's definition is \n ")]
        , div[]  (displayMeanings (Maybe.withDefault motVide (List.head (fullText))).meanings)
        ]
viewChargeFichier : Model -> Html Msg
viewChargeFichier model=
  case model.fichierRecup of
    Failure2 ->
      div[]
        [ text ("\n Pas de fichier contenant les mots avec lesquels jouer")]
    Loading2 ->
      text "\n Loading for file with words to play with"
    Success2  testText-> 
      text "\n"


displayMeanings : List Meaning -> List (Html Msg)
displayMeanings list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.partOfSpeech]] ++ [ol [] (displayDefinitions x.definitions)] ++ (displayMeanings xs)

displayDefinitions : List Definition -> List (Html Msg)
displayDefinitions list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.definition]] ++ (displayDefinitions xs)