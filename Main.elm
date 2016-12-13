import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Random
import Set
import String
import Tuple


-- CONSTANTS


{-| Set of allowed input characters
-}
validChars : Set.Set Char
validChars =
    Set.fromList <| String.toList " ABCDEFGHIJKLMNOPQRSTUVWXYZ"



-- MAIN


main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- INIT


type alias Model =
    { translated : String  -- The translated message for display
    , errorMsg : String     -- The error message (if any)
    }


init : ( Model, Cmd Msg )
init =
    let
        m =
            { translated = ""
            , errorMsg = ""
            }
    in
        ( m, Cmd.none )



-- UPDATE


type Msg
    = NewContent String  -- The user has typed in the text area
    | AddUncertainty     -- The user has pressed the "Add Uncertainty" button
    | RandomIndex Int    -- The Elm Runtime has chosen a random index for you


{-| Determine if a string is valid.

Returns `Nothing` if there was no problem and the string is valid.

Returns `Just String` if there was a problem. The corresponding
`String` contains the invalid characters (separated by ", ").
-}
validString : String -> Maybe String
validString x =
    if (Set.diff ((x |> String.toList) |> Set.fromList) validChars) |> Set.isEmpty 
    then 
      Nothing 
    
    else  
       Just (
         Set.diff (x |> String.toList  |> Set.fromList ) validChars
         |> Set.toList  
         |> List.map String.fromChar 
         |> String.join ", "
       )

{- Takes a string, runs it through validString, 
   turns the Maybe String into a String and returns it.
-}
validStringHelper : String -> String
validStringHelper x = 
    case validString x of
      Just value -> value
      Nothing -> ""
      
{-| Translate a single word `String` as necessary.

Takes a string containing one word, translates it 
if necessary, otherwise returns the original string.
-}     
replace : String -> String
replace x = 
    if x == "JERK" 
      then "JOIK"
    else if x == "YOU" 
      then "YOUS"
    else if x == "YOUR" || x == "YOURE" 
      then "YER"
    else if String.right 3 x == "ING" 
      then String.left (String.length x - 3) x ++ "IN"
    else if String.left 2 x == "TH" 
      then "D" ++ String.right(String.length x - 2) x
    else
      x

{-| Attempt to translate a `String` to Meowth speak.

Returns `Ok String` with the translated string if everything went
well.


Returns `Err String` when the input string is invalid.
-}
meowthify : String -> Result String String
meowthify s = 
      if validString s == Nothing
      then 
        Ok (String.join " " (List.map replace (s |> String.words)))
        
      else if (s |> validStringHelper |> String.length ) == 1 then
        Err ("String contains an illegal character: " ++ validStringHelper s)
      else
        Err ("String contains illegal characters: " ++ validStringHelper s)
            

{-| Insert the (uncertain)phrase "YANNO WHAT I MEAN" into a `String`
at a given index. The phrase is always inserted between words.

If the index is outside the range of the input `String` (either less
than 0 or greater than the number of words in the input), the phrase
is inserted at the beginning or the end of the `String`.
-}
addUncertainty : Int -> String -> String
addUncertainty i string =
    if i < 0 then 
      String.join " " <| List.concat [ [ "YANNO WHAT I MEAN" ], [ string ] ]
    else if i > (List.length <|String.words <| string) then
      String.join " " <| List.concat [ [ string ] , [ "YANNO WHAT I MEAN" ]]
    else  
      let 
        a = List.partition (\x -> (Tuple.first x) < i) (List.indexedMap (,) (string |> String.words))
             |> Tuple.first
             |> List.unzip
             |> Tuple.second
        b = List.partition (\x -> (Tuple.first x) < i) (List.indexedMap (,) (string |> String.words))
             |> Tuple.second
             |> List.unzip
             |> Tuple.second
      in
        String.join " " <| List.concat [a, [ "YANNO WHAT I MEAN" ], b]
          

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewContent content ->
            let
                m =  
                   {
                    translated = case content |> String.toUpper |> meowthify of
                         Ok str -> str
                         Err errorMsg -> ""
                    ,       
                    errorMsg = case content |> String.toUpper |> meowthify of
                         Ok str -> ""
                         Err errorMsg -> errorMsg
                    }
            in
              ( m, Cmd.none )  

        AddUncertainty ->
            let
              c = Random.generate RandomIndex (Random.int 0 (String.length model.translated))
                
            in     
              ( model, c )

        RandomIndex i ->
            let
                m =  
                   {
                    translated = addUncertainty i model.translated
                    ,       
                    errorMsg = model.errorMsg
                    }
            in
              ( m, Cmd.none )  

-- VIEW


{-| Convert a Model into a renderable HTML value.

This function requires no modification.
-}
view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ Html.textarea
            [ Attributes.placeholder "Text to translate"
            , Events.onInput NewContent
            ]
            []
        , Html.button
            [ Events.onClick AddUncertainty ]
            [ Html.text "Add Uncertainty" ]
        , Html.div
            []
            [ Html.p
                []
                [ Html.text model.translated ]
            , Html.p
                [ Attributes.style [ ( "color", "red" ) ] ]
                [ Html.text model.errorMsg ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
