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
    -- Converts x to a list to a set, then performs a set difference with validChars.
    -- Checks if that set is empty, if it is then all the characters are valid.
    if Set.diff ((x |> String.toList) |> Set.fromList) validChars |> Set.isEmpty then 
        Nothing 
    
    -- Otherwise it takes that set difference, which will contain the invalid characters,
    -- converts the set to a list, then converts the chars to strings, and joins that
    -- list of strings into a single string separated by commas.
    else  
        Just (
            Set.diff (x |> String.toList  |> Set.fromList) validChars
            |> Set.toList  
            |> List.map String.fromChar 
            |> String.join ", "
        )

{- Takes a string, runs it through validString, 
   turns the Maybe String into a String and returns it.
-}
validStringHelper : String -> String
validStringHelper x = 
    -- If validString returns an Ok string, returns that string.
    -- otherwise return empty string.
    case validString x of
        Just value -> value
        Nothing -> ""
      
{-| Translate a single word `String` as necessary.

Takes a string containing one word, translates it 
if necessary, otherwise returns the original string.
-}     
replace : String -> String
replace x = 
    -- Checks a string to see if it matches one of 
    -- the words that we must translate.
    if x == "JERK" then 
        "JOIK"
    else if x == "YOU" then 
        "YOUS"
    else if x == "YOUR" || x == "YOURE" then 
        "YER"
    -- Here I use String.right and String.left to check
    -- and make replacements of "ING" and "TH" because 
    -- we only translate if they are at an end anyway.
    else if String.right 3 x == "ING" then 
        String.left (String.length x - 3) x ++ "IN"
    else if String.left 2 x == "TH" then 
        "D" ++ String.right(String.length x - 2) x
    else
        x

{-| Attempt to translate a `String` to Meowth speak.

Returns `Ok String` with the translated string if everything went
well.


Returns `Err String` when the input string is invalid.
-}
meowthify : String -> Result String String
meowthify s = 
      -- if validString s returns Nothing, then check words to translate them.
      if validString s == Nothing then 
          -- Convert the string to a list of words, and use my replace function
          -- using map to check every word to see if it needs to be translated,
          -- then joins the string back together with spaces separating words.
          Ok (s |> String.words |> List.map replace |> String.join " ")
      
      -- Uses my validStringHelper to get rid of the "Just" portion of the 
      -- string so that we can accurately check the length based on number
      -- of invalid chars, to make output grammatically correct.
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
    -- Need to split the string at the index.
    let 
        -- Converts the string to a list of words, provides an index for each word, 
        -- then partitions the list into two separate lists at the given index.
        -- This provides a tuple of two lists, for a we want the first list, and 
        -- the second for b. Unzip the lists to separate the indexes from the strings.
        -- Then just take the second tuple, which contains the list of string values.
        a = string 
             |> String.words 
             |> List.indexedMap (,) 
             |> List.partition (\x -> (Tuple.first x) < i) 
             |> Tuple.first
             |> List.unzip
             |> Tuple.second
        b = string 
             |> String.words 
             |> List.indexedMap (,) 
             |> List.partition (\x -> (Tuple.first x) < i)
             |> Tuple.second
             |> List.unzip
             |> Tuple.second      
    in -- Then combine the lists with the phrase in the center, and join them into a string.
        String.join " " <| List.concat [a, [ "YANNO WHAT I MEAN" ], b]
          

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewContent content ->
            let
                m =  
                    { -- Capitalize the input string, and then meowthify it.
                      -- Use case of to get the error message and string.
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
                -- Create a command that will generate random values.
                -- This will generate values from 0 to the number of words in the string.
                c = Random.generate RandomIndex (0 |> Random.int (model.translated |> String.words |> List.length)) 
            in     
                ( model, c )

        RandomIndex i ->
            let
                m =  
                    { -- Use the random index to add uncertainty to the string when button is pressed.
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
