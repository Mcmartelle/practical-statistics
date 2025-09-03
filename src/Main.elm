module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, samplesToChance, chanceToSamples)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style, type_, value, placeholder, autofocus, min)
import Html.Events exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { numerator : Input
    , denominator : Input
    , percent : Input
    , samples : Input
    , statPercent : String
    , oCustom : FixedChance
    , sCustom: FixedSamples
    , errorMessages : List String
    }

type alias FixedChance =
    { title : String
    , samples : String
    , samplesRoundUp : String
    , samplesExact : Bool
    , chance : Float
    , err : OutputError
    }


type alias FixedSamples =
    { title : String
    , chance : String
    , chanceRounded : String
    , chanceExact : Bool
    , samples : Float
    , err : OutputError
    }

type alias Input =
    { str : String
    , val : Float
    , err : InputError
    }

type InputError
    = AllGood
    | EmptyInput
    | AboveAllowedRange
    | BelowAllowedRange
    | NotANumber

type OutputError
    = Good
    | Bad String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialNumerator = 1
        initialDenominator = 100
        initialPercent = 50
        initStatPercent = String.fromFloat ((initialNumerator / initialDenominator) * 100)
        initialSamples = 10
        
        oCustomChance = initialPercent / 100
        initOCustomSamples = chanceToSamples oCustomChance initialNumerator initialDenominator
        initOCustomSamplesRoundUp = chanceToSamplesRoundUp oCustomChance initialNumerator initialDenominator
        initOCustomSamplesExact = chanceToSamplesExact oCustomChance initialNumerator initialDenominator
        
        sCustomSamples = initialSamples
        initSCustomChance = samplesToChance sCustomSamples initialNumerator initialDenominator
        initSCustomChanceRounded = samplesToChanceRounded sCustomSamples initialNumerator initialDenominator
        initSCustomChanceExact = samplesToChanceExact sCustomSamples initialNumerator initialDenominator
        
    in
    (   { numerator =
            { str = String.fromFloat initialNumerator
            , val = initialNumerator
            , err = AllGood
            }
        , denominator =
            { str = String.fromFloat initialDenominator
            , val = initialDenominator
            , err = AllGood
            } 
        , percent =
            { str = String.fromFloat initialPercent
            , val = initialPercent
            , err = AllGood
            }
        , samples =
            { str = String.fromFloat initialSamples
            , val = initialSamples
            , err = AllGood
            }
        , statPercent = initStatPercent
        , oCustom =
            { title = ( percentToTitle initialPercent )
            , samples = initOCustomSamples
            , samplesRoundUp = initOCustomSamplesRoundUp
            , samplesExact = initOCustomSamplesExact
            , chance = oCustomChance
            , err = Good
            }
        , sCustom =
            { title = ( samplesToTitle initialSamples )
            , chance = initSCustomChance
            , chanceRounded = initSCustomChanceRounded
            , chanceExact = initSCustomChanceExact
            , samples = initialSamples
            , err = Good
            }
        , errorMessages = []
        }
    , Cmd.none
    )

percentToTitle : Float -> String
percentToTitle val =
    ( String.fromFloat val ) ++ "%"


samplesToTitle : Float -> String
samplesToTitle val =
    ( String.fromFloat val ) ++ " samples"


toWhole : Float -> String 
toWhole num =
    floor num
    |> String.fromInt


isWholeNumber : Float -> Bool
isWholeNumber num =
    ( num - ( toFloat ( floor num ) ) ) == 0


chanceToSamplesFloat : Float -> Float -> Float -> Float
chanceToSamplesFloat chance numerator denominator =
    let
        lnChance = logBase e (1 - chance)
        lnStat = logBase e (1 - (numerator / denominator))
    in
        lnChance / lnStat


chanceToSamples : Float -> Float -> Float -> String
chanceToSamples chance numerator denominator =
    String.fromFloat <| chanceToSamplesFloat chance numerator denominator


chanceToSamplesRoundUp : Float -> Float -> Float -> String
chanceToSamplesRoundUp chance numerator denominator =
    chanceToSamplesFloat chance numerator denominator
    |> ceiling
    |> String.fromInt


chanceToSamplesExact : Float -> Float -> Float -> Bool
chanceToSamplesExact chance numerator denominator =
    chanceToSamplesFloat chance numerator denominator
    |> isWholeNumber


samplesToChanceFloat : Float -> Float -> Float -> Float
samplesToChanceFloat samples numerator denominator =
    let
        -- lnSamples = e ^ (1 / samples)
        lnStat = logBase e (1 - (numerator / denominator))
    in
       (1 - e ^ (samples * lnStat)) * 100


samplesToChance : Float -> Float -> Float -> String
samplesToChance samples numerator denominator =
    String.fromFloat <| samplesToChanceFloat samples numerator denominator


samplesToChanceRounded : Float -> Float -> Float -> String
samplesToChanceRounded samples numerator denominator =
    samplesToChanceFloat samples numerator denominator
    |> round
    |> String.fromInt


samplesToChanceExact : Float -> Float -> Float -> Bool
samplesToChanceExact samples numerator denominator =
    samplesToChanceFloat samples numerator denominator
    |> isWholeNumber

        
getNumeratorError : String -> Float -> InputError
getNumeratorError nume deno =
    case String.toFloat nume of
        Just numer ->
            if numer <= 0 then BelowAllowedRange
            else if numer >= deno then AboveAllowedRange
            else AllGood
        Nothing ->
            if String.length nume == 0 then EmptyInput else NotANumber


numeratorErrorToMessage : InputError -> String
numeratorErrorToMessage err =
    case err of
        AllGood -> ""
        EmptyInput -> inputEmptyMessage
        AboveAllowedRange -> "Must be less than the second number to make a fraction less than 1"
        BelowAllowedRange -> "Must be greater than zero"
        NotANumber -> notANumberMessage


getDenominatorError : String -> Float -> InputError
getDenominatorError deno nume =
    case String.toFloat deno of
        Just denom ->
            if denom <= 0 then BelowAllowedRange
            else if denom <= nume then BelowAllowedRange 
            else AllGood
        Nothing ->
            if String.length deno == 0 then EmptyInput else NotANumber


denominatorErrorToMessage : InputError -> String
denominatorErrorToMessage err =
    case err of
        AllGood -> ""
        EmptyInput -> inputEmptyMessage
        AboveAllowedRange -> ""
        BelowAllowedRange -> "Must be greater than zero and greater than the first input number"
        NotANumber -> notANumberMessage


getPercentError : String -> InputError
getPercentError newNumber =
    case String.toFloat newNumber of
        Just perc ->
            if perc <= 0 then BelowAllowedRange
            else if ( perc == 100 && (String.length newNumber) > 5 ) then AboveAllowedRange
            else if perc >= 100 then AboveAllowedRange 
            else AllGood
        Nothing ->
            if String.length newNumber == 0 then EmptyInput else NotANumber


percentErrorToMessage : InputError -> String
percentErrorToMessage err =
    case err of
        AllGood -> ""
        EmptyInput -> inputEmptyMessage
        AboveAllowedRange -> "Must be less than 100"
        BelowAllowedRange -> "Must be greater than zero"
        NotANumber -> notANumberMessage


getSamplesError : String -> InputError
getSamplesError newNumber =
    case String.toFloat newNumber of
        Just samp ->
            if samp <= 0 then BelowAllowedRange
            else AllGood
        Nothing ->
            if String.length newNumber == 0 then EmptyInput else NotANumber


samplesErrorToMessage : InputError -> String
samplesErrorToMessage err =
    case err of
        AllGood -> ""
        EmptyInput -> inputEmptyMessage
        AboveAllowedRange -> ""
        BelowAllowedRange -> "Must be greater than zero"
        NotANumber -> notANumberMessage


generateErrorMessages : InputError -> InputError -> InputError -> InputError -> List String
generateErrorMessages numerErr denomErr percErr sampErr =
    []


calcOCustom : FixedChance -> Float -> Float -> Float -> FixedChance
calcOCustom oCus chance nume deno =
    let
        samples = chanceToSamples chance nume deno
        roundup = chanceToSamplesRoundUp chance nume deno
        exact = chanceToSamplesExact chance nume deno
        error =
            if (exact && chance == 1 && (String.length oCus.title) > 6) then Bad "The input chance of occurrence is too close to 100% for a 64 bit floating point number to quantify" 
            else if (exact && chance == 1) then Bad "A 100% chance of occurrence would require an infinite sample size" else Good
    in
    { oCus
        | samples = samples
        , samplesRoundUp = roundup
        , samplesExact = exact
        , chance = chance
        , err = error
        }
    
calcSCustom : FixedSamples -> Float -> Float -> Float -> FixedSamples
calcSCustom sCus samples nume deno =
    let
        chance = samplesToChance samples nume deno
        rounded = samplesToChanceRounded samples nume deno
        exact = samplesToChanceExact samples nume deno
        error = if (exact && chance == "100") then Bad "The calculated chance of occurrence is too close to 100% for a 64 bit floating point number to quantify" else Good
    in
    { sCus
        | chance = chance
        , chanceRounded = rounded
        , chanceExact = exact
        , samples = samples
        , err = error
        }



    

-- UPDATE


type Msg
    = ChangeNumerator String
    | ChangeDenominator String
    | ChangePercent String
    | ChangeSamples String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeNumerator newNumber ->
            let
                newInputNumerator = newNumber
                denominator = model.denominator
                numeratorVal = case String.toFloat newNumber of
                    Just num ->
                        num
                    Nothing ->
                        -1
                newInputError = getNumeratorError newNumber denominator.val
                denominatorError = getDenominatorError denominator.str numeratorVal
                newStatPercent = String.fromFloat ((numeratorVal / denominator.val) * 100)
                oCustom = calcOCustom model.oCustom model.oCustom.chance numeratorVal denominator.val
                sCustom = calcSCustom model.sCustom model.sCustom.samples numeratorVal denominator.val
            in
            ( { model
            | numerator =
                { str = newInputNumerator
                , val = numeratorVal
                , err = newInputError
                }
            , denominator = 
                { denominator
                | err = denominatorError
                }
            , statPercent = newStatPercent
            , oCustom = oCustom
            , sCustom = sCustom
            }, Cmd.none )


        ChangeDenominator newNumber ->
            let
                newInputDenominator = newNumber
                numerator = model.numerator
                denominatorVal = case String.toFloat newNumber of
                    Just num ->
                        num
                    Nothing ->
                        -1
                newInputError = getDenominatorError newNumber numerator.val
                numeratorError = getNumeratorError numerator.str denominatorVal
                newStatPercent = String.fromFloat ((numerator.val / denominatorVal) * 100)
                oCustom = calcOCustom model.oCustom model.oCustom.chance numerator.val denominatorVal
                sCustom = calcSCustom model.sCustom model.sCustom.samples numerator.val denominatorVal
            in
            ( { model
            | denominator =
                { str = newInputDenominator
                , val = denominatorVal
                , err = newInputError
                }
            , numerator = 
                { numerator
                | err = numeratorError
                }
            , statPercent = newStatPercent
            , oCustom = oCustom
            , sCustom = sCustom
            }, Cmd.none )


        ChangePercent newNumber ->
            let
                newInputPercent = newNumber
                numerator = model.numerator
                denominator = model.denominator

                percentVal = case String.toFloat newNumber of
                    Just num ->
                        num
                    Nothing ->
                        -1
                newInputError = getPercentError newNumber
                
                chance = percentVal / 100
                oCustom = calcOCustom model.oCustom chance numerator.val denominator.val
            in
            ( { model
            | percent =
                { str = newInputPercent
                , val = percentVal
                , err = newInputError
                }
            , oCustom = { oCustom
                | title = (percentToTitle percentVal)
                }
            }, Cmd.none )

        ChangeSamples newNumber ->
            let
                newInputSamples = newNumber
                numerator = model.numerator
                denominator = model.denominator

                samplesVal = case String.toFloat newNumber of
                    Just num ->
                        num
                    Nothing ->
                        -1
                newInputError = getSamplesError newNumber
                
                sCustom = calcSCustom model.sCustom samplesVal numerator.val denominator.val
            in
            ( { model
            | samples =
                { str = newInputSamples
                , val = samplesVal
                , err = newInputError
                }
            , sCustom = { sCustom
                | title = ( samplesToTitle samplesVal )
                }
            }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "col"]
        [ h1 [] [ text "Practical Statistics" ]
        , div [ class "main", class "wide", class "tall" ]
            [ div
                [ class "item input-item"
                , class <| superAllGoodCss model.numerator.err model.denominator.err model.percent.err model.samples.err
                ]
                [ p [ class "title" ] [ text "Inputs" ]
                , div [ class "sub-item"]
                    [ p [ class "label" ] [ text "Statistic" ]
                    , input [ class <| errColorCss model.numerator.err , Html.Attributes.min "0", type_ "number", autofocus True, placeholder "Enter a number...", value model.numerator.str, onInput ChangeNumerator] []
                    , p [ class "error" ] [ text <| numeratorErrorToMessage model.numerator.err ]
                    , p [] [ text "out of"]
                    , input [ class <| errColorCss model.denominator.err , Html.Attributes.min "0", type_ "number", placeholder "Enter a number...", value model.denominator.str, onInput ChangeDenominator] []
                    , p [ class "error" ] [ text <| denominatorErrorToMessage model.denominator.err ]
                    , percentView model.statPercent model.numerator.err
                ]
                , div [ class "sub-item"]
                    [ p [ class "label" ] [ text "Occurrence %" ]
                    , input [ class <| errColorCss model.percent.err, Html.Attributes.min "0", type_ "number", placeholder "Enter a number...", value model.percent.str, onInput ChangePercent] []
                    , p [ class "error" ] [ text <| percentErrorToMessage model.percent.err ]
                ]
                , div [ class "sub-item"]
                    [ p [ class "label" ] [ text "Sample Size" ]
                    , input [ class <| errColorCss model.samples.err, Html.Attributes.min "1", type_ "number", placeholder "Enter a number...", value model.samples.str, onInput ChangeSamples] []
                    , p [ class "error" ] [ text <| samplesErrorToMessage model.samples.err ]
                ]
            ] 
            , div [ class "col" ]
                [ div [ class "item-container" ]
                    [ fixedChanceView model.oCustom <| fixedChanceErr model.numerator.err model.denominator.err model.percent.err
                    ]
                , div [ class "item-container" ]
                    [ fixedSamplesView model.sCustom <| fixedSamplesErr model.numerator.err model.denominator.err model.samples.err
                ]
            ]
        ]
    ]


percentView : String -> InputError -> Html Msg
percentView str err =
    case err of
        AllGood -> 
            percentViewGood str

        AboveAllowedRange -> 
            percentViewGood str

        BelowAllowedRange -> 
            percentViewGood str

        _ ->
            percentViewBad



percentViewGood : String -> Html Msg
percentViewGood str =
    p [ class "exact" ]
      [ text <| "= " ++ str ++ "%" ]


percentViewBad : Html Msg
percentViewBad =
    p [ class "exact" ] [ text inputNotApplicable ]


inputErrGood : InputError -> Bool
inputErrGood err =
    case err of
        AllGood -> True
        _ -> False


fixedChanceErr : InputError -> InputError -> InputError -> Bool
fixedChanceErr nume deno perc =
    (inputErrGood nume) && (inputErrGood deno) && (inputErrGood perc)


fixedChanceView : FixedChance -> Bool -> Html Msg
fixedChanceView occ inputsGood =
    case occ.err of
        Good ->
            case inputsGood of
                True ->
                    let 
                        valText = case occ.samplesRoundUp of
                            "1" -> "sample" 
                            _ -> "samples"
    
                        showDecimal = not occ.samplesExact
    
                        precisionText = case occ.samplesExact of
                            True -> "exactly to reach a"
                            False -> "to reach a"

                    in
                    div [ class "item output-item"]
                    [ p [ class "title"] [ text "Chance of Occurrence ➜ Sample Size"]
                    , p [ class "subtitle" ] [ text occ.title ]
                    , p [ class "big-num" ] [ text occ.samplesRoundUp ]
                    , p [ class "explainer" ] [ text <| " " ++ valText ++ " " ++ precisionText ++ " " ++ occ.title ++ " chance of at least one occurrence "]
                    , if showDecimal then p [ class "exact"][ text occ.samples ] else p[][]
                    ]
                    
                False -> 
                    div [ class "item output-item", class "not-applicable"]
                    [ p [ class "title"] [ text "Chance of Occurrence ➜ Sample Size"]
                    , p [ class "subtitle" ] [ text occ.title ]
                    , p [ class "big-num" ][ text inputNotApplicable ]
                    ]
                    
        Bad errorMessage ->
            div [ class "item output-item", class "not-applicable"]
            [ p [ class "title"] [ text "Chance of Occurrence ➜ Sample Size"]
            , p [ class "subtitle" ] [ text occ.title ]
            , p [ class "explainer" ][ text errorMessage ]
            ]


fixedSamplesErr : InputError -> InputError -> InputError -> Bool
fixedSamplesErr nume deno samp =
    (inputErrGood nume) && (inputErrGood deno) && (inputErrGood samp)


fixedSamplesView : FixedSamples -> Bool -> Html Msg
fixedSamplesView occ inputsGood =
    case inputsGood of
        True ->
            case occ.err of
                Good ->
                    let 
                        valText = "chance"            
                
                        showDecimal = not occ.chanceExact
            
                        precisionText = case occ.chanceExact of
                            True -> "exactly for a sample size of"
                            False -> "for a sample size of"

                    in
                    div [ class "item output-item" ]
                    [ p [ class "title" ] [ text "Sample Size ➜ Chance of Occurrence" ]
                    , p [ class "subtitle" ] [ text occ.title ]
                    , if showDecimal then p [ class "big-num" ] [ text <| "~" ++ occ.chanceRounded ++ "%" ] else p [ class "big-num" ] [ text <| occ.chanceRounded ++ "%" ]
                    , p [ class "explainer" ] [ text <| " " ++ valText ++ " " ++ precisionText ++ " " ++ occ.title ++ " to have at least one occurrence "]
                    , if showDecimal then p [ class "exact"][ text <| occ.chance ++ "%" ] else p[][]
                    ]
                Bad errorMessage ->
                    div [ class "item output-item", class "not-applicable"]
                    [ p [ class "title" ] [ text "Sample Size ➜ Chance of Occurrence" ]
                    , p [ class "subtitle" ] [ text occ.title ]
                    , p [ class "explainer" ][ text errorMessage ]
                    ]
                    
        False -> 
            div [ class "item output-item", class "not-applicable"]
            [ p [ class "title" ] [ text "Sample Size ➜ Chance of Occurrence" ]
            , p [ class "subtitle" ] [ text occ.title ]
            , p [ class "big-num" ][ text inputNotApplicable ]
            ]


inputNotApplicable : String
inputNotApplicable =
    "Input is not applicable..."


inputEmptyMessage : String
inputEmptyMessage =
    "Input is empty"


notANumberMessage : String
notANumberMessage =
    "Must be a number"


superAllGood : InputError -> InputError -> InputError -> InputError -> Bool
superAllGood nume deno perc samp =
    (inputErrGood nume) && (inputErrGood deno) && (inputErrGood perc) && (inputErrGood samp)


superAllGoodCss : InputError -> InputError -> InputError -> InputError -> String
superAllGoodCss nume deno perc samp =
    if (superAllGood nume deno perc samp) then "all-good" else "oh-no"


errColorCss : InputError -> String
errColorCss err =
    case err of
        AllGood -> "all-good"
        EmptyInput -> "empty-input"
        _ -> "error-red"


errToCss : InputError -> String
errToCss err =
    case err of
        AllGood -> "all-good"
        BelowAllowedRange -> "below-allowed-range"
        AboveAllowedRange -> "above-allowed-range"
        NotANumber -> "not-a-number"
        EmptyInput -> "empty-input"


errToMessage : InputError -> String
errToMessage err =
    case err of
        AllGood -> ""
        BelowAllowedRange -> "Below allowed range"
        AboveAllowedRange -> "Above allowed range"
        NotANumber -> "Not a number"
        EmptyInput -> ""
