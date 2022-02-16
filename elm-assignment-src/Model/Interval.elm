module Model.Interval exposing (Interval, compare, full, length, oneYear, open, view, withDurationMonths, withDurationYears)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Model.Date as Date exposing (Date, Month)
import Model.Util exposing (chainCompare)


type Interval
    = Interval { start : Date, end : Maybe Date }


{-| Create an `Interval` from 2 `Date`s. If the second date is before the first the date, the function will return
`Nothing`. When possible, use the `withDurationMonths` or `withDurationYears` functions.
-}
full : Date -> Date -> Maybe Interval
full start end =
    if Date.compare start end == GT then
        Nothing

    else
        Just <| Interval { start = start, end = Just end }


{-| Create an `Interval` from a start year, start month, and a duration in months.
The start year and month are explicitly required because the duration in months is only specified if the start date
also includes a month.
This function, (assuming positive inputs) by definition, can always return a valid `Interval`.
-}
withDurationMonths : Int -> Month -> Int -> Interval
withDurationMonths startYear startMonth duration =
    let
        start =
            Date.full startYear startMonth

        end =
            Date.offsetMonths duration start
    in
    Interval { start = start, end = Just end }


{-| Create an `Interval` from a start `Date`, and a duration in years. This function, (assuming positive inputs)
by definition, can always return a valid `Interval`.
-}
withDurationYears : Date -> Int -> Interval
withDurationYears start duration =
    let
        end =
            Date.offsetMonths (duration * 12) start
    in
    Interval { start = start, end = Just end }


{-| Create an open `Interval` from a start `Date`. Usually used for creating ongoing events.
-}
open : Date -> Interval
open start =
    Interval { start = start, end = Nothing }


{-| Convenience function to create an `Interval` that represents one year.
-}
oneYear : Int -> Interval
oneYear year =
    withDurationYears (Date.onlyYear year) 1


{-| The length of an Interval, in (years, months)
-}
length : Interval -> Maybe ( Int, Int )
length (Interval interval) =
    interval.end
        |> Maybe.andThen (Date.monthsBetween interval.start)
        |> Maybe.map (\totalMonths -> ( totalMonths // 12, modBy 12 totalMonths ))


{-| Compares two intervals.

Intervals are first compared compare by the `start` field.
If the `start` field is equal, the they are compare by the `end` fields:

  - If both are missing (`Nothing`), the intervals are considered equal
  - If both are present (`Just`), the longer interval is considered greater
  - If only one interval is open (its `end` field is `Nothing`) then it will be considered greater

```
    import Model.Date as Date

    Model.Interval.compare (oneYear 2019) (oneYear 2020) --> LT
    Model.Interval.compare (oneYear 2019) (withDurationYears (Date.onlyYear 2020) 2) --> LT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (withDurationMonths 2019 Date.Jan 2) --> EQ
    Model.Interval.compare (withDurationMonths 2019 Date.Feb 2) (withDurationMonths 2019 Date.Jan 2) --> GT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (open (Date.onlyYear 2019)) --> LT
```

-}
compare : Interval -> Interval -> Order
compare (Interval intA) (Interval intB) =
    let
        checkEnd interval = 
            case interval.end of
                Just end -> True
                Nothing -> False
        
        returnEnd interval =
            case interval.end of
                Just end -> end
                _ -> Date.full 1970 Date.Jan 
    in
    
    if (Date.compare intA.start intB.start == LT) then
        LT
    else if (Date.compare intA.start intB.start == GT) then
        GT
    else
        if (checkEnd intA == False && checkEnd intB == False) then 
            EQ
        else if (checkEnd intA == True && checkEnd intB == True) then
            Date.compare (returnEnd intA) (returnEnd intB)
        else if (checkEnd intA == True && checkEnd intB == False) then
            LT
        else 
            GT

checkEndFunction : Interval -> Bool
checkEndFunction (Interval interval) = 
    case interval.end of
        Just end -> True
        Nothing -> False

returnStart : Interval -> Date
returnStart (Interval interval) = 
    interval.start

getEnd : Interval -> Maybe Date
getEnd (Interval interval) = 
    interval.end

view : Interval -> Html msg
view interval =
    let
        returnEndDate =
            if (checkEndFunction interval) then 
                String.fromInt (Date.returnYear (returnStart interval)) ++ "-" ++ Date.returnMonth (returnStart interval)
            else
                "Present"
        
        getLength =
            case (length interval) of
                Just (y, m) -> String.fromInt (Tuple.first (y, m)) ++ String.fromInt (Tuple.second (y, m))
                Nothing -> ""
    

    in
    div [class "interval"] [
        p [class "interval-start"] [text <| String.fromInt (Date.returnYear (returnStart interval)) ++ "-" ++ Date.returnMonth (returnStart interval)]
        , p [class "interval-end"] [text returnEndDate]
        , p [class "interval-length"] [text getLength]
    ]
    -- Debug.todo "Implement Model.Interval.view"
