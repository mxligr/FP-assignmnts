module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)
import Model.Repo exposing (sortByStars)
import List exposing (sortWith)


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"

intComp : Event -> Event -> Order
intComp event1 event2 =
    Interval.compare event1.interval event2.interval

sortByInterval : List Event -> List Event
sortByInterval events =
    sortWith intComp events
    -- Debug.todo "Implement Event.sortByInterval"


view : Event -> Html Never
view event =
    let
        showEventUrl url = 
            case url of
                Just ur -> ur
                Nothing -> ""
        
        importantEvent imp = 
            case imp of
                True -> " event event-important"
                _ -> "event"
    in
    div [class (importantEvent event.important)] [
        h3 [class "event-title"] [text event.title]
        , p [class "event-description"] [event.description]
        , p [class "event-category"] [categoryView event.category]
        , p [class "event-interval"] [Interval.view event.interval]
        , p [class "event-url"] [text (showEventUrl event.url)]
    ]
    -- Debug.todo "Implement the Model.Event.view function"
