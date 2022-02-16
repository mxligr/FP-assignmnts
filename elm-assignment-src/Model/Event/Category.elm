module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)
import Html exposing (var)
import Html.Attributes exposing (value)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories : List EventCategory
eventCategories =
    [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type alias SelectedEventCategories
    = { academic : Bool
        , work : Bool
        , project : Bool
        , award : Bool
    }


{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected =
    {academic = True
    , work = True
    , project = True
    , award = True}
    -- Debug.todo "Implement Model.Event.Category.allSelected"


{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current =
    case category of
        Academic -> current.academic
        Work -> current.work
        Project -> current.project
        Award -> current.award
    -- Debug.todo "Implement Model.Event.Category.isEventCategorySelected"


{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value current =
    case category of
        Academic -> {academic = value, work = current.work, project = current.project, award = current.award}
        Work -> {academic = current.academic, work = value, project = current.project, award = current.award}
        Project -> {academic = current.academic, work = current.work, project = value, award = current.award}
        Award -> {academic = current.academic, work = current.work, project = current.project, award = value}
    -- Debug.todo "Implement Model.Event.Category.set"


checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model =
    let
        checkboxAc = isEventCategorySelected Academic model
        checkboxW = isEventCategorySelected Work model
        checkboxP = isEventCategorySelected Project model
        checkboxAw = isEventCategorySelected Academic model
    in
    
    div [] [
        checkbox "Academic" checkboxAc Academic
        , checkbox "Work" checkboxW Work
        , checkbox "Projects" checkboxP Project
        , checkbox "Awards" checkboxAw Award
    ]
    -- Debug.todo "Implement the Model.Event.Category.view function"
