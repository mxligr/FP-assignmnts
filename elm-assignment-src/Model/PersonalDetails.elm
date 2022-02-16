module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, style, href)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }


view : PersonalDetails -> Html msg
view details =
    let
        renderContactDetails contact_det = List.map (\c -> p [class "contact-detail"] [text (c.name ++ ": " ++ c.detail)]) contact_det
        renderSocials social = List.map (\s -> p [class "social-link"] [text (s.name ++ ": "), a [href (s.detail)] [text s.detail]]) social
    in
    div [] ([
        h1 [id "name"] [text (details.name)]
        , em [id "intro"] [text details.intro]
    ] ++ (renderContactDetails details.contacts)
    ++ (renderSocials details.socials))
    -- Debug.todo "Implement the Model.PersonalDetails.view function"
