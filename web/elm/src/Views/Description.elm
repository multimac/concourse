module Views.Description exposing
    ( Model
    , State
    , cancel
    , handleDescriptionSaved
    , handleKeyDown
    , init
    , setPristineContent
    , update
    , view
    )

import Application.Models exposing (Session)
import Assets
import Colors
import Dashboard.Styles exposing (content)
import EffectTransformer exposing (ET)
import HoverState
import Html exposing (Attribute, Html)
import Html.Attributes exposing (id, readonly, rows, style, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter, onMouseLeave)
import Keyboard
import Markdown.Parser
import Markdown.Renderer
import Message.Callback exposing (Callback(..), Fetched)
import Message.Effects exposing (Effect(..), toHtmlID)
import Message.Message exposing (DescriptionEvent(..), DescriptionTarget(..), DomID(..), Message(..))
import Message.Subscription exposing (Delivery(..))
import Views.Icon as Icon
import Views.Spinner as Spinner
import Views.Styles


type State
    = Viewing
    | Editing
    | Saving


type alias Model =
    { id : DomID
    , content : String
    , pristineContent : String
    , state : State
    , focused : Bool
    }


init : DomID -> String -> Model
init id content =
    let
        model =
            { id = id
            , content = content
            , pristineContent = content
            , state = Viewing
            , focused = False
            }
    in
    model


cancel : Model -> Model
cancel model =
    case model.state of
        Editing ->
            { model | state = Viewing, content = model.pristineContent }

        _ ->
            model


handleDescriptionSaved : DomID -> Fetched () -> ( Model, List Effect ) -> ( Model, List Effect, Bool )
handleDescriptionSaved id result ( model, effects ) =
    if id == model.id then
        case result of
            Ok () ->
                ( { model | pristineContent = model.content, state = Viewing }
                , effects ++ [ SyncTextareaHeight (DescriptionID ( model.id, DescriptionContainer )) ]
                , True
                )

            _ ->
                ( { model | state = Editing }, effects, False )

    else
        ( model, effects, False )


handleKeyDown : Keyboard.KeyEvent -> ET Model
handleKeyDown event ( model, effects ) =
    if
        (event.code == Keyboard.Enter)
            && Keyboard.hasControlModifier event
            && model.focused
    then
        saveContent ( model, effects )

    else
        ( model, effects )


saveContent : ET Model
saveContent ( model, effects ) =
    ( { model | state = Saving }
    , effects ++ [ SaveDescription model.id model.content ]
    )


setPristineContent : String -> Model -> Model
setPristineContent content model =
    { model
        | pristineContent = content
        , content =
            case model.state of
                Viewing ->
                    content

                _ ->
                    model.content
    }


update : Message -> ET Model
update msg ( model, effects ) =
    case msg of
        Click (DescriptionID ( id, DescriptionCancelButton )) ->
            if id == model.id then
                ( cancel model, effects )

            else
                ( model, effects )

        Click (DescriptionID ( id, DescriptionEditButton )) ->
            if id == model.id then
                ( { model | state = Editing }
                , effects ++ [ Focus (toHtmlID (DescriptionID ( id, DescriptionContainer ))) ]
                )

            else
                ( model, effects )

        Click (DescriptionID ( id, DescriptionSaveButton )) ->
            if id == model.id then
                saveContent ( model, effects )

            else
                ( model, effects )

        Description ( id, EditDescription input ) ->
            if id == model.id then
                ( { model | content = input }
                , effects ++ [ SyncTextareaHeight (DescriptionID ( model.id, DescriptionTextarea )) ]
                )

            else
                ( model, effects )

        Description ( id, FocusDescription ) ->
            if id == model.id then
                ( { model | focused = True }, effects )

            else
                ( model, effects )

        Description ( id, BlurDescription ) ->
            if id == model.id then
                ( { model | focused = False }, effects )

            else
                ( model, effects )

        _ ->
            ( model, effects )


view : Bool -> HoverState.HoverState -> Model -> Html Message
view canEdit hoverState model =
    Html.div
        (id
            (toHtmlID (DescriptionID ( model.id, DescriptionContainer )))
            :: descriptionStyle (model.state /= Viewing)
        )
        (Icon.icon
            { sizePx = 16
            , image = Assets.MessageIcon
            }
            descriptionIconStyle
            :: (case ( model.state, canEdit ) of
                    ( Viewing, _ ) ->
                        [ markdown model ]

                    ( _, True ) ->
                        [ textarea model
                        , editSaveWrapper model hoverState
                        ]

                    _ ->
                        [ markdown model ]
               )
        )


button : Model -> HoverState.HoverState -> DescriptionTarget -> Html Message -> (Bool -> List (Attribute Message)) -> Html Message
button model hoverState domID contents styling =
    Html.button
        ([ id (toHtmlID (DescriptionID ( model.id, domID )))
         , onClick (Click (DescriptionID ( model.id, domID )))
         , onMouseEnter (Hover (Just (DescriptionID ( model.id, domID ))))
         , onMouseLeave (Hover Nothing)
         ]
            ++ styling (HoverState.isHovered (DescriptionID ( model.id, DescriptionEditButton )) hoverState)
        )
        [ contents ]


editSaveWrapper : Model -> HoverState.HoverState -> Html Message
editSaveWrapper model hoverState =
    Html.div
        descriptionEditSaveWrapperStyle
        (case model.state of
            Viewing ->
                [ Icon.icon
                    { sizePx = 16
                    , image = Assets.PencilIcon
                    }
                    ([ id (toHtmlID (DescriptionID ( model.id, DescriptionEditButton )))
                     , onClick (Click (DescriptionID ( model.id, DescriptionEditButton )))
                     , onMouseEnter (Hover (Just (DescriptionID ( model.id, DescriptionEditButton ))))
                     , onMouseLeave (Hover Nothing)
                     ]
                        ++ descriptionEditButtonStyle (HoverState.isHovered (DescriptionID ( model.id, DescriptionEditButton )) hoverState)
                    )
                ]

            Editing ->
                [ button model hoverState DescriptionCancelButton (Html.text "cancel") (descriptionSaveButtonStyle True)
                , button model hoverState DescriptionSaveButton (Html.text "save") (descriptionSaveButtonStyle (model.content /= model.pristineContent))
                ]

            Saving ->
                [ button
                    model
                    hoverState
                    DescriptionSavingButton
                    (Spinner.spinner
                        { sizePx = 12
                        , margin = "0"
                        }
                    )
                    descriptionSavingButtonStyle
                ]
        )


markdown : Model -> Html Message
markdown model =
    case
        model.pristineContent
            |> Markdown.Parser.parse
            |> Result.mapError (\deadEnds -> deadEnds |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
            |> Result.andThen (\ast -> ast |> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
    of
        Ok rendered ->
            Html.div [] rendered

        Err errors ->
            Html.text errors


textarea : Model -> Html Message
textarea model =
    Html.textarea
        ([ id (toHtmlID (DescriptionID ( model.id, DescriptionTextarea )))
         , value model.content
         , onInput (\input -> Description ( model.id, EditDescription input ))
         , onFocus (Description ( model.id, FocusDescription ))
         , onBlur (Description ( model.id, BlurDescription ))
         , readonly (model.state /= Editing)
         ]
            ++ descriptionTextareaStyle
        )
        []


descriptionStyle : Bool -> List (Html.Attribute msg)
descriptionStyle isEditing =
    [ style "display" "flex"
    , style "align-items" "flex-start"
    , style "background-color" <|
        if isEditing then
            Colors.pinned

        else
            Colors.pinTools
    ]


descriptionIconStyle : List (Html.Attribute msg)
descriptionIconStyle =
    [ style "background-size" "contain"
    , style "margin" "10px"
    , style "flex-shrink" "0"
    , style "background-origin" "content-box"
    ]


descriptionTextareaStyle : List (Html.Attribute msg)
descriptionTextareaStyle =
    [ style "box-sizing" "border-box"
    , style "flex-grow" "1"
    , style "resize" "none"
    , style "outline" "none"
    , style "border" "none"
    , style "color" Colors.text
    , style "background-color" "transparent"
    , style "max-height" "150px"
    , style "margin" "8px 0"
    , rows 1
    ]
        ++ Views.Styles.defaultFont


descriptionEditSaveWrapperStyle : List (Html.Attribute msg)
descriptionEditSaveWrapperStyle =
    [ style "width" "100px"
    , style "display" "flex"
    , style "justify-content" "flex-end"
    ]


descriptionEditButtonStyle : Bool -> List (Html.Attribute msg)
descriptionEditButtonStyle isHovered =
    [ style "padding" "5px"
    , style "margin" "5px"
    , style "cursor" "pointer"
    , style "background-color" <|
        if isHovered then
            Colors.sectionHeader

        else
            Colors.pinTools
    , style "background-origin" "content-box"
    , style "background-size" "contain"
    ]


descriptionSaveButtonStyle : Bool -> Bool -> List (Html.Attribute msg)
descriptionSaveButtonStyle enabled isHovered =
    [ style "border"
        ("1px solid "
            ++ (if enabled then
                    Colors.white

                else
                    Colors.buttonDisabledGrey
               )
        )
    , style "background-color"
        (if enabled && isHovered then
            Colors.frame

         else
            "transparent"
        )
    , style "color"
        (if enabled then
            Colors.text

         else
            Colors.buttonDisabledGrey
        )
    , style "padding" "5px 10px"
    , style "margin" "5px 5px 7px 7px"
    , style "outline" "none"
    , style "transition" "border 200ms ease, color 200ms ease"
    , style "cursor"
        (if enabled then
            "pointer"

         else
            "default"
        )
    ]
        ++ Views.Styles.defaultFont


descriptionSavingButtonStyle : Bool -> List (Html.Attribute msg)
descriptionSavingButtonStyle _ =
    [ style "border" ("1px solid " ++ Colors.buttonDisabledGrey)
    , style "background-color" "transparent"
    , style "color" Colors.buttonDisabledGrey
    , style "padding" "5px 10px"
    , style "margin" "5px 5px 7px 7px"
    , style "outline" "none"
    , style "transition" "border 200ms ease, color 200ms ease"
    ]
        ++ Views.Styles.defaultFont
