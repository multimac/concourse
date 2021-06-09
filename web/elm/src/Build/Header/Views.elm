module Build.Header.Views exposing
    ( BackgroundShade(..)
    , BuildComment(..)
    , BuildDuration(..)
    , BuildTab
    , ButtonType(..)
    , Header
    , Timespan(..)
    , Timestamp(..)
    , Widget(..)
    , viewHeader
    )

import Assets
import Build.Styles as Styles
import Colors
import Concourse
import Concourse.BuildStatus exposing (BuildStatus)
import Html exposing (Html, text)
import Html.Attributes
    exposing
        ( attribute
        , class
        , href
        , id
        , readonly
        , style
        , title
        , value
        )
import Html.Events exposing (onBlur, onFocus, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy
import Message.Effects exposing (Effect(..), toHtmlID)
import Message.Message as Message exposing (Message(..))
import Routes
import StrictEvents exposing (onLeftClick, onWheel)
import Views.Icon as Icon


historyId : String
historyId =
    "builds"


type alias Header =
    { leftWidgets : List Widget
    , rightWidgets : List Widget
    , backgroundColor : BuildStatus
    , tabs : List BuildTab
    , comment : BuildComment
    }


type Widget
    = Button (Maybe ButtonView)
    | Title String (Maybe Concourse.JobIdentifier)
    | Duration BuildDuration
    | Spacer ( String, BuildStatus )


type BuildDuration
    = Pending
    | Running Timestamp
    | Cancelled Timestamp
    | Finished
        { started : Timestamp
        , finished : Timestamp
        , duration : Timespan
        }


type Timestamp
    = Absolute String (Maybe Timespan)
    | Relative Timespan String


type Timespan
    = JustSeconds Int
    | MinutesAndSeconds Int Int
    | HoursAndMinutes Int Int
    | DaysAndHours Int Int


type alias BuildTab =
    { id : Int
    , name : String
    , background : BuildStatus
    , href : Routes.Route
    , isCurrent : Bool
    , hasComment : Bool
    }


type alias ButtonView =
    { type_ : ButtonType
    , isClickable : Bool
    , backgroundShade : BackgroundShade
    , backgroundColor : BuildStatus
    }


type BuildComment
    = Viewing String
    | Editing String


type BackgroundShade
    = Light
    | Dark


type ButtonType
    = CancelComment
    | EditComment
    | SaveComment
    | Abort
    | Trigger
    | Rerun


buildComment : ( Bool, String ) -> List (Html Message)
buildComment ( isReadOnly, content ) =
    [ Html.div
        [ style "display" "flex"
        , style "justify-content" "center"
        ]
        [ Html.textarea
            ([ id (toHtmlID Message.BuildCommentTextArea)
             , value content
             , onInput EditBuildComment
             , onFocus FocusBuildComment
             , onBlur BlurBuildComment
             , readonly isReadOnly
             , if isReadOnly then
                style "background-color" "transparent"

               else
                style "background-color" "rgba(255, 255, 255, 0.1)"
             ]
                ++ Styles.commentTextArea
            )
            []
        ]
    ]


buildCommentBar : BuildComment -> List (Html Message)
buildCommentBar comment =
    case comment of
        Viewing content ->
            if String.isEmpty content then
                []

            else
                buildComment ( True, content )

        Editing content ->
            buildComment ( False, content )


viewHeader : Header -> Html Message
viewHeader header =
    Html.div [ class "fixed-header" ]
        ([ Html.div
            ([ id "build-header"
             , class "build-header"
             ]
                ++ Styles.header header.backgroundColor
            )
            [ Html.div [] (List.map viewWidget header.leftWidgets)
            , Html.div [ style "display" "flex" ] (List.map viewWidget header.rightWidgets)
            ]
         , viewHistory header.backgroundColor header.tabs
         ]
            ++ buildCommentBar header.comment
        )


viewWidget : Widget -> Html Message
viewWidget widget =
    case widget of
        Button button ->
            Maybe.map viewButton button |> Maybe.withDefault (Html.text "")

        Title name jobId ->
            Html.h1 [] [ viewTitle name jobId ]

        Duration duration ->
            viewDuration duration

        Spacer ( size, status ) ->
            Html.div
                [ style "height" "100%"
                , style "width" size
                , style "border-width" "0 0 0 1px"
                , style "border-color" <| Colors.buildStatusColor False status
                , style "border-style" "solid"
                ]
                []


viewDuration : BuildDuration -> Html Message
viewDuration buildDuration =
    Html.table [ class "dictionary build-duration" ] <|
        case buildDuration of
            Pending ->
                [ Html.tr []
                    [ Html.td [ class "dict-key" ] [ Html.text "pending" ]
                    , Html.td [ class "dict-value" ] []
                    ]
                ]

            Running timestamp ->
                [ Html.tr []
                    [ Html.td [ class "dict-key" ] [ Html.text "started" ]
                    , viewTimestamp timestamp
                    ]
                ]

            Cancelled timestamp ->
                [ Html.tr []
                    [ Html.td [ class "dict-key" ] [ Html.text "finished" ]
                    , viewTimestamp timestamp
                    ]
                ]

            Finished { started, finished, duration } ->
                [ Html.tr []
                    [ Html.td [ class "dict-key" ] [ Html.text "started" ]
                    , viewTimestamp started
                    ]
                , Html.tr []
                    [ Html.td [ class "dict-key" ] [ Html.text "finished" ]
                    , viewTimestamp finished
                    ]
                , Html.tr []
                    [ Html.td [ class "dict-key" ] [ Html.text "duration" ]
                    , Html.td [ class "dict-value" ] [ Html.text <| viewTimespan duration ]
                    ]
                ]


viewTimestamp : Timestamp -> Html Message
viewTimestamp timestamp =
    case timestamp of
        Relative timespan formatted ->
            Html.td
                [ class "dict-value"
                , title formatted
                ]
                [ Html.span [] [ Html.text <| viewTimespan timespan ++ " ago" ] ]

        Absolute formatted (Just timespan) ->
            Html.td
                [ class "dict-value"
                , title <| viewTimespan timespan
                ]
                [ Html.span [] [ Html.text formatted ] ]

        Absolute formatted Nothing ->
            Html.td
                [ class "dict-value"
                ]
                [ Html.span [] [ Html.text formatted ] ]


viewTimespan : Timespan -> String
viewTimespan timespan =
    case timespan of
        JustSeconds s ->
            String.fromInt s ++ "s"

        MinutesAndSeconds m s ->
            String.fromInt m ++ "m " ++ String.fromInt s ++ "s"

        HoursAndMinutes h m ->
            String.fromInt h ++ "h " ++ String.fromInt m ++ "m"

        DaysAndHours d h ->
            String.fromInt d ++ "d " ++ String.fromInt h ++ "h"


lazyViewHistory : BuildStatus -> List BuildTab -> Html Message
lazyViewHistory backgroundColor =
    Html.Lazy.lazy (viewBuildTabs backgroundColor)


viewBuildTabs : BuildStatus -> List BuildTab -> Html Message
viewBuildTabs backgroundColor =
    List.map (viewBuildTab backgroundColor) >> Html.ul [ id historyId ]


viewBuildTab : BuildStatus -> BuildTab -> Html Message
viewBuildTab backgroundColor tab =
    Html.li
        ((id <| String.fromInt tab.id)
            :: Styles.historyItem backgroundColor tab.isCurrent tab.background
        )
        ((if tab.hasComment then
            [ Html.div (Styles.historyTriangle "5px") [] ]

          else
            []
         )
            ++ [ Html.a
                    [ onLeftClick <| Click <| Message.BuildTab tab.id tab.name
                    , href <| Routes.toString tab.href
                    ]
                    [ Html.text tab.name ]
               ]
        )


viewButton : ButtonView -> Html Message
viewButton { type_, backgroundColor, backgroundShade, isClickable } =
    let
        image =
            case type_ of
                CancelComment ->
                    Assets.AbortCircleIcon |> Assets.CircleOutlineIcon

                EditComment ->
                    Assets.InfoIcon

                SaveComment ->
                    Assets.SaveIcon

                Abort ->
                    Assets.AbortCircleIcon |> Assets.CircleOutlineIcon

                Trigger ->
                    Assets.AddCircleIcon |> Assets.CircleOutlineIcon

                Rerun ->
                    Assets.RerunIcon

        accessibilityLabel =
            case type_ of
                CancelComment ->
                    "Cancel Build Comment"

                EditComment ->
                    "Edit Build Comment"

                SaveComment ->
                    "Save Build Comment"

                Abort ->
                    "Abort Build"

                Trigger ->
                    "Trigger Build"

                Rerun ->
                    "Rerun Build"

        domID =
            case type_ of
                CancelComment ->
                    Message.CancelBuildCommentButton

                EditComment ->
                    Message.EditBuildCommentButton

                SaveComment ->
                    Message.SaveBuildCommentButton

                Abort ->
                    Message.AbortBuildButton

                Trigger ->
                    Message.TriggerBuildButton

                Rerun ->
                    Message.RerunBuildButton

        styles =
            [ style "padding" "10px"
            , style "outline" "none"
            , style "margin" "0"
            , style "border-width" "0 0 0 1px"
            , style "border-color" <| Colors.buildStatusColor False backgroundColor
            , style "border-style" "solid"
            , style "position" "relative"
            , style "background-color" <|
                Colors.buildStatusColor
                    (backgroundShade == Light)
                    backgroundColor
            , style "cursor" <|
                if isClickable then
                    "pointer"

                else
                    "default"
            ]
    in
    Html.button
        ([ attribute "role" "button"
         , attribute "tabindex" "0"
         , attribute "aria-label" accessibilityLabel
         , onMouseEnter <| Hover <| Just domID
         , onMouseLeave <| Hover Nothing
         , onFocus <| Hover <| Just domID
         , onBlur <| Hover Nothing
         , id <| toHtmlID domID
         ]
            ++ (if isClickable then
                    [ onLeftClick <| Click domID ]

                else
                    []
               )
            ++ styles
        )
        [ Icon.icon
            { sizePx = 40
            , image = image
            }
            []
        ]


viewTitle : String -> Maybe Concourse.JobIdentifier -> Html Message
viewTitle name jobID =
    case jobID of
        Just jid ->
            Html.a
                [ href <|
                    Routes.toString <|
                        Routes.Job { id = jid, page = Nothing }
                , onMouseEnter <| Hover <| Just Message.JobName
                , onMouseLeave <| Hover Nothing
                , id <| toHtmlID Message.JobName
                ]
                [ Html.span [ class "build-name" ] [ Html.text jid.jobName ]
                , Html.span [ style "letter-spacing" "-1px" ] [ Html.text (" #" ++ name) ]
                ]

        _ ->
            Html.text name


viewHistory : BuildStatus -> List BuildTab -> Html Message
viewHistory backgroundColor =
    lazyViewHistory backgroundColor
        >> List.singleton
        >> Html.div [ onWheel ScrollBuilds ]
