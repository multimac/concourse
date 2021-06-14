module Views.Styles exposing
    ( TooltipPosition(..)
    , breadcrumbComponent
    , breadcrumbContainer
    , breadcrumbItem
    , clusterName
    , commentBarEditButton
    , commentBarSaveButton
    , commentBarText
    , commentBarTextArea
    , commentBarTextButton
    , commentBarWrapper
    , concourseLogo
    , defaultFont
    , fontFamilyDefault
    , fontWeightBold
    , fontWeightDefault
    , fontWeightLight
    , instanceGroupBadge
    , pageBelowTopBar
    , pageHeaderHeight
    , pageIncludingTopBar
    , pauseToggle
    , pauseToggleIcon
    , pauseToggleTooltip
    , separator
    , topBar
    )

import Assets
import ColorValues
import Colors
import Html
import Html.Attributes exposing (rows, style)
import Routes


defaultFont : List (Html.Attribute msg)
defaultFont =
    [ style "font-size" "12px"
    , style "font-family" fontFamilyDefault
    , style "font-weight" fontWeightDefault
    ]


fontFamilyDefault : String
fontFamilyDefault =
    "Inconsolata,monospace"


fontWeightLight : String
fontWeightLight =
    "400"


fontWeightDefault : String
fontWeightDefault =
    "700"


fontWeightBold : String
fontWeightBold =
    "900"


pageHeaderHeight : Float
pageHeaderHeight =
    54


pageIncludingTopBar : List (Html.Attribute msg)
pageIncludingTopBar =
    [ style "height" "100%"
    ]


pageBelowTopBar : Routes.Route -> List (Html.Attribute msg)
pageBelowTopBar route =
    style "padding-top" "54px"
        :: (case route of
                Routes.FlySuccess _ _ ->
                    [ style "height" "100%" ]

                Routes.Resource _ ->
                    [ style "box-sizing" "border-box"
                    , style "height" "100%"
                    , style "display" "flex"
                    ]

                Routes.Pipeline _ ->
                    [ style "box-sizing" "border-box"
                    , style "height" "100%"
                    , style "display" "flex"
                    ]

                Routes.Dashboard _ ->
                    [ style "box-sizing" "border-box"
                    , style "display" "flex"
                    , style "height" "100%"
                    , style "padding-bottom" "50px"
                    ]

                Routes.Build _ ->
                    [ style "box-sizing" "border-box"
                    , style "height" "100%"
                    , style "display" "flex"
                    ]

                Routes.OneOffBuild _ ->
                    [ style "box-sizing" "border-box"
                    , style "height" "100%"
                    , style "display" "flex"
                    ]

                Routes.Job _ ->
                    [ style "box-sizing" "border-box"
                    , style "height" "100%"
                    , style "display" "flex"
                    ]
           )


topBar : Bool -> List (Html.Attribute msg)
topBar isPaused =
    [ style "position" "fixed"
    , style "top" "0"
    , style "width" "100%"
    , style "height" "54px"
    , style "z-index" "999"
    , style "display" "flex"
    , style "justify-content" "space-between"
    , style "background-color" <|
        if isPaused then
            Colors.paused

        else
            Colors.topBarBackground
    , style "border-bottom" <| "1px solid " ++ Colors.border
    ]


concourseLogo : List (Html.Attribute msg)
concourseLogo =
    [ style "background-image" <|
        Assets.backgroundImage <|
            Just Assets.ConcourseLogoWhite
    , style "background-position" "50% 50%"
    , style "background-repeat" "no-repeat"
    , style "background-size" "42px 42px"
    , style "display" "inline-block"
    , style "width" "54px"
    , style "height" "54px"
    ]


clusterName : List (Html.Attribute msg)
clusterName =
    [ style "font-size" "21px"
    , style "color" Colors.white
    , style "letter-spacing" "0.1em"
    , style "margin-left" "10px"
    ]


breadcrumbContainer : List (Html.Attribute msg)
breadcrumbContainer =
    [ style "flex-grow" "1"
    , style "display" "flex"
    ]


breadcrumbComponent :
    { component : Assets.ComponentType
    , widthPx : Float
    , heightPx : Float
    }
    -> List (Html.Attribute msg)
breadcrumbComponent { component, widthPx, heightPx } =
    [ style "background-image" <|
        Assets.backgroundImage <|
            Just <|
                Assets.BreadcrumbIcon component
    , style "background-repeat" "no-repeat"
    , style "background-size" "contain"
    , style "background-position" "center"
    , style "display" "inline-block"
    , style "height" <| String.fromFloat heightPx ++ "px"
    , style "width" <| String.fromFloat widthPx ++ "px"
    , style "margin-right" "10px"
    ]


breadcrumbItem : Bool -> List (Html.Attribute msg)
breadcrumbItem clickable =
    [ style "display" "inline-flex"
    , style "align-items" "center"
    , style "font-size" "18px"
    , style "padding" "0 10px"
    , style "line-height" "54px"
    , style "cursor" <|
        if clickable then
            "pointer"

        else
            "default"
    , style "color" Colors.white
    ]


pauseToggle : String -> List (Html.Attribute msg)
pauseToggle margin =
    [ style "position" "relative"
    , style "margin" margin
    ]


pauseToggleIcon :
    { isHovered : Bool
    , isClickable : Bool
    }
    -> List (Html.Attribute msg)
pauseToggleIcon { isHovered, isClickable } =
    [ style "opacity" <|
        if not isClickable then
            "0.2"

        else if isHovered then
            "1"

        else
            "0.5"
    , style "cursor" <|
        if isClickable then
            "pointer"

        else
            "default"
    ]


type TooltipPosition
    = Above
    | Below


pauseToggleTooltip : TooltipPosition -> List (Html.Attribute msg)
pauseToggleTooltip ttp =
    [ style "background-color" Colors.tooltipBackground
    , style "color" Colors.tooltipText
    , style "position" "absolute"
    , style
        (case ttp of
            Above ->
                "bottom"

            Below ->
                "top"
        )
        "100%"
    , style "white-space" "nowrap"
    , style "padding" "2.5px"
    , style "margin-bottom" "5px"
    , style "right" "-150%"
    , style "z-index" "1"
    ]


separator : Float -> Html.Html msg
separator topMargin =
    Html.div
        [ style "border-bottom" "1px solid black"
        , style "margin-top" <| String.fromFloat topMargin ++ "px"
        ]
        []


instanceGroupBadge : String -> List (Html.Attribute msg)
instanceGroupBadge backgroundColor =
    [ style "background-color" backgroundColor
    , style "border-radius" "4px"
    , style "color" ColorValues.grey90
    , style "display" "flex"
    , style "letter-spacing" "0"
    , style "margin-right" "8px"
    , style "width" "20px"
    , style "height" "20px"
    , style "flex-shrink" "0"
    , style "align-items" "center"
    , style "justify-content" "center"
    ]


commentBarWrapper : List (Html.Attribute msg)
commentBarWrapper =
    [ style "border" "medium dashed rgba(255, 255, 255, 0.5)"
    , style "border-radius" "7px"
    , style "margin" "16px 0"
    ]


commentBarTextArea : List (Html.Attribute msg)
commentBarTextArea =
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
        ++ defaultFont


commentBarText : List (Html.Attribute msg)
commentBarText =
    [ style "flex-grow" "1"
    , style "margin" "0"
    , style "outline" "0"
    , style "padding" "8px 0"
    , style "max-height" "150px"
    , style "overflow-y" "scroll"
    ]


commentBarEditButton : List (Html.Attribute msg)
commentBarEditButton =
    [ style "padding" "5px"
    , style "margin" "5px"
    , style "cursor" "pointer"
    , style "background-origin" "content-box"
    , style "background-size" "contain"
    ]


commentBarTextButton : List (Html.Attribute msg)
commentBarTextButton =
    [ style "padding" "5px 10px"
    , style "margin" "5px 5px 7px 7px"
    , style "outline" "none"
    , style "border" "1px solid"
    , style "transition" "border 200ms ease, color 200ms ease"
    ]
        ++ defaultFont


commentBarSaveButton :
    { isHovered : Bool, commentChanged : Bool, pinCommentLoading : Bool }
    -> List (Html.Attribute msg)
commentBarSaveButton { commentChanged, isHovered, pinCommentLoading } =
    [ style "border" <|
        "1px solid "
            ++ (if commentChanged && not pinCommentLoading then
                    Colors.white

                else
                    Colors.buttonDisabledGrey
               )
    , style "background-color" <|
        if isHovered && commentChanged && not pinCommentLoading then
            Colors.frame

        else
            "transparent"
    , style "color" <|
        if commentChanged && not pinCommentLoading then
            Colors.text

        else
            Colors.buttonDisabledGrey
    , style "padding" "5px 10px"
    , style "margin" "5px 5px 7px 7px"
    , style "outline" "none"
    , style "transition" "border 200ms ease, color 200ms ease"
    , style "cursor" <|
        if commentChanged && not pinCommentLoading then
            "pointer"

        else
            "default"
    ]
        ++ defaultFont
