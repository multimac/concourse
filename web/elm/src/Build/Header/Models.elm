module Build.Header.Models exposing
    ( BuildPageType(..)
    , CommentBarVisibility(..)
    , CurrentOutput(..)
    , HistoryItem
    , Model
    )

import Build.Output.Models exposing (OutputModel)
import Concourse
import Concourse.BuildStatus as BuildStatus
import Concourse.Pagination exposing (Page)
import Message.Message exposing (DomID(..))
import Time
import Views.CommentBar as CommentBar


type alias Model r =
    { r
        | id : Int
        , name : String
        , job : Maybe Concourse.JobIdentifier
        , scrolledToCurrentBuild : Bool
        , history : List HistoryItem
        , duration : Concourse.BuildDuration
        , status : BuildStatus.BuildStatus
        , disableManualTrigger : Bool
        , now : Maybe Time.Posix
        , fetchingHistory : Bool
        , nextPage : Maybe Page
        , hasLoadedYet : Bool
        , shortcutsEnabled : Bool
        , comment : CommentBarVisibility
    }


type alias HistoryItem =
    { id : Int
    , name : String
    , status : BuildStatus.BuildStatus
    , duration : Concourse.BuildDuration
    , comment : String
    }


type CurrentOutput
    = Empty
    | Cancelled
    | Output OutputModel


type CommentBarVisibility
    = Hidden CommentBar.Model
    | Visible CommentBar.Model


type BuildPageType
    = OneOffBuildPage Concourse.BuildId
    | JobBuildPage Concourse.JobBuildIdentifier
