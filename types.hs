{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{---------------------------------------------------------------------}
{-                              TYPES                                -}
{---------------------------------------------------------------------}

module Types where

-- import Data

import Language.Haskell.TH

import Data.Aeson ((.=), ToJSON, object, toJSON)
import Data.Maybe (maybeToList)
import GHC.Unicode

import ThMagic

-- Simple type synonyms for nices type signatures
type BandwidthKBps = Integer
type Index = Integer
type NumId = Integer
type RateBps = Integer
type RpcTag = Integer
type TrackerId = Integer
type Priority = Integer
type Sha1 = String
type Url = String



-- The general Id type. Some RPC methods accept SHA1 Ids (String),
-- as well as numeric Ids (Integer)
data Id = NumId Integer
        | Sha1 String
        deriving (Show, Eq)

instance ToJSON Id where
    toJSON (Sha1 s) = toJSON s
    toJSON (NumId i) = toJSON i

-- Some methods accept a list of Ids.
data Ids = IdList [Id]
         -- TODO: is this really necessary? Test with single SHA1 value
         | RecentlyActive
         | All
         deriving (Show, Eq)

instance ToJSON Ids where
    toJSON (IdList ids) = toJSON ids
    toJSON RecentlyActive = toJSON ("recently-active" :: String)
    -- An empty JSON array is a shortcut for all ids
    toJSON All = toJSON ([] :: [String])




{----------------------------------------------------------------------
-                           ACTION REQUEST                           -
----------------------------------------------------------------------}

-- Example:

-- Say we want to get the name and total size of torrents #7 and #10.

-- Request:

--    {
--       "arguments": {
--           "fields": [ "id", "name", "totalSize" ],
--           "ids": [ 7, 10 ]
--       },
--       "method": "torrent-get",
--       "tag": 39693
--    }

data ActionMethod = Start
                  | StartNow
                  | Stop
                  | Verify
                  | Reannounce
                  deriving (Show, Eq)


class TransmissionJson a where
    toTransmissionJson :: a -> String
    fromTransmissionJson :: String -> a

data ActionRequest = ActionRequest ActionMethod Ids (Maybe RpcTag)
                   deriving (Show, Eq)


instance ToJSON ActionRequest where
    toJSON (ActionRequest method arg optTag) =
        -- empty if no tag was given
        object $ [ "method" .= methodAesonValue
                 , "arguments" .= (object ["ids" .= arg])
                 ] ++ tag
        where tag = maybeToList $ (.=) "tag" `fmap` optTag
              methodAesonValue =
                  -- TEMPLATE HASKELL --
                  $(do tyConstr <- reify ''ActionMethod
                       let strNames = getDataConstrNames tyConstr
                       th_toJSON_action_request strNames)
                  -- **************** --


{----------------------------------------------------------------------
-                           SETTER REQUEST                           -
----------------------------------------------------------------------}

-- TODO change to Setter = Set SetField and put all those fields into seperate type SetField
data Setter = Set_BandwidthPriority    Priority           -- this torrent's bandwidth tr_priority_t
            | Set_DownloadLimit        BandwidthKBps      -- maximum download speed (KBps)
            | Set_DownloadLimited      Bool               -- true if downloadLimit is honored
            | Set_FilesWanted          [Index]            -- indices of file(s) to download
            | Set_FilesUnwanted        [Index]            -- indices of file(s) to not download
            | Set_HonorsSessionLimits  Bool               -- true if session upload limits are honored
            | Set_Location             FilePath           -- new location of the torrent's content
            | Set_PeerLimit            Integer            -- maximum number of peers
            | Set_PriorityHigh         [Index]            -- indices of high-priority file(s)
            | Set_PriorityLow          [Index]            -- indices of low-priority file(s)
            | Set_PriorityNormal       [Index]            -- indices of normal-priority file(s)
            | Set_QueuePosition        Integer            -- position of this torrent in its queue [0...n)
            | Set_SeedIdleLimit        Integer            -- torrent-level number of minutes of seeding inactivity
            | Set_SeedIdleMode         Integer            -- which seeding inactivity to use.  See tr_idlelimit
            | Set_SeedRatioLimit       Double             -- torrent-level seeding ratio
            | Set_SeedRatioMode        Integer            -- which ratio to use.  See tr_ratiolimit
            | Set_TrackerAdd           [String]           -- strings of announce URLs to add
            | Set_TrackerRemove        [Integer]          -- ids of trackers to remove
            | Set_TrackerReplace       [(TrackerId, Url)] -- pairs of <trackerId/new announce URLs>
            | Set_UploadLimit          BandwidthKBps      -- maximum upload speed (KBps)
            | Set_UploadLimited        Bool               -- true if uploadLimit is honored
            deriving (Show, Eq)


data SetterRequest = SetReq [Setter] Ids (Maybe RpcTag)
                    deriving (Show, Eq)

-- convenience for single torrent update
-- e.g. mkSetterRequest (Set_DownloadLimit 512) 2
mkSetterRequest mutatorArg torrentId =
    SetReq [mutatorArg] (IdList [NumId torrentId])

instance ToJSON SetterRequest where
    toJSON (SetReq keyValues idArg optTag) =
        object $ [ "method" .= (toJSON ("torrent-set" :: String))
                 , "arguments" .= argumentsObj
                 ] ++ tag
        where
            tag = maybeToList $ (.=) "tag" `fmap` optTag
            argumentsObj = object $ idPair : argumentPairs
            idPair = "ids" .= (toJSON idArg)
            argumentPairs = flip map keyValues $ \method ->
                -- TEMPLATE HASKELL --
                $(do tyConstr <- reify ''Setter
                     let strNames = getDataConstrNames tyConstr
                     th_toJSON_setter_request strNames)
                -- **************** --



{---------------------------------------------------------------------}
{-                           GETTER REQUEST                          -}
{---------------------------------------------------------------------}

-- | 3.3.  Torrent Accessors

-- |    Method name: "torrent-get".

-- |    Request arguments:

-- |    (1) An optional "ids" array as described in 3.1.
-- |    (2) A required "fields" array of keys. (see list below)

-- |    Response arguments:

-- |    (1) A "torrents" array of objects, each of which contains
-- |        the key/value pairs matching the request's "fields" argument.
-- |    (2) If the request's "ids" field was "recently-active",
-- |        a "removed" array of torrent-id numbers of recently-removed
-- |        torrents.

-- |    Note: For more information on what these fields mean, see the comments
-- |    in libtransmission/transmission.h.  The "source" column here
-- |    corresponds to the data structure there.

data File = File Integer -- Bytes completed
                 Integer -- Length
                 String -- Name
          deriving (Show, Eq)

data FileStat = FileStat Integer  -- Bytes completed
                         Bool     -- Wanted
                         Priority -- Priority
              deriving (Show, Eq)

-- TODO
-- change to:
-- data TorrentField = Get GetField -- and somehow encode types for responses
data TorrentField = Get_ActivityDate             Integer
                  | Get_AddedDate                Integer
                  | Get_BandwidthPriority        Priority --
                  | Get_Comment                  String
                  | Get_CorruptEver              Integer
                  | Get_Creator                  String
                  | Get_DateCreated              Integer
                  | Get_DesiredAvailable         Integer
                  | Get_DoneDate                 Integer
                  | Get_DownloadDir              String
                  | Get_DownloadedEver           Integer
                  | Get_DownloadLimit            Integer --
                  | Get_DownloadLimited          Bool --
                  | Get_Error                    Integer
                  | Get_ErrorString              String
                  | Get_Eta                      Integer
                  | Get_EtaIdle                  Integer
                  | Get_Files                    [File]
                  | Get_FileStats                [FileStat]
                  | Get_HashString               String
                  | Get_HaveUnchecked            Integer
                  | Get_HaveValid                Integer
                  | Get_HonorsSessionLimits      Bool --
                  | Get_Id                       Integer
                  | Get_IsFinished               Bool
                  | Get_IsPrivate                Bool
                  | Get_IsStalled                Bool
                  | Get_LeftUntilDone            Integer
                  | Get_MagnetLink               Integer
                  | Get_ManualAnnounceTime       Integer
                  | Get_MaxConnectedPeers        Integer
                  | Get_MetadataPercentComplete  Double
                  | Get_Name                     String
                  | Get_PeerLimit                Integer --
                  --Get_ | FPeers                    array (see below)
                  | Get_PeersConnected           Integer
                  --Get_ | FPeersFrom                object (see below)
                  | Get_PeersGettingFromUs       Integer
                  | Get_PeersSendingToUs         Integer
                  | Get_PercentDone              Double
                  --Get_ | FPieces                   String (see below)
                  | Get_PieceCount               Integer
                  | Get_PieceSize                Integer
                  --Get_ | FPriorities               array (see below)
                  | Get_QueuePosition            Integer --
                  | Get_RateDownload             RateBps
                  | Get_RateUpload               RateBps
                  | Get_RecheckProgress          Double
                  | Get_SecondsDownloading       Integer
                  | Get_SecondsSeeding           Integer
                  | Get_SeedIdleLimit            Integer --
                  | Get_SeedIdleMode             Integer --
                  | Get_SeedRatioLimit           Double --
                  | Get_SeedRatioMode            Integer --
                  | Get_SizeWhenDone             Integer
                  | Get_StartDate                Integer
                  | Get_Status                   Integer
                  --Get_ | FTrackers                 array (see below)
                  --Get_ | FTrackerStats             array (see below)
                  | Get_TotalSize                Integer
                  | Get_TorrentFile              String
                  | Get_UploadedEver             Integer
                  | Get_UploadLimit              Integer --
                  | Get_UploadLimited            Bool --
                  | Get_UploadRatio              Double
                  --Get_ | FWanted                   array (see below)
                  --Get_ | FWebseeds                 array (see below)
                  | Get_WebseedsSendingToUs      Integer
                  deriving (Show, Eq)


data GetterRequest = GetReq [TorrentField] Ids (Maybe RpcTag)
                     deriving (Show)

instance ToJSON GetterRequest where
    toJSON (GetReq fields idArg optTag) =
        object $ [ "method" .= (toJSON ("torrent-get" :: String))
                 , "arguments" .= argumentsObj
                 ] ++ tag
        where
            tag = maybeToList $ (.=) "tag" `fmap` optTag
            argumentsObj = object $ [idPair, fieldPair]
            idPair = "ids" .= (toJSON idArg)
            fieldPair = "fields" .= (toJSON fieldsArray)
            -- fieldsArray :: [String]
            fieldsArray = flip map fields $ \method ->
                -- TEMPLATE HASKELL --
                $(do tyConstr <- reify ''TorrentField
                     let strNames = getDataConstrNames tyConstr
                     th_toJSON_getter_request strNames)
                -- **************** --

-- | Requests support three keys:
--
-- | (1) A required "method" string telling the name of the method to invoke
-- | (2) An optional "arguments" object of key/value pairs
-- | (3) An optional "tag" number used by clients to track responses.
-- |     If provided by a request, the response MUST include the same tag.

-- data RpcRequest = SetterRequest (Maybe RpcTag)
--                   GetterRequest (Maybe RpcTag)

-- | Reponses support three keys:

-- | (1) A required "result" string whose value MUST be "success" on success,
-- |     or an error string on failure.
-- | (2) An optional "arguments" object of key/value pairs
-- | (3) An optional "tag" number as described in 2.1.

-- Only Get returns information, therefor TorrentField is ok
-- Left errorString, Right if ok
data RpcResponse = Either String (Maybe [TorrentField], Maybe RpcTag)
