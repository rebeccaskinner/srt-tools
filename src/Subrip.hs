{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Subrip where
import Data.Word
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import Text.Printf

data Timestamp = Timestamp
  { timestampHours :: Word8
  , timestampMinutes :: Word8
  , timestampSeconds :: Word8
  , timestampMilliseconds :: Word16
  }
  deriving stock (Eq, Show)

instance Ord Timestamp where
  compare t1 t2 =
    compare (timestampHours t1) (timestampHours t2)
    <> compare (timestampMinutes t1) (timestampMinutes t2)
    <> compare (timestampSeconds t1) (timestampSeconds t2)
    <> compare (timestampMilliseconds t1) (timestampMilliseconds t2)

mergeRanges :: (Timestamp, Timestamp) -> (Timestamp, Timestamp) -> (Timestamp, Timestamp)
mergeRanges (startA, endA) (startB, endB) = (min startA startB, max endA endB)

renderTimestamp :: Timestamp -> Text
renderTimestamp (Timestamp hours minutes seconds millis) =
  Text.pack $ printf "%02d:%02d:%02d,%03d" hours minutes seconds millis

renderTimerange :: Timestamp -> Timestamp -> Text
renderTimerange t1 t2 =
  renderTimestamp t1 <> " --> " <> renderTimestamp t2

parseTimestamp :: Text -> Either String Timestamp
parseTimestamp timestamp = do
  case Text.splitOn ":" timestamp of
    [hours, minutes, secondsMillis] ->
      case Text.splitOn "," secondsMillis of
        [seconds, millis] ->
          Timestamp
            <$> decimal hours
            <*> minuteSecond minutes
            <*> minuteSecond seconds
            <*> decimal millis
        _ -> Left "failed to parse seconds,millis string"
    _ -> Left "failed to parse timestamp"
  where
    decimal :: Integral a => Text -> Either String a
    decimal = fmap fst . Text.decimal
    minuteSecond :: Integral a => Text -> Either String a
    minuteSecond t = do
      n <- decimal t
      if n >= 60
        then Left "time out of range"
        else pure n

data Subtitle = Subtitle
  { subtitleSequenceNumber :: Int
  , subtitleStartTime :: Timestamp
  , subtitleEndTime :: Timestamp
  , subtitleText :: Text
  }
  deriving stock (Show)

subtitleRange :: Subtitle -> (Timestamp, Timestamp)
subtitleRange subtitle = (subtitleStartTime subtitle, subtitleEndTime subtitle)

parseSubtitle :: Text -> Text -> [Text] -> Either String Subtitle
parseSubtitle seqNumber timestamp subText = do
  (parsedSequenceNumber, _) <- Text.decimal seqNumber
  (startTime, stopTime) <-
    case Text.splitOn " --> " timestamp of
      [start, stop] -> (,) <$> parseTimestamp start <*> parseTimestamp stop
      _otherwise -> Left "failed to parse subtitle time range"
  pure Subtitle
    { subtitleSequenceNumber = parsedSequenceNumber
    , subtitleStartTime = startTime
    , subtitleEndTime = stopTime
    , subtitleText = Text.unlines subText
    }

renderSubtitle :: Subtitle -> Text
renderSubtitle (Subtitle seqNum startTime endTime txt) =
  Text.unlines [ Text.pack $ show seqNum
               , renderTimerange startTime endTime
               , txt
               , Text.empty
               ]

newtype Subrip = Subrip { getSubtitles :: [Subtitle] }
  deriving stock (Show)

parseSubrip :: Text -> Either String Subrip
parseSubrip subrip = Subrip <$> traverse parseChunk chunks
  where
    chunks :: [Text]
    chunks = filter (not . Text.null) $ Text.splitOn "\n\n" subrip
    parseChunk :: Text -> Either String Subtitle
    parseChunk chunk =
      case Text.lines chunk of
        (seqNumber:timestamp:subText) ->
          parseSubtitle seqNumber timestamp subText
        _otherwise -> Left "invalid subtitle"

resequenceSubtitles :: Subrip -> Subrip
resequenceSubtitles (Subrip subtitles) =
  Subrip $ zipWith renumber subtitles [1..]
  where
    renumber subtitle n = subtitle { subtitleSequenceNumber = n }

compressSubtitles :: Subrip -> Subrip
compressSubtitles = resequenceSubtitles . Subrip . compress . getSubtitles
  where
    compress subtitles =
      let (first, rest) = foldr combine (Nothing, []) subtitles
      in maybe rest (:rest) first
    combine subtitle (Nothing, acc) = (Just subtitle, acc)
    combine subtitle (Just thisSub, acc)
      | subtitleText subtitle == subtitleText thisSub =
          let (start, end) = mergeRanges (subtitleRange subtitle) (subtitleRange thisSub)
              mergedSub = thisSub { subtitleStartTime = start, subtitleEndTime = end }
          in (Just mergedSub, acc)
      | otherwise = (Just subtitle, thisSub : acc)

renderSubtitles :: Subrip -> Text
renderSubtitles (Subrip subtitles) =
  Text.unlines . normalizeSpacing $ renderSubtitle <$> subtitles
  where
    normalizeSpacing = snd . foldr dropExtraSpaces (0 :: Int, []) . Text.lines . Text.unlines
    dropExtraSpaces t (n, acc)
      | Text.null t && n >= 1 = (n + 1, acc)
      | Text.null t = (n + 1, t : acc)
      | otherwise = (0, t : acc)
