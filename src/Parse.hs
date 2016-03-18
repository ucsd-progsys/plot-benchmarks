module Parse where

import Benchmark
import Data.Csv
import Data.List
import System.Directory
import Data.Either
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import System.FilePath
import Text.Regex.Posix

gulpLogs :: FilePath -> IO [V.Vector Benchmark]
gulpLogs f = do
   conts <- getDirectoryContents f
   let justCsv = filter (isSuffixOf ".csv") conts
   let noHidden = filter (\a -> not (isPrefixOf "." a)) justCsv
   let toGulp = fmap (\a -> f </> a) noHidden
   logs <- sequence $ fmap parseLog toGulp
   return $ rights logs

parseLog :: FilePath -> IO (Either String (V.Vector Benchmark))
parseLog p = do
   file <- BS.readFile p
   let (hdr, csv) = splitHeader file delimiter
   timezone <- getCurrentTimeZone
   let timestamp = getEpochTime hdr
   let gitHash = getGitHash hdr
   case (timestamp, gitHash) of
      (Nothing, Nothing) -> return $ Left "Missing timestamp and git hash!"
      (Nothing, _) -> return $ Left "Missing timestamp!"
      (_, Nothing) -> return $ Left "Missing git hash!"
      (Just ts, Just gh) -> case (decode HasHeader csv) of
         Right bm ->
            return $ Right $ fmap
               (\a -> a {benchTimestamp = utcToLocalTime
                                             timezone
                                             $ posixSecondsToUTCTime
                                               $ realToFrac ts,
                         benchHash = gh})
               bm

delimiter :: String
delimiter = take 80 $ repeat '-'

getEpochTime :: [String] -> Maybe Int
getEpochTime s = do
   let pat = "Epoch Timestamp: "
   elm <- find (\e -> e =~ pat :: Bool) s
   let (_, _, res) = elm =~ pat :: (String, String, String)
   return (read res :: Int)

getGitHash :: [String] -> Maybe String
getGitHash s = do
   let pat = "[[:space:]]*[(][[:print:]]*[)][[:space:]]*[:][[:space:]]*"
   elm <- find (\e -> e =~ pat :: Bool) s
   let (_, _, res) = elm =~ pat :: (String, String, String)
   return res

splitHeader :: BS.ByteString -> String -> ([String], BS.ByteString)
splitHeader msg delim = (hdr, BS.pack $ unlines csv)
   where
      (hdr, csv) = let ((hdrr, csvr), _) = foldl' foldFn initAcc lns in
         (reverse hdrr, reverse csvr)
      lns = lines $ BS.unpack msg
      initAcc = (([],[]), False)
      foldFn ((ls, rs), True) e = ((ls, e:rs), True)
      foldFn ((ls, rs), False) e = if e == delim
                                      then
                                      ((ls, rs), True)
                                      else
                                      ((e:ls, rs), False)

dumpLogs :: FilePath -> [(String, [(LocalTime, Benchmark)])] -> IO ()
dumpLogs out dps = sequence_ $ fmap dumpLog dps
   where
      dumpLog :: (String, [(LocalTime, Benchmark)]) -> IO ()
      dumpLog (n, dps') = do
         let n' = specToUscore n
         let dps'' = encodeByName
                        (V.fromList [csvOutName,
                                     csvOutDate,
                                     csvOutHash,
                                     csvOutTime,
                                     csvOutPass])
                        dps'
         BS.writeFile (out </> n' ++ ".csv") dps''
      specToUscore s = fmap mapper s
         where
            mapper c = case c of
               '/' -> '_'
               '.' -> '_'
               c' -> c'
