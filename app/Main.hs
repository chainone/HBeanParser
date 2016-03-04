module Main where

import BeanParser
import System.Environment
import Data.List.Split
import Data.Maybe
import System.IO


-- id=someidentifier;class=somdidentifier
queryKeyValuePairs :: String -> Maybe [(String, String)]
queryKeyValuePairs s
                  | all (\x -> length x == 2) l = Just $ map (\a -> (head a, a!!1)) l
                  | otherwise = Nothing
                  where l = map (splitOn "=") $ splitOn ";" $ filter (/=' ') s

-- dataSource.appName;xxx.xxx.xxx;xxx
keypathList :: String -> [[String]]
keypathList s = map (splitOn ".") $ splitOn ";" $ filter (/=' ') s

helpMessage :: String
helpMessage = unlines ["HBeanParser [Bean XML Path] [Bean Identify Key Value Pairs] [Query Key Paths]"
                      ,"Example:"
                      ,"hbeanparser path_to_bean_xml_file.xml id=some_kind_of_id;class=some_kind_of_class dataSource.appName;dataLocation.appConf"]

errorMessage :: String
errorMessage = unlines ["Please make sure your input is valid", helpMessage]

processArgs :: [String] -> Maybe ([(String, String)], [[String]])
processArgs [bi, kp]
                  | isNothing kv || null kpl = Nothing
                  | otherwise = Just (fromJust kv, kpl)
                  where kv = queryKeyValuePairs bi
                        kpl = keypathList kp
processArgs s = Nothing


main :: IO ()
main = do
   (path:xs) <- getArgs
   -- putStrLn $ show $ processArgs xs
   case processArgs xs of Just (kv,kpl) -> startParse path kv kpl
                          Nothing -> putStrLn errorMessage
