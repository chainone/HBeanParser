{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module BeanParser
    ( getKeyPathStrings
    , getKeyPathsStrings
    , startParse
    ) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.IO
import Control.Applicative


hasAttibuteKeyValuePair :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
hasAttibuteKeyValuePair kvp = (\(k,v) -> hasAttrValue k (== v)) <$> kvp

hasORAttributeKeyValuePairs :: ArrowXml a => [(String, String)] -> a XmlTree XmlTree
hasORAttributeKeyValuePairs xs = foldl1 (\a b -> a <+> b ) (hasAttibuteKeyValuePair xs)

getAttrOrChildValue :: ArrowXml a => String -> a XmlTree String
getAttrOrChildValue k = ifA (hasAttr k) (getAttrValue k) (getChildren >>> hasName k >>> getText)

getBeanRefValue :: ArrowXml a => a XmlTree String
getBeanRefValue = ifA (hasAttr "ref") (getAttrValue "ref") (getChildren >>> hasName "ref" >>> getAttrValue "bean")

getKeyPathStrings :: IOSArrow XmlTree XmlTree -> [(String, String)] -> [String] -> IO [String]
getKeyPathStrings _ _ [] = return []
getKeyPathStrings a kvp [x] =  runX $ a >>> hasORAttributeKeyValuePairs kvp /> hasName "property" >>> hasAttrValue "name" (== x) >>> getAttrOrChildValue "value"
getKeyPathStrings a kvp (x:xs) = do
      refValues <- runX $ a >>> hasORAttributeKeyValuePairs kvp /> hasName "property" >>> hasAttrValue "name" (== x) >>> getBeanRefValue
      getKeyPathStrings a ( (\r -> ("id", r)) <$> refValues) xs

getKeyPathsStrings :: IOSArrow XmlTree XmlTree -> [(String, String)] -> [[String]] -> IO [String]
getKeyPathsStrings a kvp kps = foldl1 (liftA2 (++)) $ getKeyPathStrings a kvp <$> kps

getXMLContent :: String -> IO String
getXMLContent path = do
   handle <- openFile path ReadMode
   c <- mkTextEncoding "GBK"
   hSetEncoding handle c
   hGetContents handle

startParse :: String -> [(String, String)] -> [[String]] -> IO ()
startParse path kv kpl = do
   content <- getXMLContent path
   let xml = readString [withWarnings no] content
   ret <- getKeyPathsStrings (xml >>> css "bean") kv kpl
   print ret
