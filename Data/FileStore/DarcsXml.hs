module Data.FileStore.DarcsXml (parseDarcsXML) where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Char (isSpace)
import Data.Time.Format (parseTimeM)
import Data.FileStore.Compat.Locale (defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Text.XML.Light

import Data.FileStore.Types (Change(..), Revision(..), Author(..))
import Data.FileStore.Utils (splitEmailAuthor)

-- | Take a String presumed to be a Darcs-generated changelog in XML format;
--   discard all tags, initializations, etc, leaving only actual patches;
--   then convert each patch entry into FileStore's homebrew 'Revision' type.
parseDarcsXML :: String -> Maybe [Revision]
parseDarcsXML str = do changelog <- parseXMLDoc str
                       let patches = filterChildrenName (\(QName n _ _) -> n == "patch") changelog
                       return $ map parseIntoRevision patches

parseIntoRevision :: Element -> Revision
parseIntoRevision a = Revision { revId = hashXML a,
                                 revDateTime = date a,
                                 revAuthor = Author { authorName=authorXML a, authorEmail=emailXML a },
                                 revDescription = descriptionXML a,
                                 revChanges = catMaybes $ changesXML a }
    where
        -- If we can't get a date from the XML, we default to the beginning of the POSIX era.
        -- This at least makes it easy for someone to filter out bad dates, as obviously no real DVCSs
        -- were in operation then. :)
        -- date :: Element -> UTCTime
        date = fromMaybe (posixSecondsToUTCTime $ realToFrac (0::Int)) . parseTimeM True defaultTimeLocale "%c" . dateXML

authorXML, dateXML, descriptionXML, emailXML, hashXML :: Element -> String
authorXML = snd . splitEmailAuthor . fromMaybe "" . findAttr (QName "author" Nothing Nothing)
emailXML  = fromMaybe "" . fst . splitEmailAuthor . fromMaybe "" . findAttr (QName "author" Nothing Nothing)
dateXML   = fromMaybe "" . findAttr (QName "local_date" Nothing Nothing)
hashXML   = fromMaybe "" . findAttr (QName "hash" Nothing Nothing)
descriptionXML = fromMaybe "" . fmap strContent . findChild (QName "name" Nothing Nothing)

-- Perhaps there was no '--summary' option used, in which case there is no 'Change' information we
-- can extract.
changesXML :: Element -> [Maybe Change]
changesXML a = case (changes a) of
                    Just b -> analyze $ filterSummary b
                    Nothing -> []

-- | Extract the file-modification fields
changes :: Element -> Maybe Element
changes = findElement (QName  "summary" Nothing Nothing)

analyze :: [Element] -> [Maybe Change]
analyze s = map convert s
  where convert a
           | x == "add_directory" || x == "add_file" = Just (Added b)
           | x == "remove_file" || x == "remove_directory" = Just (Deleted b)
           | x == "added_lines"
              || x == "modify_file"
              || x == "removed_lines"
              || x == "replaced_tokens"
              || x == "move" = Just (Modified b)
           | otherwise = Nothing
             where  x = qName . elName $ a
                    b = takeWhile (/='\n') $ dropWhile isSpace $ strContent a

filterSummary :: Element -> [Element]
filterSummary = filterElementsName (\(QName {qName = x}) -> x == "add_file"
                                || x == "add_directory"
                                || x == "remove_file"
                                || x == "remove_directory"
                                || x == "modify_file"
                                || x == "added_lines"
                                || x == "removed_lines"
                                || x == "replaced_tokens"
                                || x == "move")
