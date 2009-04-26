module Data.FileStore.DarcsXml (parseDarcsXML) where

import Data.Maybe (fromJust, fromMaybe)
import Data.Char (isSpace)
import Data.DateTime (parseDateTime)
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
                                 revChanges = changesXML a }
    where
        -- If we can't get a date from the XML, we default to the beginning of the POSIX era.
        -- This at least makes it easy for someone to filter out bad dates, as obviously no real DVCSs
        -- were in operation then. :)
        -- date :: Element -> UTCTime
        date = fromMaybe (posixSecondsToUTCTime $ realToFrac (0::Int)) . parseDateTime "%c" . dateXML

authorXML, dateXML, descriptionXML, emailXML, hashXML :: Element -> String
authorXML = snd . splitEmailAuthor . fromMaybe "" . findAttr (QName "author" Nothing Nothing)
emailXML  = fromMaybe "" . fst . splitEmailAuthor . fromMaybe "" . findAttr (QName "author" Nothing Nothing)
dateXML   = fromMaybe "" . findAttr (QName "local_date" Nothing Nothing)
hashXML   = fromMaybe "" . findAttr (QName "hash" Nothing Nothing)
descriptionXML = fromMaybe "" . fmap strContent . findChild (QName "name" Nothing Nothing)

changesXML :: Element -> [Change]
changesXML = analyze . filterSummary . changes

-- | Extract the file-modification fields
changes :: Element -> Element
changes = fromJust . findElement (QName  "summary" Nothing Nothing)

analyze :: [Element] -> [Change]
analyze s = map convert s
  where convert a
           | x == "add_directory" || x == "add_file" = Added b
           | x == "remove_file" || x == "remove_directory" = Deleted b
           | x == "added_lines"
              || x == "modify_file"
              || x == "removed_lines"
              || x == "replaced_tokens" = Modified b
           | otherwise = error "Unknown change type"
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
                                || x == "replaced_tokens")
