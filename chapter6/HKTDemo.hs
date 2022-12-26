{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

module HKTDemo where

import Data.Kind

-- To test function with NonEmpty
import qualified Data.List.NonEmpty as NonEmpty

toCSV :: forall (t :: Type -> Type) (a :: Type) . (Foldable t, Show a) => t a -> String
toCSV =
    let
        addField :: Show a => String -> a -> String
        addField s a = s <> "," <> (show a)

        dropLeadingComma :: String -> String
        dropLeadingComma  s =
            case s of
                ',':s' -> s'
                _ -> s
    in dropLeadingComma . foldl addField ""


csvThings :: String
csvThings =
    let
        plainList = toCSV [1,2,3]
        nonEmptyList = toCSV $ 1 NonEmpty.:| [2,3]
    in unlines [plainList, nonEmptyList]