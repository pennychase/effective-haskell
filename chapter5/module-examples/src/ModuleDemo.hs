module ModuleDemo where

import qualified HaskellBook.Examples as Examples

georgesEmailAddress :: Maybe String
georgesEmailAddress =
    case Examples.lookupUser "george" of
        Nothing -> Nothing
        Just unauthenticatedGeorge ->
            case Examples.authenticate unauthenticatedGeorge "secret" of
                Nothing -> Nothing
                Just george -> Just $ Examples.getUserEmailAddress george

friendlyEmail :: String -> String
friendlyEmail emailAddress =
    Examples.formatMessage Examples.GreetingMessage
        { Examples.greetingSalutation = Examples.Salutation "Hello"
        , Examples.greetingTo = Examples.Name emailAddress
        , Examples.greetingFrom = [Examples.Name "mailer daemon"]
        }

demo :: String
demo = maybe "unknown user" friendlyEmail georgesEmailAddress
