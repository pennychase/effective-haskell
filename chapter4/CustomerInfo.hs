{-# LANGUAGE RecordWildCards #-}
module CustomerInfo where

-- CustomerInfo

data CustomerInfo = CustomerInfo
    { customerInfoFirstName :: String
    , customerInfoLastName :: String
    , customerInfoWidgetCount :: Int
    , customerInfoBalance :: Int
    }

showCustomer :: CustomerInfo -> String
showCustomer CustomerInfo{..} =
    customerInfoFirstName <> " " <> customerInfoLastName <> " " <> show customerInfoWidgetCount <> " " <> show customerInfoBalance

-- Create customers with default widgetCount and balance
{-
customerFactory :: String -> String -> CustomerInfo
customerFactory fname lname =
    CustomerInfo
        { customerInfoBalance = 0
        , customerInfoWidgetCount = 5
        , customerInfoFirstName = fname
        , customerInfoLastName = lname
        }
-}
-- Using RecordWildCards. The supplied values must have the names of the recrod fields.
customerFactory :: String -> String -> CustomerInfo
customerFactory customerInfoFirstName customerInfoLastName =
    let
        customerInfoBalance = 0
        customerInfoWidgetCount = 5
    in CustomerInfo {..}

-- clear balance and widgetCount
emptyCart :: CustomerInfo -> CustomerInfo
emptyCart customer =
    customer { customerInfoBalance = 0
             , customerInfoWidgetCount = 0}


-- Contact

data PreferredContactMethod 
    = Email String
    | TextMessage String
    | Mail String String String Int

-- example data
emailContact :: PreferredContactMethod
emailContact = Email "me@example.com"

textContact :: PreferredContactMethod
textContact = TextMessage "+1 212 555 1212"

mailContact :: PreferredContactMethod
mailContact = Mail "1123 S. Road St." "Suite 300" "Examplesville, OH" 98142

-- confirm contact with contact info
confirmContact :: PreferredContactMethod -> String
confirmContact contact =
    case contact of
        Email emailAddress -> "Okay, I'll email you at " <> emailAddress
        TextMessage number -> "Okay, I'll text you at " <> number
        Mail street1 street2 citystate zip ->
            "Okay, I'll send a letter to\n"
            <> street1 <> "\n"
            <> street2 <> "\n"
            <> citystate <> " " <> show zip

-- confirm contact type w/o personal info
confirmContact' :: PreferredContactMethod -> String
confirmContact' contact =
    case contact of
        Email {} -> "Okay, I'll email you"
        TextMessage {} -> "Okay, I'll text you"
        Mail {} -> "Okay, I'll send you a letter"


-- Summing Records

data Person =
    Customer
        { name :: String
        , balance :: Integer
        }
    | Employee
        { name :: String
        , manager :: String
        , salary :: Int
    }

-- example data
george = Customer { name = "George Bird"
                  , balance = 500
                  }
porter = Employee { name = "Porter P. Pupper"
                  , manager = "Remi"
                  , salary = 30000
                  }


