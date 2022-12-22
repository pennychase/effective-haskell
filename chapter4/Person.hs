{-# LANGUAGE RecordWildCards #-}

module Person where

-- Implement a Person sum type with a record for each constructor (to avoid partial functions and runtime errors)

data CustomerInfo =
    CustomerInfo 
        { customerName :: String
        , customerBalance :: Int
        }

data EmployeeInfo =
    EmployeeInfo
        { employeeName :: String
        , employeeManagerName :: String
        , employeeSalary :: Int
        }

data Person
    = Customer CustomerInfo
    | Employee EmployeeInfo

-- example data
george = Customer $
    CustomerInfo { customerName = "George Bird"
                 , customerBalance = 100
                 }

porter = Employee $
    EmployeeInfo { employeeName = "Porter P. Pupper"
                 , employeeManagerName = "Remi"
                 , employeeSalary = 30000
                 }

getPersonName :: Person -> String
getPersonName person =
    case person of
        Customer customer -> customerName customer
        Employee employee -> employeeName employee


getPersonManagerName :: Person -> Maybe String
getPersonManagerName person =
    case person of
        Employee employee -> Just $ employeeManagerName employee
        _ -> Nothing

getPersonSalary :: Person -> Maybe Int
getPersonSalary person =
    case person of
        Employee employee -> Just $ employeeSalary employee
        _ -> Nothing

getPersonBalance :: Person -> Maybe Int
getPersonBalance person =
    case person of
        Customer customer -> Just $ customerBalance customer
        _ -> Nothing



