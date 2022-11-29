module Main where

makeGreeting salutation person =
    salutation <> " " <> person

greetPerson = makeGreeting "Hello"

enthusiasticGreeting salutation = makeGreeting (salutation <> "!")

greetGeorge = (`makeGreeting` "George")

greetGeorge' = flip makeGreeting "George"

-- pointfree version of makeGreeting

-- First refactor
makeGreeting' = (<>) . firstPart
    where
        firstPart salutation = salutation <> " "

-- Full pointfree, substituing defintion of firstPart into code
makeGreeting'' = (<>) . (<> " ")

extendedGreeting person =
    helloAndGoodbye "Hello" "Goodbye"
    where
        helloAndGoodbye hello goodbye =
            let hello' = makeGreeting hello person
                goodbye' = makeGreeting goodbye person
            in joinWithNewlines hello' goodbye'
        joinWithNewlines a b = a <> "\n" <> b

main = print "No salutations yet"


