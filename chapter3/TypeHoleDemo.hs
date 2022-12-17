module type holeDemo where

-- First example of typehole
-- exampleNumbers :: [Int]
-- exampleNumbers = [1 .. 10]

-- getFiveNumbers :: [Int]
-- getFiveNumbers = let quantity = 5 in take quantity _

-- Second example of type hole: polymorphism
permuteThruple :: (a,b,c) -> ((a,b,c),(a,c,b),(b,a,c),(b,c,a),(c,a,b),(c,b,a))
permuteThruple (a,b,c) = ((a,b,c),(a,c,b),(b,a,c),(b,c,a),(c,a,b),(c,b,a))

mergeFirstTwo :: (a,b,c) -> (a -> b -> d) -> (d, c)
mergeFirstTwo (a,b,c) f = (f a b, c)

showFields :: String
showFields =
-- Originally: let (a, b) = combinePermutations . permuteThruple $ _
-- Type hole said we needed a thruple (String, String, anything)
    let (a, b) = combinePermutations . permuteThruple $ ("hello", "world", 10)  
    in unlines [fst a, fst b]
    where
        joinFields a b = show a <> " . " <> b
        combinePermutations (a,b,c,d,e,f) = (mergeFirstTwo a joinFields, mergeFirstTwo c joinFields)

-- Third example: error appears to be in wrong part of code
showStringPair :: (String, String) -> String
showStringPair (a,b) = "fst: " <> a <> ", snd: " <> b

doubleField :: a -> (a,a)
doubleField a = (a,a) 

showValues :: String
-- Original: showValues = unlines $ map (showStringPair . doubleField) [1 .. 10]
-- Type hole shows we need Integer -> Stirng,String): showValues = unlines $ map (showStringPair . _doubleField) [1 .. 10]
-- Add show to type hole: showValues = unlines $ map (showStringPair . _doubleField . show) [1 .. 10]
-- Compiler suggests: doubleField :: forall a. a -> (a, a) with doubleField @String
-- (doubleField . show) gives a String argument to doubleField as GHC suggests
showValues = unlines $ map (showStringPair . doubleField . show) [1 .. 10]
