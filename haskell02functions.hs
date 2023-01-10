fever::Float -> Bool
fever f = f > 37.8

hasFever::[Float] -> [Float]
hasFever f = filter fever f

hasFever' :: [Float] -> [Float]
hasFever' f = filter (\x -> x > 37.8) f

itemize::[String] -> [String]
itemize s = map (\i -> "<li>"++ i ++"</li>") s

bigCircles:: Float -> [Float] -> [Float]
bigCircles f fl = filter (\fx -> fx > f) fl

quarentine:: [(String, Float)] -> [(String,Float)]
quarentine f = filter (\(_, fe) -> fe > 37.8) f

agesIn:: [Int] -> Int -> [Int]
agesIn a i = filter (\x -> x > i) a

startsWithA:: String -> Bool
startsWithA s = (take 1 s) == "A"

returnSuperNames:: String -> String
returnSuperNames s = do 
    if startsWithA s
        then "Super"++s
    else
        s

superNames:: [String] -> [String] 
superNames s = map returnSuperNames s

onlyShorts:: [String] -> [String]
onlyShorts s = filter (\x -> (length x ) < 5) s 


main = do
    print(hasFever [35.8, 37.8, 39, 37.6])
    print(hasFever' [35.8, 37.8, 39, 37.6])
    print(itemize ["hheee", "hello", "teste", "qq"])
    print(bigCircles 36 [35.8, 37.8, 39, 37.6])
    print( quarentine [("AA",36.7),("Hello", 38.2)] )
    print( agesIn [23,3,4,577,45] 10 )
    print( superNames ["Ola", "ABC", "teste", "alo"]) 
    print( onlyShorts ["Ola", "ABCDEF", "testef", "alo"]) 



















