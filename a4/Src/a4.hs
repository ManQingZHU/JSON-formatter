import System.Environment
import Data.Char

-- getting arguments from command line refer from : https://stackoverflow.com/questions/17374356/call-main-function-with-arguments
main :: IO ()
main = do
    args <- getArgs
    case args of
        [fin] -> do
            x <- readFile fin
            putStrLn "<html>\n<body style = \"background-color:fff9ea \">"
            putStrLn "<span style=\"font-family:monospace; white-space:pre \">"
            parseAndPrint (token x) "" [] 0
            putStrLn "</span>\n</body>\n</html>"
            -- Test
            -- printElements (token x)
        _ -> putStrLn "Need an input filename in the command line!"

printElements :: [String] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStrLn x
                          printElements xs

-- helper func for `countJStr` 
recursiveJStr :: String -> Bool -> Int
recursiveJStr [] _ = 0
recursiveJStr (x:xs) slash | x == '\\' = 1 + recursiveJStr xs (not slash)
                           | x == '"' && (not slash) = 0
                           | slash = 1 + recursiveJStr xs False
                           | otherwise = 1 + recursiveJStr xs slash

-- return the length of JSON string
countJStr :: String -> Int
countJStr [] = 0
countJStr txt = recursiveJStr txt False

-- return the length of JSON number
countJNum :: String -> Int
countJNum [] = 0
countJNum (x:xs) | elem x [']', '}', ','] || isSpace x = 0
                 | otherwise = 1 + countJNum xs

-- make every JSON element a separate token
token :: String -> [String]
token [] = []
             -- string
token (x:xs) | x == '"' =  xList : jsonStr : xList : token (drop (cntStr+1) xs)
             -- number
             | x == '-' || isDigit x = jsonNum : token (drop cntNum xs)
             -- true, null
             | x == 't' || x == 'n' = (xList ++ (take 3 xs)) : token (drop 3 xs)  
             -- false
             | x == 'f' = (xList ++ (take 4 xs)) : token (drop 4 xs) 
             -- white space
             | isSpace x = token xs
             -- other char
             | otherwise = xList : token xs
             where
                    xList = [x] 
                    cntStr = countJStr xs
                    jsonStr = xList ++ (take cntStr xs)
                    cntNum = countJNum xs
                    jsonNum = xList ++ take cntNum xs
                
-- print use different color
-- the first char of string is the mark for different JSON element
printWithColor:: String -> IO()
printWithColor [] = putStr ""
printWithColor (x:xs) | x == '{' = putStr ("<span style=\"color:b7b7b7\">" ++ xs ++ "</span>")
                      | x == '[' = putStr ("<span style=\"color:76b852\">" ++ xs ++ "</span>")
                      | x == ':' = putStr ("<span style=\"color:f1632a\">" ++ xs ++ "</span>")
                      | x == ',' = putStr ("<span style=\"color:000000\">" ++ xs ++ "</span>")
                      -- JSON number
                      | x == '#' = putStr ("<span style=\"color:3399cc\">" ++ xs ++ "</span>") 
                      -- JSON string
                      | x == '"' = putStr ("<span style=\"color:59626a\">" ++ xs ++ "</span>")
                      -- escape char
                      | x == '\\' = putStr ("<span style=\"color:7d3f98\">" ++ xs ++ "</span>")
                      -- true, false, null
                      | otherwise = putStr ("<span style=\"color:f48924\">" ++ xs ++ "</span>")

-- print a new line and indent, 3 space per indent
printNLIndent :: Int -> IO()
printNLIndent n = putStr ('\n' : (replicate (3*n) ' '))     

-- return the number of consecutive ordinary char in the string
countOrdinary :: String -> Int
countOrdinary [] = 0
countOrdinary (x:xs) | elem x ['<', '>', '&', '\'', '\\'] = 0
                     | otherwise = 1 + countOrdinary xs

-- return the length of hex number 
countHex :: String -> Int
countHex [] = 0
countHex (x:xs) | not (isHexDigit x) = 0
                | otherwise = 1 + countHex xs

parseStr :: String -> IO()
parseStr [] = putStr ""
parseStr txt = let cntOrd = countOrdinary txt
               in if cntOrd < length txt 
                then do
                    printWithColor ('"' : (take cntOrd txt))
                    let txtWithoutOrd = drop cntOrd txt
                        txtWithoutOrd2 = tail txtWithoutOrd
                        j = head txtWithoutOrd
                        k = head txtWithoutOrd2
                    if j /= '\\' then do  -- char need to be replaced
                        case j of
                            '<' -> printWithColor "\"&lt;"
                            '>' -> printWithColor "\"&gt;"
                            '&' ->  printWithColor "\"&amp;"
                            '\'' -> printWithColor "\"&apos;"
                            _ -> putStr ""
                        parseStr txtWithoutOrd2
                    else  if elem k ['"', '\\', '/', 'b', 'f', 'n', 'r', 't'] -- escape char
                    then do
                        printWithColor ("\\\\" ++ [k]) 
                        parseStr (tail txtWithoutOrd2)
                    else if k == 'u' && countHex (tail txtWithoutOrd2) == 4
                    then do
                        printWithColor ('\\' : (take 6 txtWithoutOrd))
                        parseStr (drop 6 txtWithoutOrd)
                    else do
                        printWithColor "\"\\"
                        parseStr txtWithoutOrd2 
                else printWithColor ('"' : txt)              

parseAndPrint :: [String] -> String -> [Bool] -> Int -> IO()
parseAndPrint [] _ _ _ = putStr ""
parseAndPrint (t:ts) pre scopeStack indent = 
                        let t_first = head t
                            pre_first = head pre
                            nxt_first = head (head ts)
                        in if length t > 1 then do
                            -- print key word
                            if elem t_first ['t', 'f', 'n'] then printWithColor ('c' : t) 
                            -- print string
                            else if  t_first == '"' then parseStr (tail t)
                            -- print number
                            else printWithColor ('#' : t)
                            parseAndPrint ts t scopeStack indent
                        else case t_first of
                                '{' -> do
                                    printWithColor "{{"
                                    printNLIndent (indent+1)
                                    parseAndPrint ts t (True : scopeStack) (indent+1)
                                '}' -> do
                                    printNLIndent (indent-1)
                                    printWithColor "{}"
                                    parseAndPrint ts t (tail scopeStack) (indent-1)
                                ':' -> do
                                    printWithColor ": : "
                                    parseAndPrint ts t scopeStack indent
                                '[' -> do
                                    printWithColor "[["
                                    if nxt_first == '{' then printNLIndent (indent+1)
                                    else putStr ""
                                    parseAndPrint ts t (False : scopeStack) (indent+1)
                                ']' -> do
                                    if head pre == '}' then printNLIndent indent
                                    else putStr ""
                                    printWithColor "[]"
                                    parseAndPrint ts t (tail scopeStack) (indent-1)
                                ',' -> do
                                    printWithColor ",,"
                                    if head scopeStack || elem nxt_first ['[', '{'] || elem pre_first [']', '}'] 
                                    then printNLIndent indent
                                    else putStr ""
                                    parseAndPrint ts t scopeStack indent
                                '"' -> do
                                    printWithColor ("\"&quot;")
                                    parseAndPrint ts t scopeStack indent
                                -- number
                                _ -> do
                                    printWithColor ('#' : t)
                                    parseAndPrint ts t scopeStack indent
                        
