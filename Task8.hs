module Task8 where

import Text.Parsec hiding(digit)

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf $ ['0'..'9'] 

intNum :: Parser String
intNum = many1 digit

partOfFloatNum :: Parser String
partOfFloatNum = do
                    char '.'
                    result <- many1 digit
                    return $ ('.':result)

floatNum :: Parser Float
floatNum = do
                num <- intNum
                opt <- option "" partOfFloatNum
                return $ read $ (num ++ opt)

byNumber :: Char 
                    -> (Float -> Float -> Float) 
                    -> Parser Float
                    -> Parser (Float -> Float)
byNumber symbol func base = do
                                char symbol
                                spaces
                                n <- base
                                spaces
                                return $ (`func` n)

negative :: Parser Float
negative = expr 
            <|> do
                    char '-'
                    spaces
                    res <- expr
                    return $ negate res


power :: Parser Float
power = do
            x <- negative
            spaces
            ys <- many powNumber
            return $ foldl (\ x f -> f x) x ys

powNumber :: Parser (Float -> Float)
powNumber = byNumber '^' (**) negative

multNumber :: Parser (Float -> Float)
multNumber = byNumber '*' (*) power

divNumber :: Parser (Float -> Float)
divNumber = byNumber '/' (/) power

multiplication :: Parser Float
multiplication = do
                    x <- power
                    spaces
                    ys <- many (multNumber <|> divNumber)
                    return $ foldl (\ x f -> f x) x ys

plusNumber :: Parser (Float -> Float)
plusNumber = byNumber '+' (+) multiplication

minusNumber :: Parser (Float -> Float)
minusNumber = byNumber '-' (-) multiplication

addition :: Parser Float
addition = do
                x <- multiplication
                spaces
                ys <- many (plusNumber <|> minusNumber)
                return $ foldl (\ x f -> f x) x ys

expr :: Parser Float
expr = floatNum 
        <|> do 
                char '('
                spaces
                res <- addition
                char ')'
                spaces
                return $ res

root :: Parser Float
root = do
            spaces
            p <- addition
            eof
            return $ p

main =
        do
            s <- getLine
            putStrLn $ show $ parse root "<input>" s
            main
