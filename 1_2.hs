module Task1_2 where

import Todo(todo)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n = factorial n == [1,n]
factorial n = [x | x <- [1..n], mod n x == 0]


-- наибольший общий делитель двух чисел
gcd' :: Integer -> Integer -> Integer
gcd' x 0  =  x
gcd' x y  =  gcd y (x `rem` y)


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Int -> Integer
pow a p
    | (p == 1) = a
    | (odd p) = a * pow a (p - 1)
    | otherwise = (pow a (p `div` 2)) * (pow a (p `div` 2))

doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = (ceiling $ sqrt $ fromIntegral from) <= (floor $ sqrt $ fromIntegral to)


isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year | month < 1 || month > 12 = False
                             | year  < 0 = False
                             | month == 2 = isLeapYearCheck day year
                             | isMonthHave31DayCheck month == True  && day >= 1 && day <= 31 = True
                             | isMonthHave31DayCheck month == False && day >= 1 && day <= 30 = True
                             | otherwise = False

isMonthHave31DayCheck month | month < 8 && month `mod` 2 /= 0 = True
                            | month >= 8 && month `mod` 2 == 0 = True
                            | otherwise = False

isLeapYearCheck day year | year `mod` 400 == 0 && day >= 1 && day <= 29 = True
                         | year `mod` 100 == 0 && (day < 1 || day > 28) = False
                         | year `mod` 100 == 0 && day >= 1 && day <= 28 = True
                         | year `mod` 4   == 0 && day >= 1 && day <= 29 = True
			 | otherwise = False
