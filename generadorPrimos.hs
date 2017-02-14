import Control.Monad
import Data.Char

divisible::Int->Int->Bool
divisible x y = (mod x y) ==0

divisibles::Int->[Int]
divisibles x = [y | y <-[1..x],divisible x y]

esPrimo::Int->Bool
esPrimo n = length (divisibles n) <=2

primos::Int->[Int]
primos n = [x | x <-[1..n],esPrimo x]

generar = filter esPrimo [1..]

generarHasta num = filter esPrimo [1..num]

generarDesdeHasta min max = filter esPrimo [min..max]