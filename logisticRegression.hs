import System.Random
import Control.Monad
import System.IO

data Bs a = Bullshit a deriving(Show)


monadbreak (Bullshit val) = val

-- | ∑i(y(i)log(hθ(x(i)))+(1−y(i))log(1−hθ(x(i)))).
dot::(Num a) => [a]->[a]->a
dot [] [] = 0
dot (first:rest) (first2:rest2) = (first * first2) + (dot rest rest2)

hypothesis::(Floating a) => [a]->[a]->a
hypothesis weightvec datavec = 1.0 / (1.0 + exp(-dot weightvec datavec))

-- | ∑i(y(i)log(hθ(x(i)))+(1−y(i))log(1−hθ(x(i)))).
logisticError::(Floating a) => [a]->[a]->Bool->a
logisticError weightvec datavec label = if label then log(hypothesis weightvec datavec) 
									else (1.0 - log(hypothesis weightvec datavec))

totalError::(Floating a) => [a]->[[a]]->[Bool]->a
totalError _ [] [] = 0.0
totalError weightvec (datavec:datavecs) (label:labels) = (logisticError weightvec datavec label) 
															+ (totalError weightvec datavecs labels)

gradientError::(Floating a) => [a]->[a]->Bool->[a]
gradientError weightvec datavec label = 
									let x = (hypothesis weightvec datavec) in 
									if label then map ((x - 1.0) * ) datavec
										else map (x * ) datavec

getRandomVec :: Int -> IO [Float]
getRandomVec size = replicateM size (randomIO :: IO Float) 


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'




main = do 
	inh <- openFile "test.txt" ReadMode
	inpStr <- hGetLine inh
	vec <- getRandomVec 10
	putStrLn (show vec ++ "    " ++ show inpStr)

		





