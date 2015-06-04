import System.Random
import Control.Monad
import System.IO

numIterations = 1000
gradientStep = 0.01
datasize = 784

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

addListOfLists::[[Float]]->[Float]
addListOfLists [[]] = take datasize (repeat 0.0)
addListOfLists (h:t) = zipWith (+) h (addListOfLists t)


totalGradientError::Int->[[Float]]->[Float]->[Float]
totalGradientError dataArray currentWeightVec = addListOfLists(map (\dataVec->gradientError currentWeightVec dataVec False) dataArray)


getRandomVec::Int -> IO [Float]
getRandomVec size = replicateM size (randomIO :: IO Float) 


wordsWhen::(Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

char2float :: Char -> Float
char2float n = fromInteger (read [n])

--Not happy about these two functions, need to make them more clean..
-- ******************************************************* 
convertLineToVec :: String -> [Float]
convertLineToVec s = map (\c -> (char2float (c !! 0))) (wordsWhen (','==) s)

readData inh dataArray = do
	ineof <- hIsEOF inh
	if ineof then return dataArray
		else do 
			inpStr <- hGetLine inh
			readData inh (dataArray ++ [convertLineToVec inpStr])

-- ********************************************************

{-iterate i weightVec dataArray = if i==0 then weightVec 
									else iterate i-1 weightVec + map(gradientStep *-}





		





