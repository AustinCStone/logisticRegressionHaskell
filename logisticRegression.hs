import System.Random
import Control.Monad
import System.IO
import Debug.Trace

numIterations = 10000
alpha = 0.01
datasize = 784




data Image = Image { label :: Bool 
			  , imagevec::[Float]
} deriving (Show)

-- scalar multiply
sm::(Num a)=>a->[a]->[a]
sm scalar vec = map (\x->scalar*x) vec

-- add two vectors
vadd::(Num a)=>[a]->[a]->[a]
vadd vec1 vec2 = zipWith (+) vec1 vec2

--dot two vectors
dot::(Num a) => [a]->[a]->a
dot [] [] = 0
dot (first:rest) (first2:rest2) = (first * first2) + (dot rest rest2)

normalize vec = sm (1.0/(dot vec vec)) vec

hypothesis::(Floating a) => [a]->[a]->a
hypothesis weightvec datavec = 1.0 / (1.0 + exp(-dot weightvec datavec))

-- | ∑i(y(i)log(hθ(x(i)))+(1−y(i))log(1−hθ(x(i)))).
logisticError::(Floating a) => [a]->[a]->Bool->a
logisticError weightvec datavec label = if label then log(hypothesis weightvec datavec) 
									else (1.0 - log(hypothesis weightvec datavec))

totalError :: [Float] -> [Image] -> Float
totalError _ [] = 0.0
totalError weightvec (image:images) = (logisticError weightvec (imagevec image) (label image))
															+ (totalError weightvec images)

gradientError::(Floating a) => [a]->[a]->Bool->[a]
gradientError weightvec datavec label = 
									let x = (hypothesis weightvec datavec) in 
									if label then map ((x - 1.0) * ) datavec
										else map (x * ) datavec

addListOfLists::[[Float]]->[Float]
addListOfLists [[]] = take datasize (repeat 0.0)
addListOfLists (h:[]) = h
addListOfLists (h:t) = vadd h (addListOfLists t)

totalGradientError::[Image]->[Float]->[Float]
totalGradientError trainData currentweightvec = addListOfLists (map 
	(\image->(gradientError currentweightvec (imagevec(image)) (label(image))) ) 
	trainData)

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

readData :: Handle -> [Image] -> Bool -> IO [Image]
readData inh trainData label = do
	ineof <- hIsEOF inh
	if ineof then return (tail trainData)
		else do 
			inpStr <- hGetLine inh
			readData inh (trainData ++ [(Image {label=label, imagevec=(convertLineToVec inpStr)} )] )  label
-- ********************************************************


converge i weightvec trainData | trace ("Iteration " ++ show i ++ " Total Error: " ++ show (totalError weightvec trainData))  False = undefined
converge i weightvec trainData = if i==0 then weightvec 
									else converge (i-1) (normalize  (vadd 
										weightvec
										(sm alpha (vadd (totalGradientError trainData weightvec) weightvec))) )
										trainData

-- | interleave two lists
interleave::[a]->[a]->[a]
interleave [] ys = ys
interleave (x:xs) ys = x:interleave ys xs

normalizeData::[Image]->[Image]
normalizeData (image:[]) = let nimagevec = normalize(imagevec image) in  
	[Image{label = label(image), imagevec=nimagevec}]
normalizeData (image:images) = let nimagevec = normalize(imagevec image) in 
	[Image{label = label(image), imagevec=nimagevec}] ++ (normalizeData images)



main = do
	inh1 <- openFile "test1.txt" ReadMode
	inh0 <- openFile "test0.txt" ReadMode
	outh <- openFile "output.txt" WriteMode
	trainData1 <- readData inh1 [] True
	trainData0 <- readData inh0 [] False
	initweightvec <- getRandomVec datasize
	putStrLn $ show (converge numIterations (normalize initweightvec) (normalizeData(interleave trainData0 trainData1)))




