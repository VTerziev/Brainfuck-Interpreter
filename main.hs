import Control.Exception
import Data.Typeable
import qualified Data.ByteString.Lazy as BinL

type InternalState = ([Int], Int, Int, [Int]) -- RAM, ip, dp, ['s indexes
type Input = [Int]
type Output = [Int]
type State = (Input, InternalState, Output)
type Operator = State -> State
type Program = Input -> Output

getIp :: State -> Int
getIp (_, (_, ip, _, _), _) = ip

getOutput :: State -> Output
getOutput (_, _, o) = o;

ramSize = 256

right :: Operator
right (i, (ram, ip, dp, brackets), o) = if dp+1 > ramSize 
  then error "Dp index went too far right"
  else (i, (ram, ip+1, dp+1, brackets), o)

left :: Operator
left (i, (ram, ip, 0, brackets), o) = error "Dp index went too far left"
left (i, (ram, ip, dp, brackets), o) = (i, (ram, ip+1, dp-1, brackets), o)

changeElement :: Int -> ( a -> a ) -> [a] -> [a] -- change element at index I, with operation F, in an array
changeElement i f arr = (take i arr)++[f (arr!!i)]++(drop (i+1) arr)

add1 :: Operator
add1 (i, (ram, ip, dp, brackets), o) = (i, (changeElement dp (+1) ram, ip+1, dp, brackets), o)

rem1 :: Operator 
rem1 (i, (ram, ip, dp, brackets), o) = (i, (changeElement dp (+(-1)) ram, ip+1, dp, brackets), o)

read :: Operator
read([], st, o) = error "No elements left in the input stream" 
read (i, (ram, ip, dp, brackets), o) = (tail i, (changeElement dp ((\x -> (head i))) ram, ip+1, dp, brackets), o)

print :: Operator
print (i, (ram, ip, dp, brackets), o) = (i, (ram, ip+1, dp, brackets), o++[ram!!dp])

openBr :: Operator
openBr(i, (ram, ip, dp, brackets), o) = (i, (ram, ip+1, dp, ip:brackets), o)

closeBr :: Operator
closeBr(i, (ram, ip, dp, []), o) = error "Mismatching brackets" 
closeBr(i, (ram, ip, dp, brackets), o) = if ((ram!!dp) == 0) 
  then (i, (ram, ip+1, dp, tail brackets), o)  
  else (i, (ram, (head brackets)+1, dp, brackets), o)

wrap :: Char -> State -> State
wrap '+' st = add1 st
wrap '-' st = rem1 st
wrap '.' st = Main.print st
wrap ',' st = Main.read st
wrap '>' st = right st
wrap '<' st = left st
wrap '[' st = openBr st
wrap ']' st = closeBr st
wrap x (i, (ram, ip, dp, br), o) = (i, (ram, ip+1, dp, br), o)

kappa :: String -> State -> State
kappa program st = if getIp st >= length program 
 then st
 else kappa program (wrap (program!!(getIp st)) st)

process :: String -> Input -> Output
process program i = getOutput (kappa program (i,([0,0..], 0, 0, []),[]))

load :: String -> Program 
load program = process program

loadBin :: [Int] -> Program
loadBin programBin = process (readBin programBin)

concat :: Program -> Program -> Program
concat p q i = p ( q i ) 

odds [] = []
odds [x] = []
odds (e1:e2:xs) = e2 : odds xs

evens [] = []
evens [x] = [x]
evens (e1:e2:xs) = e1 : evens xs

readBin :: [Int] -> String
readBin [] = []
readBin (0:0:0:xs) = '+':readBin xs
readBin (0:0:1:xs) = '-':readBin xs
readBin (0:1:0:xs) = '.':readBin xs
readBin (0:1:1:xs) = ',':readBin xs
readBin (1:0:0:xs) = '>':readBin xs
readBin (1:0:1:xs) = '<':readBin xs
readBin (1:1:0:xs) = '[':readBin xs
readBin (1:1:1:xs) = ']':readBin xs

writeBin [] = []
writeBin ('+':xs) = 0:0:0:(writeBin xs)
writeBin ('-':xs) = 0:0:1:(writeBin xs)
writeBin ('.':xs) = 0:1:0:(writeBin xs)
writeBin (',':xs) = 0:1:1:(writeBin xs)
writeBin ('>':xs) = 1:0:0:(writeBin xs)
writeBin ('<':xs) = 1:0:1:(writeBin xs)
writeBin ('[':xs) = 1:1:0:(writeBin xs)
writeBin (']':xs) = 1:1:1:(writeBin xs)

parallel :: Program -> Program -> Program
parallel p q i = (p i)++(q i)

alter :: Program -> Program -> Program
alter p q i = (p (odds i))++(q (evens i)) 

makeSafe :: Program -> Program -- instead of throwing an exception, the program should not halt
makeSafe p i = p i 

generatePrograms :: Int -> [[Int]]
generatePrograms 1 = [[0], [1]]
generatePrograms len = [ (y:x) | x <- (generatePrograms (len-1)), y <- [0,1] ]

all = Prelude.concat [ (generatePrograms x) | x <- [3,6..] ]

runAll :: Input -> [(String,Output)] -- returning pair of (The source code, The output)
runAll i = [ (readBin p, (makeSafe (load (readBin p))) i) | p <- Main.all ]

match :: Input -> Output -> String
match i o = head [ fst possible | possible <- (runAll i), (snd possible == o) ]

