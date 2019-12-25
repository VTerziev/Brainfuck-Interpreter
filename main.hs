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


right :: Operator
right (i, (ram, ip, dp, brackets), o) = (i, (ram, ip+1, dp+1, brackets), o)

left :: Operator
left (i, (ram, ip, 0, brackets), o) = (i, (ram, ip+1, 0, brackets), o) -- should throw an exception
left (i, (ram, ip, dp, brackets), o) = (i, (ram, ip+1, dp-1, brackets), o)

changeElement :: Int -> ( a -> a ) -> [a] -> [a] -- change element at index I, with operation F, in an array
changeElement i f arr = (take i arr)++[f (arr!!i)]++(drop (i+1) arr)

add1 :: Operator
add1 (i, (ram, ip, dp, brackets), o) = (i, (changeElement dp (+1) ram, ip+1, dp, brackets), o)

rem1 :: Operator 
rem1 (i, (ram, ip, dp, brackets), o) = (i, (changeElement dp (+(-1)) ram, ip+1, dp, brackets), o)

read :: Operator
read([], st, o) = ([], st, o) -- should throw an exception
read (i, (ram, ip, dp, brackets), o) = (tail i, (changeElement dp ((\x -> (head i))) ram, ip+1, dp, brackets), o)

print :: Operator
print (i, (ram, ip, dp, brackets), o) = (i, (ram, ip+1, dp, brackets), o++[ram!!dp])

openBr :: Operator
openBr(i, (ram, ip, dp, brackets), o) = (i, (ram, ip+1, dp, ip:brackets), o)

closeBr :: Operator
closeBr(i, (ram, ip, dp, []), o) = (i, (ram, ip, dp, []), o) -- should throw an exception
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

kappa :: String -> State -> State
kappa program st = if getIp st >= length program 
 then st
 else kappa program (wrap (program!!(getIp st)) st)

process :: String -> Input -> Output
process program i = getOutput (kappa program (i,([0,0..], 0, 0, []),[]))

load :: String -> Program 
load program = process program





