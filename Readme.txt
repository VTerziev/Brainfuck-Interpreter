Brainfuck Interpreter

The main type, that's used is a Program- it take a stream of integers as input and produces a stream of integers as output.

There's a function, called 'load' which is given a source code of a BF program and it returns a program, that corresponds to the given code. 
After that the resulting program can be fed with any input.
Example: 
g = load ",++." // the program reads a number, increases it with 2 and prints it 
g [3] // run the program with a single number in the input stream 
 -> [5]  // the program returns a single number, which is equal to 3+2

Of course, if the source code or the input is illegal, a proper error message will be shown. 

There's another function, called 'concat' which is given two programs p and q, and returns a program r = p( q ( input) )
Example:
swap = load ",>,.<." // a program that reads two numbers and swaps them 
id = concat swap swap // swap twice

id [2,3] // run id with two numbers
 -> [2,3]  

 There's also 'parallel' and 'alter'. 
 The first one takes two programs p and q, which will recieve the same input, executes them in parralel and returns the outputs of them both, concatenated.
 The second one takes p and q, and returns a program, that alternates giving the input to p and q. 

 The source code can be given as a string, but it can be also given as binary data. The operations are encoded with 3 bits. To use that kind of input, use the 'readBin' function, which parses the binary data to a string. 
 If you're having troubles generating a binary source code- you can use the 'writeBin' which parses a string to binary data. 

If you really want to read or write from files, you can use the 'readFile' and 'writeFile' functions. 
Example:
sourceCode <- readFile "someFile.txt"
f = load sourceCode
...

Finding the shortest program, that matches an input to an output:
1. All possible programs are stored in Main.all 
2. You can run them on some input with 'runAll $input' 
3. There's a function 'match' which finds the shortest program that produces a given output

Example:
match [] [0] 
 -> '.' // just print the value of the cell, pointed by the DP

