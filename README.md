# RPN
## Reverse polish notation algorithm in Haskell

#### Tests  

```convertToRPN "3 + 2 - 5"  
answer: "3 2 + 5 -"  

solveRPN "3 2 + 5 -"  
answer: 0.0  

convertToRPN "10 - ( 4 + 3 ) * 2"  
answer: "10 4 3 + 2 * -"  

solveRPN "10 4 3 + 2 * -"  
answer: -4.0  

convertToRPN "2 + 512 / 2 ^ 0.5 ^ 2"  
answer: "2 512 2 0.5 2 ^ ^ / +"  

solveRPN "2 512 2 0.5 2 ^ ^ / +"  
answer: 432.53897  

convertToRPN "( cos ( sin 0 ) + 2 ) ^ 2"  
answer: "0 sin cos 2 + 2 ^"  

solveRPN "0 sin cos 2 + 2 ^"  
answer: 9.0  

convertToRPN "a + b * c"  
answer: "a b c * +"  

convertToRPN "( a + b ) ^ c"  
answer: "a b + c ^"  

convertToRPN "cos a * ( b - c ) ^ d"  
answer: "a cos b c - d ^ *"  ```
