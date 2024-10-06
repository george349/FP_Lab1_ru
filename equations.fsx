open System

// Function to print the table header
let printHeader =
    printfn "\n----------------------------------"
    printfn "|        Root-Finding Results    |"
    printfn "----------------------------------"
    printfn "|    Method     |      Value     |"
    printfn "----------------------------------"

let tolerance = 0.0001

let absolute x =
    if x < 0.0 then 
        -x
    else x

// Dichotomy method
let dichotomy f a b = 
    let rec cycle a b =
        let midpoint = 0.5 * (a + b)
        if absolute (b - a) < tolerance then 
            midpoint
        else
            if (f a) * (f midpoint) < 0.0 then
                cycle a midpoint
            else
                cycle midpoint b
    cycle a b

// Iterations method
let iterations phi x0 = 
    let rec cycle x =
        if (absolute ((phi x) - x) < tolerance) then
            phi x
        else
            cycle (phi x)
    cycle x0

// Newton's method
let newton f f' x0 = 
    let phi x = x - (f x) / (f' x)
    iterations phi x0

// Define the functions for your variant
let fun1 x = 0.6 * 3.0 ** x - 2.3 * x - 3.0
let fun2 x = x ** 2.0 - Math.Log(1.0 + x) - 3.0 
let fun3 x = 2.0 * x * Math.Sin x - Math.Cos x

// Define the derivatives of the functions
let fun1' x = 0.6 * Math.Log 3.0 * 3.0 ** x - 2.3
let fun2' x = 2.0 * x - 1.0 / (1.0 + x) // Fixed the derivative for f2
let fun3' x = 2.0 * Math.Sin x + 2.0 * x * Math.Cos x

// Define phi functions for iteration methods
let phifun1 x = x - (f1 x) / (f1' x)
let phifun2 x = x - (f2 x) / (f2' x)
let phifun3 x = x - (f3 x) / (f3' x)

// Main function to run the methods and display results
let main = 
    printHeader
    printfn "| %12s | %15.5f |" "Dichotomy" (dichotomy fun1 2.0 3.0)
    printfn "| %12s | %15.5f |" "Iterations" (iterations phifun1 2.5)
    printfn "| %12s | %15.5f |" "Newton" (newton fun1 fun1' 2.5)
    printfn "----------------------------------"
    
    printfn "| %12s | %15.5f |" "Dichotomy" (dichotomy fun2 2.0 3.0)
    printfn "| %12s | %15.5f |" "Iterations" (iterations phifun2 2.5)
    printfn "| %12s | %15.5f |" "Newton" (newton fun2 fun2' 2.5)
    printfn "----------------------------------"

    printfn "| %12s | %15.5f |" "Dichotomy" (dichotomy fun3 0.4 1.0)
    printfn "| %12s | %15.5f |" "Iterations" (iterations phifun3 0.7)
    printfn "| %12s | %15.5f |" "Newton" (newton fun3 fun3' 0.7)
    printfn "----------------------------------"

// Run the main function
main
