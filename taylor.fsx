open System

let epsilon = 1e-6 // Small precision value for floating-point comparisons

// Factorial function
let rec factorial n = if n = 0 then 1 else n * factorial (n - 1)

// Taylor series for sin(x) - Dumb Taylor series
let rec taylor_naive x n =
    let rec series_sum term_sum i =
        if i > n then term_sum
        else
            let term = ((-1.0) ** float i) * (x ** float (2 * i + 1)) / float (factorial (2 * i + 1))
            series_sum (term_sum + term) (i + 1)
    series_sum 0.0 0

// Improved Taylor series calculation with better convergence handling for sin(x)
let taylor x =
    let rec series_sum term_sum i =
        let term = ((-1.0) ** float i) * (x ** float (2 * i + 1)) / float (factorial (2 * i + 1))
        if abs term < epsilon then (term_sum, i) // Return the sum and the number of terms
        else series_sum (term_sum + term) (i + 1)
    series_sum 0.0 0

// Main function to display Taylor series computations
let main =
    let f = sin // Example function (sin in this case)
    let a = 0.0
    let b = 1.0
    let n = 10

    // Print table header
    printfn "------------------------------------------------------------------"
    printfn "|  x  |  Builtin  | Smart Taylor | terms1 | Dumb Taylor | terms2 |"
    printfn "------------------------------------------------------------------"

    // Generate table of values for Taylor series
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let dumbTaylorValue = taylor_naive x n
        let (smartTaylorValue, terms1) = taylor x
        let terms2 = n + 1 // Since dumbTaylor uses up to n terms
        printfn "| %5.2f | %10.6f | %10.6f | %6d | %10.6f | %6d |" x (f x) smartTaylorValue terms1 dumbTaylorValue terms2

    printfn "------------------------------------------------------------------"

// Run the main function
main
