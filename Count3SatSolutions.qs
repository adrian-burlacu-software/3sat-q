// Helper Q# function to count the number of solutions for the current 3SAT problem
open Microsoft.Quantum.Intrinsic;
open Microsoft.Quantum.Canon;
open Microsoft.Quantum.Arrays;
open Microsoft.Quantum.Core;

function Count3SatSolutions() : Int {
    let nQubits = 10;
    let problem = [
         // Clause 1: (x0 OR x1 OR x2)
        [(0, false), (1, false), (2, false)],
        // Clause 2: (NOT x0 OR NOT x1 OR x3)
        [(0, true), (1, true), (3, false)],
        // Clause 3: (x0 OR NOT x2 OR x4)
        [(0, false), (2, true), (4, false)],
        // Clause 4: (NOT x1 OR x2 OR NOT x3)
        [(1, true), (2, false), (3, true)],
        // Clause 5: (x3 OR x4 OR x5)
        [(3, false), (4, false), (5, false)],
        // Clause 6: (NOT x3 OR NOT x4 OR x6)
        [(3, true), (4, true), (6, false)],
        // Clause 7: (x4 OR NOT x5 OR x7)
        [(4, false), (5, true), (7, false)],
        // Clause 8: (NOT x4 OR x5 OR NOT x6)
        [(4, true), (5, false), (6, true)],
        // Clause 9: (x6 OR x7 OR x8)
        [(6, false), (7, false), (8, false)],
        // Clause 10: (NOT x6 OR NOT x7 OR x9)
        [(6, true), (7, true), (9, false)],
        // Clause 11: (x7 OR NOT x8 OR x0)
        [(7, false), (8, true), (0, false)],
        // Clause 12: (NOT x7 OR x8 OR NOT x9)
        [(7, true), (8, false), (9, true)],
        // Clause 13: (x0 OR x5 OR NOT x8)
        [(0, false), (5, false), (8, true)],
        // Clause 14: (NOT x1 OR x6 OR x9)
        [(1, true), (6, false), (9, false)],
        // Clause 15: (x2 OR NOT x5 OR x7)
        [(2, false), (5, true), (7, false)],
        // Clause 16: (NOT x2 OR x6 OR NOT x9)
        [(2, true), (6, false), (9, true)],
        // Clause 17: (x0 OR x1 OR NOT x9)
        [(0, false), (1, false), (9, true)],
        // Clause 18: (NOT x0 OR NOT x1 OR x8)
        [(0, true), (1, true), (8, false)],
        // Clause 19: (x2 OR x3 OR NOT x7)
        [(2, false), (3, false), (7, true)],
        // Clause 20: (NOT x2 OR NOT x3 OR x6)
        [(2, true), (3, true), (6, false)],
        // Clause 21: (x4 OR x5 OR NOT x0)
        [(4, false), (5, false), (0, true)],
        // Clause 22: (NOT x4 OR NOT x5 OR x1)
        [(4, true), (5, true), (1, false)],
        // Clause 23: (x6 OR x7 OR NOT x2)
        [(6, false), (7, false), (2, true)],
        // Clause 24: (NOT x6 OR NOT x7 OR x3)
        [(6, true), (7, true), (3, false)],
        // Clause 25: (x8 OR x9 OR NOT x4)
        [(8, false), (9, false), (4, true)]
    ];
    mutable count = 0;
    for i in 0..(2^nQubits - 1) {
        mutable assignment : Result[] = [];
        for idx in 0..nQubits-1 {
            set assignment += [(if ((i &&& (1 <<< idx)) != 0) { One } else { Zero })];
        }
        if (Is3SatSolution(problem, assignment)) {
            set count += 1;
        }
    }
    Message($"Number of solutions: {count}");
    return count;
}

/// # Summary
/// Checks if a given result satisfies the 3SAT problem.
function Is3SatSolution(problem : (Int, Bool)[][], results : Result[]) : Bool {
    for clause in problem {
        mutable clauseSat = false;
        for (varIdx, isNegated) in clause {
            let bit = results[varIdx] == One ? true | false;
            let litSat = (isNegated and not bit) or (not isNegated and bit);
            if litSat { set clauseSat = true; }
        }
        if not clauseSat { return false; }
    }
    return true;
}
