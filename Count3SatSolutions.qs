// Helper Q# function to count the number of solutions for the current 3SAT problem
open Microsoft.Quantum.Intrinsic;
open Microsoft.Quantum.Canon;
open Microsoft.Quantum.Arrays;
open Microsoft.Quantum.Core;

function Count3SatSolutions() : Int {
    let nQubits = 8;
    let problem = [
        [(0, false), (1, false), (2, false)],
        [(0, true), (3, false), (4, false)],
        [(2, false), (3, true), (5, false)],
        [(1, false), (4, true), (6, false)],
        [(1, true), (5, false), (7, false)],
        [(3, false), (6, true), (7, false)],
        [(2, true), (4, false), (5, true)],
        [(0, false), (6, true), (7, true)],
        [(0, true), (1, true), (2, true)],
        [(3, true), (4, true), (5, true)]
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
