//  Adapted from the Q# samples repository
//  Copyright (c) Microsoft Corporation.
//  Licensed under the MIT License.
//
/// # Sample
/// Grover's Search Algorithm
///
/// # Description
/// Grover's search algorithm is a quantum algorithm that finds with high
/// probability the unique input to a black box function that produces a
/// particular output value.
///
/// This Q# program implements the Grover's search algorithm.
import Std.Convert.*;
import Std.Math.*;
import Std.Arrays.*;
import Std.Measurement.*;
import Std.Diagnostics.*;

operation Main() : Result[] {
    // Example 3SAT problem with 10 variables (x0 to x9):
    // (x0 OR NOT x1 OR x2)
    // AND (NOT x3 OR x4 OR x5)
    // AND (x6 OR x7 OR NOT x8)
    // AND (NOT x0 OR x2 OR x9)
    // AND (x1 OR NOT x4 OR x7)
    // AND (x3 OR x5 OR NOT x6)
    // AND (x8 OR NOT x9 OR x0)
    // AND (NOT x2 OR x4 OR x6)
    // AND (x1 OR x3 OR NOT x5)
    // AND (x7 OR NOT x8 OR x9)
    let nQubits = 10;
    let problem = [
        // Clause 1: (x0 OR NOT x1 OR x2)
        [(0, false), (1, true), (2, false)],
        // Clause 2: (NOT x3 OR x4 OR x5)
        [(3, true), (4, false), (5, false)],
        // Clause 3: (x6 OR x7 OR NOT x8)
        [(6, false), (7, false), (8, true)],
        // Clause 4: (NOT x0 OR x2 OR x9)
        [(0, true), (2, false), (9, false)],
        // Clause 5: (x1 OR NOT x4 OR x7)
        [(1, false), (4, true), (7, false)],
        // Clause 6: (x3 OR x5 OR NOT x6)
        [(3, false), (5, false), (6, true)],
        // Clause 7: (x8 OR NOT x9 OR x0)
        [(8, false), (9, true), (0, false)],
        // Clause 8: (NOT x2 OR x4 OR x6)
        [(2, true), (4, false), (6, false)],
        // Clause 9: (x1 OR x3 OR NOT x5)
        [(1, false), (3, false), (5, true)],
        // Clause 10: (x7 OR NOT x8 OR x9)
        [(7, false), (8, true), (9, false)]
    ];
    let iterations = CalculateOptimalIterations(nQubits);
    Message($"Number of iterations: {iterations}");

    return GroverSearch(nQubits, iterations, problem);
}

/// # Summary
/// Implements Grover's algorithm, which searches all possible inputs to an
/// operation to find a particular marked state.
operation GroverSearch(
    nQubits : Int,
    iterations : Int,
    problem : (Int, Bool)[][]
) : Result[] {
    mutable foundSolutions : Int[] = [];
    mutable allResults : Result[][] = [];
    mutable done = false;
    mutable misses = 0;
    // We will keep searching until we find all solutions or we miss
    repeat {
        use qubits = Qubit[nQubits];
        PrepareUniform(qubits);
        for _ in 1..iterations {
            ReflectAbout3SatSolution(problem, qubits);
            // Exclude already found solutions
            for sol in foundSolutions {
                ReflectAboutMarkedSolution(sol, qubits);
            }
            ReflectAboutUniform(qubits);
        }
        let result = MResetEachZ(qubits);
        let intResult = ResultArrayAsInt(result);
        if (Is3SatSolution(problem, result)) {
            if (not ContainsInt(foundSolutions, intResult)) {
                set foundSolutions += [intResult];
                set allResults += [result];
                Message($"Found solution: {result}");
                misses = 0;
            }
            // Do not set done, keep searching
        } else {
            // If we miss, we will try again
            set misses += 1;
            Message($"Missed solution: {result}");
            if (misses > 100) {
                Message("Too many misses, stopping search.");
                set done = true;
            }
        }
    } until done fixup {};
    Message($"All solutions found: {allResults}");
    // Return the first solution if any, or an empty array
    if Length(allResults) > 0 {
        return allResults[0];
    } else {
        return [];
    }
}

/// # Summary
/// Returns the optimal number of Grover iterations needed to find a marked
/// item, given the number of qubits in a register.
function CalculateOptimalIterations(nQubits : Int) : Int {
    if nQubits > 126 {
        fail "This sample supports at most 126 qubits.";
    }

    let nItems = 2.0^IntAsDouble(nQubits);
    let angle = ArcSin(1. / Sqrt(nItems));
    let iterations = Round(0.25 * PI() / angle - 0.5);
    iterations
}

/// # Summary
/// Reflects about the basis state marked by alternating zeros and ones.
/// This operation defines what input we are trying to find in the search.
operation ReflectAboutMarked(inputQubits : Qubit[]) : Unit {
    Message("Reflecting about marked state...");
    use outputQubit = Qubit();
    within {
        // We initialize the outputQubit to (|0⟩ - |1⟩) / √2, so that
        // toggling it results in a (-1) phase.
        X(outputQubit);
        H(outputQubit);
        // Flip the outputQubit for marked states.
        // Here, we get the state with alternating 0s and 1s by using the X
        // operation on every other qubit.
        for q in inputQubits[...2...] {
            X(q);
        }
    } apply {
        Controlled X(inputQubits, outputQubit);
    }
}

/// # Summary
/// Given a register in the all-zeros state, prepares a uniform
/// superposition over all basis states.
operation PrepareUniform(inputQubits : Qubit[]) : Unit is Adj + Ctl {
    for q in inputQubits {
        H(q);
    }
}

/// # Summary
/// Reflects about the all-ones state.
operation ReflectAboutAllOnes(inputQubits : Qubit[]) : Unit {
    Controlled Z(Most(inputQubits), Tail(inputQubits));
}

/// # Summary
/// Reflects about the uniform superposition state.
operation ReflectAboutUniform(inputQubits : Qubit[]) : Unit {
    within {
        // Transform the uniform superposition to all-zero.
        Adjoint PrepareUniform(inputQubits);
        // Transform the all-zero state to all-ones
        for q in inputQubits {
            X(q);
        }
    } apply {
        // Now that we've transformed the uniform superposition to the
        // all-ones state, reflect about the all-ones state, then let the
        // within/apply block transform us back.
        ReflectAboutAllOnes(inputQubits);
    }
}

/// # Summary
/// Marks the input state if it satisfies the given 3SAT problem.
/// Each clause is an array of 3 tuples: (variable index, isNegated)
/// problem: Clause[][], where Clause = (Int, Bool)[]
/// inputQubits: Qubit[] encoding the solution vector
operation ReflectAbout3SatSolution(problem : (Int, Bool)[][], inputQubits : Qubit[]) : Unit {
    Message("Reflecting about 3SAT-satisfying state...");
    use outputQubit = Qubit();
    use clauseQubits = Qubit[Length(problem)];
    within {
        X(outputQubit);
        H(outputQubit);
        // For each clause, allocate an ancilla to store if the clause is satisfied
        for clauseIdx in 0 .. Length(problem) - 1 {
            let clause = problem[clauseIdx];
            use literalQubits = Qubit[3];
            for litIdx in 0..2 {
                let (varIdx, isNegated) = clause[litIdx];
                if isNegated {
                    X(inputQubits[varIdx]);
                }
                CNOT(inputQubits[varIdx], literalQubits[litIdx]);
                if isNegated {
                    X(inputQubits[varIdx]);
                }
            }
            // OR the 3 literalQubits into clauseQubits[clauseIdx]
            CNOT(literalQubits[0], clauseQubits[clauseIdx]);
            CNOT(literalQubits[1], clauseQubits[clauseIdx]);
            CNOT(literalQubits[2], clauseQubits[clauseIdx]);
            // Uncompute literalQubits
            for litIdx in 2..-1..0 {
                let (varIdx, isNegated) = clause[litIdx];
                if isNegated {
                    X(inputQubits[varIdx]);
                }
                CNOT(inputQubits[varIdx], literalQubits[litIdx]);
                if isNegated {
                    X(inputQubits[varIdx]);
                }
            }
        }
    } apply {
        // If all clauseQubits are 1, flip the outputQubit (multi-controlled X)
        Controlled X(clauseQubits, outputQubit);
    }
    // clauseQubits are automatically released/reset by Q#
}

/// # Summary
/// Converts a Result[] array to an Int representation.
function ResultArrayAsInt(results : Result[]) : Int {
    mutable value = 0;
    for idx in 0..Length(results)-1 {
        if results[idx] == One {
            set value += 1 <<< idx;
        }
    }
    return value;
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

/// # Summary
/// Helper: Check if an int is in an array.
function ContainsInt(arr : Int[], value : Int) : Bool {
    for x in arr {
        if x == value { return true; }
    }
    return false;
}

/// # Summary
/// Oracle to mark a specific solution, excluding it in future searches.
operation ReflectAboutMarkedSolution(solution : Int, inputQubits : Qubit[]) : Unit {
    use outputQubit = Qubit();
    within {
        X(outputQubit);
        H(outputQubit);
        for idx in 0..Length(inputQubits)-1 {
            if ((solution &&& (1 <<< idx)) == 0) {
                X(inputQubits[idx]);
            }
        }
    } apply {
        Controlled X(inputQubits, outputQubit);
        // Uncompute inputQubits flips
        for idx in 0..Length(inputQubits)-1 {
            if ((solution &&& (1 <<< idx)) == 0) {
                X(inputQubits[idx]);
            }
        }
    }
}