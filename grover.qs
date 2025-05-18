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

operation Main() : Unit {
    // Example 3SAT problem with 10 variables (x0 to x9) with few(17) solutions:
    // (x0 OR x1 OR x2)
    // AND (NOT x0 OR NOT x1 OR x3)
    // AND (x0 OR NOT x2 OR x4)
    // AND (NOT x1 OR x2 OR NOT x3)
    // AND (x3 OR x4 OR x5)
    // AND (NOT x3 OR NOT x4 OR x6)
    // AND (x4 OR NOT x5 OR x7)
    // AND (NOT x4 OR x5 OR NOT x6)
    // AND (x6 OR x7 OR x8)
    // AND (NOT x6 OR NOT x7 OR x9)
    // AND (x7 OR NOT x8 OR x0)
    // AND (NOT x7 OR x8 OR NOT x9)
    // AND (x0 OR x5 OR NOT x8)
    // AND (NOT x1 OR x6 OR x9)
    // AND (x2 OR NOT x5 OR x7)
    // AND (NOT x2 OR x6 OR NOT x9)
    // AND (x0 OR x1 OR NOT x9)
    // AND (NOT x0 OR NOT x1 OR x8)
    // AND (x2 OR x3 OR NOT x7)
    // AND (NOT x2 OR NOT x3 OR x6)
    // AND (x4 OR x5 OR NOT x0)
    // AND (NOT x4 OR NOT x5 OR x1)
    // AND (x6 OR x7 OR NOT x2)
    // AND (NOT x6 OR NOT x7 OR x3)
    // AND (x8 OR x9 OR NOT x4)
    let nQubits = 10;
    let nMisses = 100;
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
    let nClauses = Length(problem);
    let estimatedSolutions = EstimateNumSolutionsStatistical(nQubits, nClauses);
    Message($"Estimated number of solutions (statistical): {estimatedSolutions}");
    // Use (Round(x) as Int) for conversion
    let nSolutions = Round(estimatedSolutions);
    let iterations = CalculateOptimalIterations(nQubits, nSolutions);
    Message($"Number of iterations: {iterations}");

    let missedCount = GroverSearch(nQubits, iterations, problem, nMisses);
    Message($"Number of misses: {missedCount}");

    return ();
}

/// # Summary
/// Implements Grover's algorithm, which searches all possible inputs to an
/// operation to find a particular marked state.
operation GroverSearch(
    nQubits : Int,
    iterations : Int,
    problem : (Int, Bool)[][],
    nMisses : Int
) : Int {
    mutable foundSolutions : Int[] = [];
    mutable allResults : Result[][] = [];
    mutable done = false;
    mutable misses = 0;
    mutable missesCount = 0;

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
                Message($"Found solution: {Length(foundSolutions)}");
                misses = 0;
            }
            else {
              set misses += 1;
              set missesCount += 1;
              // Message($"Already found solution: {result}");
            }
            // Do not set done, keep searching
        } else {
            // If we miss, we will try again
            set misses += 1;
            set missesCount += 1;
            // Message($"Missed solution: {result}");
        }

        // Check if we have missed too many times
        if (misses > nMisses) {
            Message("Too many misses, stopping search.");
            set done = true;
        }
    } until done fixup {};
    Message($"All solutions found: {allResults}");
    Message($"Number of found solutions: {Length(foundSolutions)}");

    return missesCount;
}

/// # Summary
/// Returns the optimal number of Grover iterations needed to find a marked
/// item, given the number of qubits in a register and the number of solutions.
function CalculateOptimalIterations(nQubits : Int, nSolutions : Int) : Int {
    if nQubits > 126 {
        fail "This sample supports at most 126 qubits.";
    }
    let nItems = 2.0^IntAsDouble(nQubits);
    if nSolutions > 0 {
        // Use formula for multiple solutions: k ≈ π/4 * sqrt(N/M)
        let iterations = Round((PI() / 4.0) * Sqrt(nItems / IntAsDouble(nSolutions)));
        return iterations;
    } else {
        // Fallback to original formula for single solution
        let angle = ArcSin(1. / Sqrt(nItems));
        let iterations = Round((0.25 * PI() / angle - 0.5) * 1.0);
        return iterations;
    }
}

/// # Summary
/// Estimates the number of solutions to a 3SAT problem using the statistical independence assumption.
function EstimateNumSolutionsStatistical(nQubits : Int, nClauses : Int) : Double {
    let nAssignments = 2.0 ^ IntAsDouble(nQubits);
    let probSatisfy = (7.0 / 8.0) ^ IntAsDouble(nClauses);
    return nAssignments * probSatisfy;
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

            // Corrected logic to compute (L0 OR L1 OR L2) into clauseQubits[clauseIdx]
            // clauseQubits[clauseIdx] is initially |0>. We want it to be |1> if clause is satisfied, |0> otherwise.
            // A clause (L0 or L1 or L2) is satisfied if NOT ((NOT L0) AND (NOT L1) AND (NOT L2))
            // literalQubits currently hold L0, L1, L2.

            // 1. Temporarily flip literalQubits to get (NOT L0), (NOT L1), (NOT L2)
            X(literalQubits[0]);
            X(literalQubits[1]);
            X(literalQubits[2]);

            // 2. Compute ((NOT L0) AND (NOT L1) AND (NOT L2)) into clauseQubits[clauseIdx]
            //    clauseQubits[clauseIdx] will become |1> if L0,L1,L2 were all false (i.e., clause unsatisfied by them).
            Controlled X(literalQubits, clauseQubits[clauseIdx]);

            // 3. Flip clauseQubits[clauseIdx] to represent clause satisfaction.
            //    Now, clauseQubits[clauseIdx] is |1> if the clause IS satisfied, and |0> otherwise.
            X(clauseQubits[clauseIdx]);

            // 4. Unflip literalQubits to restore their original state (L0, L1, L2)
            //    This is crucial for the automatic uncomputation of the 'use literalQubits' block,
            //    as the 'within' block's adjoint needs them in this state to correctly uncompute
            //    the CNOTs that prepared them from inputQubits.
            X(literalQubits[0]);
            X(literalQubits[1]);
            X(literalQubits[2]);
            // End of corrected clause logic

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
        Controlled X(clauseQubits, outputQubit);
    }
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
    }
}