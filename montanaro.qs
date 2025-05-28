//  Montanaro's Quantum Backtracking Algorithm for 3SAT
//  Adapted for Q# and this project from "Quantum Walk Speedup of Backtracking Algorithms" by Ashley Montanaro
//  https://arxiv.org/abs/1509.02374
//
//  This file provides a detailed, tutorial-style implementation of Montanaro's quantum backtracking algorithm
//  to solve 3SAT, reusing Grover's utilities and the 3SAT problem representation from grover.qs.
//  See README for references and usage.
//
//  ---------------------------------------------------------------------
//  Overview:
//
//  Montanaro's algorithm is a quantum speedup of classical backtracking algorithms for constraint satisfaction problems (CSPs).
//  It recursively explores the solution tree, using quantum walks to amplify paths that lead to valid solutions.
//  For 3SAT, each node corresponds to a partial assignment; the algorithm branches on variable assignments,
//  recursively searching for a satisfying complete assignment.
//
//  This implementation uses Grover's search as a subroutine to quickly check for solutions in subspaces.
//
//  ---------------------------------------------------------------------
//  Usage:
//    - Run the MontanaroMain operation as the entry point.
//    - The sample problem is reused from grover.qs (10 variables, 25 clauses).
//
//  ---------------------------------------------------------------------

import Std.Convert.*;
import Std.Math.*;
import Std.Arrays.*;
import Std.Measurement.*;
import Std.Diagnostics.*;

@EntryPoint()
operation MontanaroMain() : Unit {
    // Sample problem: same as in grover.qs
    let nQubits = 10;
    let nMisses = 10;
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

    Message("Starting Montanaro's quantum backtracking algorithm for 3SAT.");
    let result = MontanaroBacktracking(problem, nQubits);
    if (result::found) {
        Message($"Found satisfying assignment: {result::solution}");
    } else {
        Message("No satisfying assignment found.");
    }
}

/// # Summary
/// MontanaroBacktracking implements the quantum backtracking algorithm for 3SAT.
/// It recursively explores variable assignments, using Grover's search as a quantum subroutine
/// to check for the existence of solutions in subspaces.
///
/// Returns: Result type with the solution assignment as an Int, or NotFound if no solution exists.
operation MontanaroBacktracking(
    problem : (Int, Bool)[][],
    nQubits : Int
) : ResultType {
    // Start recursive search with all variables unassigned.
    return MontanaroRecursive(problem, nQubits, PartialAssignment([]));
}

newtype PartialAssignment = (Int, Bool)[]; // Array of (variable index, value) pairs

// ResultType encodes the result of backtracking.
newtype ResultType = (found : Bool, solution : Int);

/// # Summary
/// Recursive helper for Montanaro's backtracking algorithm.
/// - At each recursion, try assigning True/False to the next unassigned variable.
/// - For each child, use Grover's search to check if a solution exists extending that partial assignment.
/// - If a solution is found, recurse down that branch.
/// - If all branches fail, return NotFound.
/// - If all variables are assigned, check if the assignment satisfies the formula.
operation MontanaroRecursive(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assigned : PartialAssignment
) : ResultType {
    // If all variables are assigned, check solution directly.
    if (Length(assigned!) == nQubits) {
        // Convert assignment to Result[] for Is3SatSolution
        let results = AssignmentToResultArray(assigned, nQubits);
        if (Is3SatSolution(problem, results)) {
            let solutionInt = ResultArrayAsInt(results);
            return ResultType(true, solutionInt);
        } else {
            return ResultType(false, 0);
        }
    }
    // Otherwise, branch on next unassigned variable.
    let nextVar = Length(assigned!); // Assign variables in order for simplicity.
    mutable childResults : ResultType[] = [];
    for value in [false, true] {
        let newAssignment = PartialAssignment(assigned! + [(nextVar, value)]);
        // Use Grover's search to check if this partial assignment can extend to a full solution.
        if (MontanaroGroverSubspaceSearch(problem, nQubits, newAssignment)) {
            // Recurse down this branch.
            let res = MontanaroRecursive(problem, nQubits, newAssignment);
            if (res::found) {
                return res; // Found solution
            }
            // else continue to other branch
        }
    }
    // No branch led to a solution.
    return ResultType(false, 0);
}

/// # Summary
/// Checks (using Grover's algorithm) whether any extension of a given partial assignment
/// leads to a solution of the 3SAT instance.
/// Returns true if a solution exists, false otherwise.
/// This is the quantum speedup: instead of classical checking, leverages Grover to search subspaces.
operation MontanaroGroverSubspaceSearch(
    problem : (Int, Bool)[][],
    nQubits : Int,
    partial : PartialAssignment
) : Bool {
    // Calculate the number of unassigned qubits (subspace dimension)
    let nUnassigned = nQubits - Length(partial!);
    
    // If all qubits are assigned, just check if it's a solution
    if (nUnassigned == 0) {
        let results = AssignmentToResultArray(partial, nQubits);
        return Is3SatSolution(problem, results);
    }
    
    // Estimate number of solutions in the subspace
    let nSubspaceItems = 2.0 ^ IntAsDouble(nUnassigned);
    let estimatedSolutions = EstimateNumSolutionsStatistical(nUnassigned, Length(problem));
    let nSolutions = MaxI(1, Round(estimatedSolutions));
    let iterations = CalculateOptimalIterations(nUnassigned, nSolutions);
    
    // Use a smaller number of attempts for subspace search
    let maxAttempts = 5;
    
    use qubits = Qubit[nQubits];
    
    // Set the qubits according to partial assignment
    for (varIdx, value) in partial! {
        if (value) {
            X(qubits[varIdx]);
        }
    }
    
    // Get array of unassigned qubit indices
    mutable unassignedIndices : Int[] = [];
    for i in 0..nQubits-1 {
        mutable isAssigned = false;
        for (varIdx, _) in partial! {
            if (varIdx == i) {
                set isAssigned = true;
            }
        }
        if (not isAssigned) {
            set unassignedIndices += [i];
        }
    }
    
    mutable foundSolution = false;
    mutable attempts = 0;
    
    repeat {
        // Prepare uniform superposition over unassigned qubits only
        for idx in unassignedIndices {
            H(qubits[idx]);
        }
        
        // Apply Grover iterations
        for _ in 1..iterations {
            ReflectAbout3SatSolutionWithPartial(problem, qubits, partial);
            ReflectAboutUniformPartial(qubits, unassignedIndices);
        }
        
        // Measure and check if we found a solution
        let result = MResetEachZ(qubits);
        if (Is3SatSolution(problem, result)) {
            set foundSolution = true;
        }
        
        // Ensure all qubits are properly reset to |0⟩ state
        // First, reset all qubits to |0⟩
        ResetAll(qubits);
        
        // Then reapply the partial assignment for the next iteration (if needed)
        if (not foundSolution and attempts + 1 < maxAttempts) {
            for (varIdx, value) in partial! {
                if (value) {
                    X(qubits[varIdx]);
                }
            }
        }
        
        set attempts += 1;
    } until (foundSolution or attempts >= maxAttempts) fixup {};
    
    return foundSolution;
}

/// # Summary
/// Converts a partial assignment to a Result[] array (Q#'s qubit measurement type).
function AssignmentToResultArray(
    assigned : PartialAssignment,
    nQubits : Int
) : Result[] {
    mutable arr = Repeated(Zero, nQubits);
    for (idx, val) in assigned! {
        set arr w/= idx <- (if val { One } else { Zero });
    }
    return arr;
}

/// # Summary
/// Estimates the number of solutions to a 3SAT problem using the statistical independence assumption.
function EstimateNumSolutionsStatistical(nQubits : Int, nClauses : Int) : Double {
    let nAssignments = 2.0 ^ IntAsDouble(nQubits);
    let probSatisfy = (7.0 / 8.0) ^ IntAsDouble(nClauses);
    let rawEstimate = nAssignments * probSatisfy;
    let adjustmentFactor = 0.5; // Use 1.0 for no adjustment.
    return rawEstimate * adjustmentFactor;
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
        let iterations = Round((PI() / 4.0) * Sqrt(nItems / IntAsDouble(nSolutions)));
        return iterations;
    } else {
        let angle = ArcSin(1. / Sqrt(nItems));
        let iterations = Round((0.25 * PI() / angle - 0.5) * 1.0);
        return iterations;
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
/// Reflects about the uniform superposition state for only the specified qubits.
operation ReflectAboutUniformPartial(inputQubits : Qubit[], unassignedIndices : Int[]) : Unit {
    within {
        // Transform the uniform superposition to all-zero for unassigned qubits
        for idx in unassignedIndices {
            H(inputQubits[idx]);
        }
        // Transform the all-zero state to all-ones for unassigned qubits
        for idx in unassignedIndices {
            X(inputQubits[idx]);
        }
    } apply {
        // Reflect about the all-ones state for unassigned qubits only
        if (Length(unassignedIndices) > 0) {
            let unassignedQubits = Mapped(i -> inputQubits[i], unassignedIndices);
            if (Length(unassignedQubits) == 1) {
                Z(unassignedQubits[0]);
            } else {
                Controlled Z(Most(unassignedQubits), Tail(unassignedQubits));
            }
        }
    }
}

/// # Summary
/// Modified 3SAT oracle that respects partial assignments.
operation ReflectAbout3SatSolutionWithPartial(
    problem : (Int, Bool)[][],
    inputQubits : Qubit[],
    partial : PartialAssignment
) : Unit {
    // Precompute partial assignment information to avoid mutable assignments in within block
    let partialInfo = PrecomputePartialAssignmentInfo(problem, partial);
    
    use outputQubit = Qubit();
    use clauseQubits = Qubit[Length(problem)];
    
    within {
        X(outputQubit);
        H(outputQubit);
        
        // For each clause, check if it's satisfied
        for clauseIdx in 0 .. Length(problem) - 1 {
            let clause = problem[clauseIdx];
            let clauseInfo = partialInfo[clauseIdx];
            
            use literalQubits = Qubit[3];
            within {
                for litIdx in 0..2 {
                    let (varIdx, isNegated) = clause[litIdx];
                    let (isPartiallyAssigned, partialValue) = clauseInfo[litIdx];
                    
                    if (isPartiallyAssigned) {
                        // Use the partial assignment value
                        let litValue = if isNegated { not partialValue } else { partialValue };
                        if (litValue) {
                            X(literalQubits[litIdx]);
                        }
                    } else {
                        // Use the quantum state
                        if isNegated {
                            X(inputQubits[varIdx]);
                            CNOT(inputQubits[varIdx], literalQubits[litIdx]);
                            X(inputQubits[varIdx]);
                        } else {
                            CNOT(inputQubits[varIdx], literalQubits[litIdx]);
                        }
                    }
                }
            } apply {
                // Compute clause satisfaction: clause is satisfied if at least one literal is true
                // So we want to flip if all literals are false
                X(literalQubits[0]);
                X(literalQubits[1]);
                X(literalQubits[2]);
                Controlled X(literalQubits, clauseQubits[clauseIdx]);
                X(clauseQubits[clauseIdx]);
                X(literalQubits[0]);
                X(literalQubits[1]);
                X(literalQubits[2]);
            }
        }
    } apply {
        Controlled X(clauseQubits, outputQubit);
    }
}

/// # Summary
/// Precomputes partial assignment information for each literal in each clause.
/// Returns array of clause info, where each clause info contains (isAssigned, value) for each literal.
function PrecomputePartialAssignmentInfo(
    problem : (Int, Bool)[][],
    partial : PartialAssignment
) : (Bool, Bool)[][] {
    mutable result : (Bool, Bool)[][] = [];
    for clause in problem {
        mutable clauseInfo : (Bool, Bool)[] = [];
        for (varIdx, _) in clause {
            mutable isPartiallyAssigned = false;
            mutable partialValue = false;
            for (pVarIdx, pValue) in partial! {
                if (pVarIdx == varIdx) {
                    set isPartiallyAssigned = true;
                    set partialValue = pValue;
                }
            }
            set clauseInfo += [(isPartiallyAssigned, partialValue)];
        }
        set result += [clauseInfo];
    }
    return result;
}

// ...existing helper functions...
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

//
// End of montanaro.qs
//