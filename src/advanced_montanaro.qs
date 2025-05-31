//  Advanced Optimized Montanaro's Quantum Backtracking Algorithm
//  This version includes comprehensive optimizations for large-scale 3SAT problems:
//  1. Constraint propagation (unit propagation, pure literal elimination)
//  2. Smart variable ordering heuristics (most constrained variable first)
//  3. Hybrid classical-quantum approach with adaptive switching
//  4. Early termination and conflict analysis
//  5. Solution density estimation and branch pruning
//  6. Preprocessing optimizations

open Microsoft.Quantum.Canon;
open Microsoft.Quantum.Diagnostics;
open Microsoft.Quantum.Intrinsic;
open Microsoft.Quantum.Measurement;

import Std.Convert.*;
import Std.Math.*;
import Std.Arrays.*;
import Std.Measurement.*;
import Std.Diagnostics.*;
import test_problem_generators.*;
import quantum_subspace_has_promise.*;



@EntryPoint()
operation AdvancedMontanaroMain() : Unit {
    let DEBUG_OUTPUT = true; // Set to false for production runs
    if (DEBUG_OUTPUT) { Message("=== Advanced Optimized Montanaro Algorithm ==="); }
    
    // Test progression: 10 -> 15 -> 20 -> 25 -> 30 qubits
    let testCases = [
        (10, "Original 10-qubit problem"),
        (15, "Structured 15-qubit problem"),
        (20, "Moderate 20-qubit problem"),
        (25, "Challenging 25-qubit problem"),
        (30, "Large-scale 30-qubit problem"),
        (35, "Very large-scale 35-qubit problem"),
        (40, "Extremely large-scale 40-qubit problem"),
        (45, "Massive 45-qubit problem"),
        (50, "Gargantuan 50-qubit problem"),
        (55, "Colossal 55-qubit problem"),
        (60, "Astronomical 60-qubit problem")
    ];
    
    for (nQubits, description) in testCases {
        if (DEBUG_OUTPUT) { Message($"=== Testing {description} ==="); }
        let problem = GenerateStructuredProblem(nQubits); 
        if (DEBUG_OUTPUT) { Message($"Problem: {nQubits} variables, {Length(problem)} clauses"); }
        
        let result = AdvancedMontanaroSolver(problem, nQubits);
        
        if (result::found) {
            if (DEBUG_OUTPUT) { Message($"✓ Solution found: {result::solution}"); }
            let assignment = IntToResultArray(result::solution, nQubits);
            if (Is3SatSolution(problem, assignment)) {
                if (DEBUG_OUTPUT) { Message("✓ Solution verified as correct!"); }
            } else {
                if (DEBUG_OUTPUT) { Message("✗ WARNING: Solution verification failed!"); }
            }
        } else {
            if (DEBUG_OUTPUT) { Message("✗ No solution found"); }
        }
        
        if (DEBUG_OUTPUT) { Message($"Search statistics: {result::stats}"); }
    }
}

// Define result types
newtype AdvancedResultType = (found : Bool, solution : Int, stats : String);
newtype BasicResultType = (found : Bool, solution : Int);
newtype SimplifiedProblem = (problem : (Int, Bool)[][], assignment : PartialAssignment);

/// # Summary
/// Main solver with advanced optimizations
operation AdvancedMontanaroSolver(
    problem : (Int, Bool)[][],
    nQubits : Int
) : AdvancedResultType {
   // if (DEBUG_OUTPUT) { Message("Starting advanced preprocessing..."); }
    
    // Preprocessing phase
    let preprocessed = AdvancedPreprocessing(problem, nQubits);
    
    if (Length(preprocessed::assignment::assignments) == nQubits) {
        //if (DEBUG_OUTPUT) { Message("Problem solved during preprocessing!"); }
        let solutionInt = ResultArrayAsInt(AssignmentToResultArray(preprocessed::assignment, nQubits));
        return AdvancedResultType(true, solutionInt, "Preprocessing");
    }
    
    //if (DEBUG_OUTPUT) { Message($"Preprocessing complete. {Length(preprocessed::assignment::assignments)} variables assigned."); }
    
    // Main search phase
    let result = AdvancedMontanaroRecursive(
        preprocessed::problem, 
        nQubits, 
        preprocessed::assignment, 
        0,
        0, // nodeCount
        0, // quantumCalls  
        0, // classicalCalls
        0  // maxDepth
    );
    
    let stats = $"Advanced search completed with optimizations";
    return AdvancedResultType(result::found, result::solution, stats);
}

/// # Summary
/// Advanced recursive search with comprehensive optimizations
operation AdvancedMontanaroRecursive(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assigned : PartialAssignment,
    depth : Int,
    nodeCount : Int,
    quantumCalls : Int,
    classicalCalls : Int,
    maxDepth : Int
) : BasicResultType {
    let currentNodeCount = nodeCount + 1;
    let currentMaxDepth = MaxI(maxDepth, depth);

    // Base case: all variables assigned
    if (Length(assigned::assignments) == nQubits) {
        let results = AssignmentToResultArray(assigned, nQubits);
        if (Is3SatSolution(problem, results)) {
            let solutionInt = ResultArrayAsInt(results);
            return BasicResultType(true, solutionInt);
        } else {
            return BasicResultType(false, 0);
        }
    }
    
    // Fast conflict detection
    if (HasConflict(problem, assigned)) {
        return BasicResultType(false, 0);
    }
    
    let nextVar = ChooseVariableAdvanced(problem, assigned, nQubits);
    let nUnassigned = nQubits - Length(assigned::assignments);
    if (nUnassigned <= 4) {
        return AdvancedClassicalBacktrack(problem, nQubits, assigned, nextVar);
    }
    
    let valueOrder = DetermineValueOrder(problem, assigned, nextVar);
    for value in valueOrder {
        let newAssignment = PartialAssignment(assigned::assignments + [(nextVar, value)]);
        if (IsAssignmentPromisingRelaxed(problem, nQubits, newAssignment)) {
            mutable useQuantum = true;
            if (nUnassigned > 15) {
                useQuantum = true;
            } else {
                useQuantum = QuantumSubspaceHasPromise(problem, nQubits, newAssignment);
            }
            if (useQuantum) {
                let res = AdvancedMontanaroRecursive(
                    problem, 
                    nQubits, 
                    newAssignment, 
                    depth + 1,
                    currentNodeCount,
                    quantumCalls + 1,
                    classicalCalls,
                    currentMaxDepth
                );
                if (res::found) {
                    return res;
                }
            }
        }
    }
    return BasicResultType(false, 0);
}

/// # Summary
/// Advanced preprocessing with multiple techniques
operation AdvancedPreprocessing(
    problem : (Int, Bool)[][],
    nQubits : Int
) : SimplifiedProblem {
    mutable currentProblem = problem;
    mutable assignment = PartialAssignment([]);
    mutable totalPropagations = 0;
    mutable changed = true;
    mutable iterations = 0;
    repeat {
        set changed = false;
        set iterations += 1;
        let unitResultAssignment = UnitPropagation(currentProblem, assignment, nQubits);
        if (Length(unitResultAssignment::assignments) > Length(assignment::assignments)) {
            set assignment = unitResultAssignment;
            set changed = true;
            set totalPropagations += 1;
        }
        let pureResultAssignment = PureLiteralElimination(currentProblem, assignment, nQubits);
        if (Length(pureResultAssignment::assignments) > Length(assignment::assignments)) {
            set assignment = pureResultAssignment;
            set changed = true;
            set totalPropagations += 1;
        }
        if (nQubits >= 25) {
            set currentProblem = EliminateSubsumedClauses(currentProblem);
        }
        set currentProblem = SimplifyProblem(currentProblem, assignment);
    } until (not changed or iterations >= 10);
    // if (DEBUG_OUTPUT) { Message($"Preprocessing: {totalPropagations} propagations in {iterations} iterations"); }
    return SimplifiedProblem(currentProblem, assignment);
}

/// # Summary
/// Unit propagation: if a clause has only one unassigned literal, assign it to true
operation UnitPropagation(
    problem : (Int, Bool)[][],
    initialProblemAssignment : PartialAssignment,
    nQubits : Int 
) : PartialAssignment {
    mutable workingAssignments = initialProblemAssignment::assignments;
    mutable continueLooping = true;
    repeat {
        set continueLooping = false;
        mutable assignmentsFoundThisIteration = []; 
        mutable currentlyAssignedLookup = [false, size = nQubits];
        for (varIdx, _) in workingAssignments {
            if varIdx >= 0 and varIdx < nQubits {
                set currentlyAssignedLookup w/= varIdx <- true;
            }
        }
        for clause in problem {
            let contextForFind = PartialAssignment(workingAssignments);
            let unitLiteralInfo = FindUnitLiteral(clause, contextForFind); 
            if (unitLiteralInfo[0] >= 0) {
                let varIdx = unitLiteralInfo[0];
                let valueToAssign = unitLiteralInfo[1] == 1; 
                if varIdx >= 0 and varIdx < nQubits and not currentlyAssignedLookup[varIdx] {
                    mutable alreadyFoundForThisIteration = false;
                    for (foundVar, _) in assignmentsFoundThisIteration {
                        if foundVar == varIdx {
                            alreadyFoundForThisIteration = true;
                        }
                    }
                    if not alreadyFoundForThisIteration {
                        set assignmentsFoundThisIteration += [(varIdx, valueToAssign)];
                    }
                }
            }
        }
        if (Length(assignmentsFoundThisIteration) > 0) {
            set continueLooping = true;
            for (newVarIdx, newValue) in assignmentsFoundThisIteration {
                mutable trulyNewToAdd = true;
                if newVarIdx >=0 and newVarIdx < nQubits and currentlyAssignedLookup[newVarIdx]{
                    trulyNewToAdd = false;
                }
                if trulyNewToAdd {
                     set workingAssignments += [(newVarIdx, newValue)];
                }
            }
        }
    } until (not continueLooping);
    
    return PartialAssignment(workingAssignments);
}

/// # Summary
/// Pure literal elimination: if a variable appears only positively or only negatively, assign it
function PureLiteralElimination(
    problem : (Int, Bool)[][],
    assignment : PartialAssignment,
    nQubits : Int
) : PartialAssignment {
    mutable currentAssignments = assignment::assignments;
    mutable isEffectivelyAssigned = [false, size = nQubits];
    for (varIdx, _) in currentAssignments {
        if varIdx >= 0 and varIdx < nQubits {
            set isEffectivelyAssigned w/= varIdx <- true;
        }
    }
    mutable newlyFoundAssignmentsInThisPass = [];
    for varIdxToTestPurity in 0..nQubits-1 {
        mutable alreadyAssignedInContext = false;
        if varIdxToTestPurity >=0 and varIdxToTestPurity < nQubits and isEffectivelyAssigned[varIdxToTestPurity] {
            alreadyAssignedInContext = true;
        } else {
             for (newlyFoundVar, _) in newlyFoundAssignmentsInThisPass {
                if newlyFoundVar == varIdxToTestPurity {
                    alreadyAssignedInContext = true;
                }
            }
        }
        if (not alreadyAssignedInContext) {
            let purity = CheckVariablePurity(problem, varIdxToTestPurity); 
            if (purity[0] != 0) {
                let valueToAssign = purity[0] > 0; 
                set newlyFoundAssignmentsInThisPass += [(varIdxToTestPurity, valueToAssign)];
                if varIdxToTestPurity >= 0 and varIdxToTestPurity < nQubits {
                    set isEffectivelyAssigned w/= varIdxToTestPurity <- true;
                }
            }
        }
    }
    return PartialAssignment(currentAssignments + newlyFoundAssignmentsInThisPass);
}

/// # Summary
/// Advanced classical backtracking for small subspaces
function AdvancedClassicalBacktrack(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assigned : PartialAssignment,
    startVar : Int
) : BasicResultType {
    mutable initialAssignmentsArr = assigned::assignments;
    mutable isInitiallyAssignedLookup = [false, size = nQubits];
    for (varIdx, _) in initialAssignmentsArr {
        if (varIdx >= 0 and varIdx < nQubits) {
            set isInitiallyAssignedLookup w/= varIdx <- true;
        }
    }
    mutable unassignedVarIndices = [];
    for varIdx in 0..nQubits-1 {
        if not isInitiallyAssignedLookup[varIdx] {
            set unassignedVarIndices += [varIdx];
        }
    }
    let nActualUnassigned = Length(unassignedVarIndices);
    if (nActualUnassigned == 0) {
        let currentResults = AssignmentToResultArray(assigned, nQubits);
        if (Is3SatSolution(problem, currentResults)) {
            return BasicResultType(true, ResultArrayAsInt(currentResults));
        } else {
            return BasicResultType(false, 0);
        }
    }
    for i in 0..(1 <<< nActualUnassigned) - 1 {
        mutable currentTestCombinedAssignments = initialAssignmentsArr;
        for k in 0..nActualUnassigned-1 {
            let originalVarIndex = unassignedVarIndices[k];
            let bitValueForThisVar = (i &&& (1 <<< k)) != 0;
            set currentTestCombinedAssignments += [(originalVarIndex, bitValueForThisVar)];
        }
        let tempPartialAssignment = PartialAssignment(currentTestCombinedAssignments);
        let resultsArray = AssignmentToResultArray(tempPartialAssignment, nQubits); 
        if (Is3SatSolution(problem, resultsArray)) {
            return BasicResultType(true, ResultArrayAsInt(resultsArray));
        }
    }
    return BasicResultType(false, 0);
}

// Utility Functions (Problem Generators moved to test_problem_generators.qs)

function ChooseVariableAdvanced(
    problem : (Int, Bool)[][],
    assigned : PartialAssignment,
    nQubits : Int
) : Int {
    mutable bestVar = -1;
    mutable bestScore = -1; 
    mutable isVarAssigned = [false, size = nQubits];
    for (varIndex, _) in assigned::assignments {
        if varIndex >= 0 and varIndex < nQubits { 
            set isVarAssigned w/= varIndex <- true;
        }
    }
    for varIdx in 0..nQubits-1 {
        if (not isVarAssigned[varIdx]) {
            mutable currentScore = 0; 
            for clause in problem {
                for (literalVar, _) in clause {
                    if literalVar == varIdx {
                        set currentScore += 1;
                    }
                }
            }
            if currentScore > bestScore { 
                set bestVar = varIdx;
                set bestScore = currentScore;
            }
        }
    }
    if bestVar == -1 {
        for varIdxFallback in 0..nQubits-1 {
            if (not isVarAssigned[varIdxFallback]) {
                return varIdxFallback;
            }
        }
        return 0; 
    }
    return bestVar;
}

function DetermineValueOrder(
    problem : (Int, Bool)[][],
    assigned : PartialAssignment,
    varIdx : Int
) : Bool[] {
    let trueScore = CountSatisfiedClauses(problem, assigned, varIdx, true);
    let falseScore = CountSatisfiedClauses(problem, assigned, varIdx, false);
    return if trueScore >= falseScore { [true, false] } else { [false, true] };
}

function IsAssignmentPromisingRelaxed(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assignment : PartialAssignment
) : Bool {
    if (HasConflict(problem, assignment)) {
        return false;
    }
    let nUnassigned = nQubits - Length(assignment::assignments);
    if (nUnassigned > 20) {
        return true;
    }
    return EstimateSolutionDensity(problem, assignment, nQubits) > 0.0001;
}

// === Optimization: Fast Conflict Check ===
function HasConflict(problem : (Int, Bool)[][], assigned : PartialAssignment) : Bool {
    for clause in problem {
        let simplifiedClause = SimplifyClause(clause, assigned);
        if (Length(simplifiedClause) == 0 and not IsClauseSatisfied(clause, assigned)) {
            return true;
        }
    }
    return false;
}

function EstimateSolutionDensity(
    problem : (Int, Bool)[][],
    assignment : PartialAssignment,
    nQubits : Int
) : Double {
    mutable nUnassigned = 0;
    for varIdx in 0..nQubits-1 {
        if (not IsVariableAssigned(varIdx, assignment)) {
            set nUnassigned += 1;
        }
    }
    if (nUnassigned <= 0) {
        return 1.0;
    }
    let clauseDensity = IntAsDouble(Length(problem)) / IntAsDouble(1 <<< MinI(nUnassigned, 20));
    return 1.0 / (1.0 + clauseDensity * 0.5);
}

function IsVariableAssigned(varIdx : Int, assignment : PartialAssignment) : Bool {
    for (assignedVar, _) in assignment::assignments {
        if (assignedVar == varIdx) {
            return true;
        }
    }
    return false;
}

// Utility functions for type conversion and solution checking
function AssignmentToResultArray(
    assigned : PartialAssignment,
    nQubits : Int
) : Result[] {
    mutable arr = Repeated(Zero, nQubits);
    for (idx, val) in assigned::assignments {
        set arr w/= idx <- (if val { One } else { Zero });
    }
    return arr;
}

function ResultArrayAsInt(results : Result[]) : Int {
    mutable value = 0;
    for idx in 0..Length(results)-1 {
        if results[idx] == One {
            set value += 1 <<< idx;
        }
    }
    return value;
}

function IntToResultArray(value : Int, nQubits : Int) : Result[] {
    mutable result = Repeated(Zero, nQubits);
    for i in 0..nQubits-1 {
        if ((value &&& (1 <<< i)) != 0) {
            set result w/= i <- One;
        }
    }
    return result;
}

function Is3SatSolution(problem : (Int, Bool)[][], results : Result[]) : Bool {
    for clause in problem {
        mutable clauseSat = false;
        for (varIdx, isNegated) in clause {
            let bit = results[varIdx] == One;
            let litSat = (isNegated and not bit) or (not isNegated and bit);
            if litSat { set clauseSat = true; }
        }
        if not clauseSat { return false; }
    }
    return true;
}

function FindUnitLiteral(clause : (Int, Bool)[], assignment : PartialAssignment) : Int[] {
    mutable unassigned = [];
    mutable satisfied = false;
    for (varIdx, isNegated) in clause {
        mutable isAssigned = false;
        mutable assignedValue = false;
        for (assignedVar, value) in assignment::assignments {
            if (assignedVar == varIdx) {
                set isAssigned = true;
                set assignedValue = value;
            }
        }
        if (isAssigned) {
            let literalSatisfied = (not isNegated and assignedValue) or (isNegated and not assignedValue);
            if (literalSatisfied) {
                set satisfied = true;
            }
        } else {
            set unassigned += [(varIdx, isNegated)];
        }
    }
    if (satisfied or Length(unassigned) != 1) {
        return [-1, 0];
    }
    let (varIdx, isNegated) = unassigned[0];
    return [varIdx, if isNegated { 0 } else { 1 }];
}

function CheckVariablePurity(problem : (Int, Bool)[][], varIdx : Int) : Int[] {
    mutable positiveOccurrences = 0;
    mutable negativeOccurrences = 0;
    for clause in problem {
        for (clauseVar, isNegated) in clause {
            if (clauseVar == varIdx) {
                if (isNegated) {
                    set negativeOccurrences += 1;
                } else {
                    set positiveOccurrences += 1;
                }
            }
        }
    }
    if (positiveOccurrences > 0 and negativeOccurrences == 0) {
        return [1];
    } elif (negativeOccurrences > 0 and positiveOccurrences == 0) {
        return [-1];
    } else {
        return [0];
    }
}

function SimplifyProblem(
    problem : (Int, Bool)[][],
    assignment : PartialAssignment
) : (Int, Bool)[][] {
    mutable simplified = [];
    for clause in problem {
        let simplifiedClause = SimplifyClause(clause, assignment);
        if (Length(simplifiedClause) > 0) {
            set simplified += [simplifiedClause];
        }
    }
    return simplified;
}

function SimplifyClause(
    clause : (Int, Bool)[],
    assignment : PartialAssignment
) : (Int, Bool)[] {
    mutable simplified = [];
    mutable satisfied = false;
    for (varIdx, isNegated) in clause {
        mutable isAssigned = false;
        mutable assignedValue = false;
        for (assignedVar, value) in assignment::assignments {
            if (assignedVar == varIdx) {
                set isAssigned = true;
                set assignedValue = value;
            }
        }
        if (isAssigned) {
            let literalSatisfied = (not isNegated and assignedValue) or (isNegated and not assignedValue);
            if (literalSatisfied) {
                set satisfied = true;
            }
        } else {
            set simplified += [(varIdx, isNegated)];
        }
    }
    return if satisfied { [] } else { simplified };
}

function CountSatisfiedClauses(
    problem : (Int, Bool)[][],
    assigned : PartialAssignment,
    varIdx : Int,
    value : Bool
) : Int {
    mutable count = 0;
    let testAssignment = PartialAssignment(assigned::assignments + [(varIdx, value)]);
    for clause in problem {
        if (IsClauseSatisfied(clause, testAssignment)) {
            set count += 1;
        }
    }
    return count;
}

function IsClauseSatisfied(clause : (Int, Bool)[], assignment : PartialAssignment) : Bool {
    for (varIdx, isNegated) in clause {
        for (assignedVar, assignedValue) in assignment::assignments {
            if (assignedVar == varIdx) {
                let literalSatisfied = (not isNegated and assignedValue) or (isNegated and not assignedValue);
                if (literalSatisfied) {
                    return true;
                }
            }
        }
    }
    return false;
}

/// # Summary
/// Enhanced preprocessing functions for larger problems
function EliminateSubsumedClauses(problem : (Int, Bool)[][]) : (Int, Bool)[][] {
    mutable result = [];
    for i in 0..Length(problem)-1 {
        let clauseI = problem[i];
        mutable isSubsumed = false;
        for j in 0..Length(problem)-1 {
            if (i != j) {
                let clauseJ = problem[j];
                if (IsClauseSubsumedBy(clauseI, clauseJ)) {
                    set isSubsumed = true;
                }
            }
        }
        if (not isSubsumed) {
            set result += [clauseI];
        }
    }
    return result;
}

function IsClauseSubsumedBy(clause1 : (Int, Bool)[], clause2 : (Int, Bool)[]) : Bool {
    // clause1 is subsumed by clause2 if all literals in clause2 appear in clause1
    for (var2, neg2) in clause2 {
        mutable found = false;
        for (var1, neg1) in clause1 {
            if (var1 == var2 and neg1 == neg2) {
                set found = true;
            }
        }
        if (not found) {
            return false;
        }
    }
    return true;
}