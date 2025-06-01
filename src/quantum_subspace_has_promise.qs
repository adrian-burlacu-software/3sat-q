//  Enhanced Quantum Subspace Promise with Improved Accuracy
//  Key improvements: confidence intervals, adaptive thresholds, and better sampling strategies

open Microsoft.Quantum.Canon;
open Microsoft.Quantum.Diagnostics;
open Microsoft.Quantum.Intrinsic;
open Microsoft.Quantum.Measurement;
open Microsoft.Quantum.Math;

import Std.Convert.*;
import Std.Math.*;
import Std.Arrays.*;
import Std.Measurement.*;
import Std.Diagnostics.*;

/// # Summary
/// Enhanced quantum-enhanced promise check with confidence intervals
operation QuantumSubspaceHasPromise(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assignment : PartialAssignment
) : Bool {
    let nUnassigned = nQubits - Length(assignment::assignments);

    // For very small subspaces, use classical verification (more efficient)
    if (nUnassigned <= 3) {
        return ClassicalSubspaceHasPromise(problem, nQubits, assignment);
    }

    // For medium subspaces (4-12 variables), use enhanced quantum amplitude estimation
    if (nUnassigned <= 12) {
        return EnhancedQuantumAmplitudeEstimationHasPromise(problem, nQubits, assignment, nUnassigned);
    }

    // For large subspaces, use statistical approach with confidence intervals
    return ConfidenceIntervalStatisticalHasPromise(problem, nQubits, assignment, nUnassigned);
}

/// # Summary
/// Enhanced quantum amplitude estimation with confidence intervals and adaptive sampling
operation EnhancedQuantumAmplitudeEstimationHasPromise(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assignment : PartialAssignment,
    nUnassigned : Int
) : Bool {
    // Start with coarse estimation for early filtering
    let coarseResult = CoarseAmplitudeEstimation(problem, nQubits, assignment, nUnassigned);
    let threshold = AdaptivePromiseThreshold(nUnassigned);
    
    // Early exit if clearly unpromising (saves computation)
    if (coarseResult < 0.3 * threshold) {
        return false;
    }
    
    // Early exit if clearly promising (but verify with more samples)
    if (coarseResult > 3.0 * threshold) {
        let verificationResult = MediumAmplitudeEstimation(problem, nQubits, assignment, nUnassigned);
        return verificationResult > threshold;
    }
    
    // For borderline cases, use confidence interval approach
    return ConfidenceBasedEstimation(problem, nQubits, assignment, nUnassigned);
}

/// # Summary
/// Coarse amplitude estimation with improved sampling
operation CoarseAmplitudeEstimation(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assignment : PartialAssignment,
    nUnassigned : Int
) : Double {
    let nSamples = 128;  // Increased from 64 for better accuracy
    mutable successCount = 0;
    
    for _ in 0..nSamples-1 {
        if (ImprovedQuantumSample(problem, nQubits, assignment, nUnassigned)) {
            set successCount += 1;
        }
    }
    
    return IntAsDouble(successCount) / IntAsDouble(nSamples);
}

/// # Summary
/// Medium precision amplitude estimation
operation MediumAmplitudeEstimation(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assignment : PartialAssignment,
    nUnassigned : Int
) : Double {
    let nSamples = 384;  // Increased from 256 for better accuracy
    mutable successCount = 0;
    
    for _ in 0..nSamples-1 {
        if (ImprovedQuantumSample(problem, nQubits, assignment, nUnassigned)) {
            set successCount += 1;
        }
    }
    
    return IntAsDouble(successCount) / IntAsDouble(nSamples);
}

/// # Summary
/// Confidence interval based estimation for borderline cases
operation ConfidenceBasedEstimation(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assignment : PartialAssignment,
    nUnassigned : Int
) : Bool {
    let maxSamples = 512;
    let minSamples = 64;
    let confidenceThreshold = 0.1; // 10% confidence interval width
    
    mutable totalSamples = 0;
    mutable successCount = 0;
    mutable confidenceWidth = 1.0;
    
    repeat {
        // Sample in batches for efficiency
        let batchSize = 32;
        mutable batchSuccess = 0;
        
        for _ in 0..batchSize-1 {
            if (ImprovedQuantumSample(problem, nQubits, assignment, nUnassigned)) {
                set batchSuccess += 1;
            }
        }
        
        set successCount += batchSuccess;
        set totalSamples += batchSize;
        
        // Calculate confidence interval
        if (totalSamples >= minSamples) {
            let currentDensity = IntAsDouble(successCount) / IntAsDouble(totalSamples);
            let variance = currentDensity * (1.0 - currentDensity) / IntAsDouble(totalSamples);
            set confidenceWidth = 1.96 * Sqrt(variance); // 95% confidence interval
            
            let threshold = AdaptivePromiseThreshold(nUnassigned);
            
            // Early stopping if confidence interval doesn't overlap threshold
            if (currentDensity - confidenceWidth > threshold) {
                return true;  // Clearly promising
            }
            if (currentDensity + confidenceWidth < threshold) {
                return false; // Clearly unpromising
            }
        }
        
    } until (totalSamples >= maxSamples or confidenceWidth < confidenceThreshold);
    
    let finalDensity = IntAsDouble(successCount) / IntAsDouble(totalSamples);
    return finalDensity > AdaptivePromiseThreshold(nUnassigned);
}

/// # Summary
/// Statistical approach with multiple rounds and confidence intervals
operation ConfidenceIntervalStatisticalHasPromise(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assignment : PartialAssignment,
    nUnassigned : Int
) : Bool {
    let nRounds = 4; // Increased from 3 for better reliability
    let samplesPerRound = ImprovedAdaptiveSampleCount(nUnassigned) / nRounds;
    
    mutable totalSolutions = 0;
    mutable totalSamples = 0;
    mutable densityHistory = [];
    
    for round in 0..nRounds-1 {
        mutable roundSolutions = 0;
        
        for _ in 0..samplesPerRound-1 {
            if (ImprovedQuantumSample(problem, nQubits, assignment, nUnassigned)) {
                set roundSolutions += 1;
            }
        }
        
        set totalSolutions += roundSolutions;
        set totalSamples += samplesPerRound;
        
        let currentDensity = IntAsDouble(totalSolutions) / IntAsDouble(totalSamples);
        set densityHistory += [currentDensity];
        
        // Calculate confidence interval
        if (totalSamples >= 64) {
            let variance = currentDensity * (1.0 - currentDensity) / IntAsDouble(totalSamples);
            let confidenceWidth = 1.96 * Sqrt(variance);
            let threshold = ImprovedAdaptivePromiseThreshold(nUnassigned);
            
            // More conservative early stopping
            if (totalSamples >= 128) {
                if (currentDensity - confidenceWidth > 1.5 * threshold) {
                    return true;
                }
                if (currentDensity + confidenceWidth < 0.7 * threshold) {
                    return false;
                }
            }
        }
        
        // Trend analysis for additional confidence
        if (round >= 2) {
            let stabilityCheck = CheckDensityStability(densityHistory);
            if (stabilityCheck and totalSamples >= 192) {
                // Early exit using return instead of break
                let finalDensity = IntAsDouble(totalSolutions) / IntAsDouble(totalSamples);
                return finalDensity > ImprovedAdaptivePromiseThreshold(nUnassigned);
            }
        }
    }
    
    let finalDensity = IntAsDouble(totalSolutions) / IntAsDouble(totalSamples);
    return finalDensity > ImprovedAdaptivePromiseThreshold(nUnassigned);
}

/// # Summary
/// Improved quantum sampling with bias correction
operation ImprovedQuantumSample(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assignment : PartialAssignment,
    nUnassigned : Int
) : Bool {
    let unassignedVars = GetUnassignedVariables(assignment, nQubits);
    
    use qubits = Qubit[nUnassigned];

    // Improved state preparation with problem-aware bias
    PrepareImprovedSuperposition(qubits, problem, assignment, unassignedVars, nUnassigned);

    // Measure and check
    let results = ForEach(M, qubits);

    // Build complete assignment
    mutable completeAssignment = assignment::assignments;
    for i in 0..nUnassigned-1 {
        let varIdx = unassignedVars[i];
        let value = results[i] == One;
        set completeAssignment += [(varIdx, value)];
    }

    let finalResults = AssignmentToResultArray(PartialAssignment(completeAssignment), nQubits);
    let isSolution = Is3SatSolution(problem, finalResults);

    // Reset qubits
    ResetAll(qubits);

    return isSolution;
}

/// # Summary
/// Improved state preparation with problem-aware biasing
operation PrepareImprovedSuperposition(
    qubits : Qubit[],
    problem : (Int, Bool)[][],
    assignment : PartialAssignment,
    unassignedVars : Int[],
    nUnassigned : Int
) : Unit {
    // Start with uniform superposition
    for q in qubits { H(q); }
    
    // Apply problem-aware bias for larger subspaces
    if (nUnassigned > 8) {
        // Calculate clause satisfaction bias for each variable
        let biases = CalculateVariableBiases(problem, assignment, unassignedVars);
        
        // Apply small rotations based on bias (minimal performance impact)
        for i in 0..nUnassigned-1 {
            let bias = biases[i];
            if (AbsD(bias) > 0.1) {
                // Small rotation to bias towards satisfying assignments
                let angle = MinD(0.2, AbsD(bias) * 0.3); // Limit angle to avoid over-biasing
                if (bias > 0.0) {
                    Ry(angle, qubits[i]);
                } else {
                    Ry(-angle, qubits[i]);
                }
            }
        }
    }
}

/// # Summary
/// Calculate variable biases based on clause analysis
function CalculateVariableBiases(
    problem : (Int, Bool)[][],
    assignment : PartialAssignment,
    unassignedVars : Int[]
) : Double[] {
    mutable biases = Repeated(0.0, Length(unassignedVars));
    
    for clauseIdx in 0..Length(problem)-1 {
        let clause = problem[clauseIdx];
        let (satisfied, criticalVars) = AnalyzeClause(clause, assignment, unassignedVars);
        
        if (not satisfied and Length(criticalVars) > 0) {
            // Bias towards satisfying this unsatisfied clause
            for (varIdx, isNegated) in criticalVars {
                let pos = FindPosition(unassignedVars, varIdx);
                if (pos >= 0) {
                    // Bias towards the value that would satisfy this literal
                    let biasDelta = 1.0 / IntAsDouble(Length(criticalVars));
                    if (isNegated) {
                        set biases w/= pos <- biases[pos] - biasDelta;
                    } else {
                        set biases w/= pos <- biases[pos] + biasDelta;
                    }
                }
            }
        }
    }
    
    return biases;
}

/// # Summary
/// Improved adaptive sample count with better scaling
function ImprovedAdaptiveSampleCount(nUnassigned : Int) : Int {
    if (nUnassigned <= 8) {
        return 640; // Increased for better accuracy
    } elif (nUnassigned <= 16) {
        return 1280; // Increased for better accuracy
    } elif (nUnassigned <= 24) {
        return 768; // Moderate increase
    } elif (nUnassigned <= 32) {
        return 256; // Doubled for large spaces
    } else {
        return 64; // Doubled for very large spaces
    }
}

/// # Summary
/// Improved adaptive threshold with smoother transitions
function ImprovedAdaptivePromiseThreshold(nUnassigned : Int) : Double {
    // Smoother exponential decay for better accuracy
    let base = 0.15; // Increased base threshold
    let decay = 0.85; // Slightly slower decay
    return base * (decay ^ IntAsDouble(nUnassigned) / 4.0);
}

/// # Summary
/// Check if density estimates are stabilizing
function CheckDensityStability(densityHistory : Double[]) : Bool {
    if (Length(densityHistory) < 3) {
        return false;
    }
    
    let n = Length(densityHistory);
    let recent = densityHistory[n-3..n-1];
    
    // Check if recent estimates are within 20% of each other
    let maxRecent = Fold(MaxD, recent[0], recent);
    let minRecent = Fold(MinD, recent[0], recent);
    
    return (maxRecent - minRecent) / maxRecent < 0.2;
}

// Helper functions

function GetUnassignedVariables(assignment : PartialAssignment, nQubits : Int) : Int[] {
    mutable unassignedVars = [];
    mutable isAssigned = [false, size = nQubits];

    for (varIdx, _) in assignment::assignments {
        if (varIdx >= 0 and varIdx < nQubits) {
            set isAssigned w/= varIdx <- true;
        }
    }

    for varIdx in 0..nQubits-1 {
        if (not isAssigned[varIdx]) {
            set unassignedVars += [varIdx];
        }
    }
    
    return unassignedVars;
}

function AnalyzeClause(
    clause : (Int, Bool)[],
    assignment : PartialAssignment,
    unassignedVars : Int[]
) : (Bool, (Int, Bool)[]) {
    mutable satisfied = false;
    mutable criticalVars = [];
    
    for (varIdx, isNegated) in clause {
        let (isAssigned, assignedValue) = GetAssignedValue(assignment, varIdx);
        
        if (isAssigned) {
            let literalSatisfied = (isNegated and not assignedValue) or (not isNegated and assignedValue);
            if (literalSatisfied) {
                set satisfied = true;
            }
        } else {
            // This is a critical unassigned variable
            set criticalVars += [(varIdx, isNegated)];
        }
    }
    
    return (satisfied, criticalVars);
}

function FindPosition(arr : Int[], value : Int) : Int {
    for i in 0..Length(arr)-1 {
        if (arr[i] == value) {
            return i;
        }
    }
    return -1;
}

// Include existing helper functions from original file
function ClassicalSubspaceHasPromise(
    problem : (Int, Bool)[][],
    nQubits : Int,
    assignment : PartialAssignment
) : Bool {
    // ... (keep original implementation)
    let nUnassigned = nQubits - Length(assignment::assignments);

    if (nUnassigned == 0) {
        let results = AssignmentToResultArray(assignment, nQubits);
        return Is3SatSolution(problem, results);
    }

    // Get unassigned variable indices
    mutable unassignedVars = [];
    mutable isAssigned = [false, size = nQubits];

    for (varIdx, _) in assignment::assignments {
        if (varIdx >= 0 and varIdx < nQubits) {
            set isAssigned w/= varIdx <- true;
        }
    }

    for varIdx in 0..nQubits-1 {
        if (not isAssigned[varIdx]) {
            set unassignedVars += [varIdx];
        }
    }

    let nPossible = 1 <<< nUnassigned;

    // Check all possible assignments to unassigned variables
    for i in 0..nPossible-1 {
        mutable testAssignment = assignment::assignments;

        for k in 0..nUnassigned-1 {
            let varIdx = unassignedVars[k];
            let value = (i &&& (1 <<< k)) != 0;
            set testAssignment += [(varIdx, value)];
        }

        let results = AssignmentToResultArray(PartialAssignment(testAssignment), nQubits);
        if (Is3SatSolution(problem, results)) {
            return true; // Found at least one solution
        }
    }

    return false; // No solutions found
}

function AdaptivePromiseThreshold(nUnassigned : Int) : Double {
    if (nUnassigned <= 4) {
        return 0.1; // 10% solution density for small subspaces
    } elif (nUnassigned <= 8) {
        return 0.05; // 5% solution density for medium subspaces
    } elif (nUnassigned <= 16) {
        return 0.02; // 2% solution density for larger subspaces
    } elif (nUnassigned <= 24) {
        return 0.01; // 1% solution density for very large subspaces
    } else {
        return 0.005; // 0.5% solution density for extremely large subspaces
    }
}

function AssignmentToResultArray(assigned : PartialAssignment, nQubits : Int) : Result[] {
    mutable arr = Repeated(Zero, nQubits);
    for (idx, val) in assigned::assignments {
        if (idx >= 0 and idx < nQubits) {
            set arr w/= idx <- (if val { One } else { Zero });
        }
    }
    return arr;
}

function Is3SatSolution(problem : (Int, Bool)[][], results : Result[]) : Bool {
    for clause in problem {
        mutable clauseSat = false;
        for (varIdx, isNegated) in clause {
            if (varIdx >= 0 and varIdx < Length(results)) {
                let bit = results[varIdx] == One;
                let litSat = (isNegated and not bit) or (not isNegated and bit);
                if (litSat) { 
                    set clauseSat = true; 
                }
            }
        }
        if (not clauseSat) { 
            return false; 
        }
    }
    return true;
}

function GetAssignedValue(assignment : PartialAssignment, varIdx : Int) : (Bool, Bool) {
    for (assignedVar, value) in assignment::assignments {
        if (assignedVar == varIdx) {
            return (true, value);
        }
    }
    return (false, false);
}

newtype PartialAssignment = (assignments : (Int, Bool)[]);