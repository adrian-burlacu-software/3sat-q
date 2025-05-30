open Microsoft.Quantum.Intrinsic;
open Std.Random; // For DrawRandomInt, DrawRandomDouble

// Helper operation to get a random boolean (moved here as it's used by GenerateProblemWithKnownSolutionOp)
operation RandomBoolOp() : Bool {
    return DrawRandomDouble(0.0, 1.0) < 0.5;
}

// New helper operation to generate a problem with a known solution (moved here)
operation GenerateProblemWithKnownSolutionOp(nQubits : Int, numClauses : Int) : ((Int, Bool)[][], Result[]) {
    mutable knownSolution : Result[] = Repeated(Zero, nQubits);
    for i in 0..nQubits-1 {
        if (RandomBoolOp()) {
            set knownSolution w/= i <- One;
        }
    }

    mutable problem : (Int, Bool)[][] = [];

    for _ in 0..numClauses-1 {
        mutable clauseLiterals : (Int, Bool)[] = [];
        mutable varsInClause = [-1, -1, -1];
        
        // 1. Pick 3 distinct variables
        for iLit in 0..2 {
            mutable varIdx = -1;
            mutable isUnique = false;
            repeat {
                set varIdx = DrawRandomInt(0, nQubits - 1);
                set isUnique = true;
                for kChosen in 0..iLit-1 {
                    if (varsInClause[kChosen] == varIdx) {
                        set isUnique = false;
                    }
                }
            } until isUnique;
            set varsInClause w/= iLit <- varIdx;
        }

        // 2. Tentatively assign negations
        for iLit in 0..2 {
            let varIdx = varsInClause[iLit];
            let isNegated = RandomBoolOp();
            set clauseLiterals += [(varIdx, isNegated)];
        }

        // 3. Check if the clause is satisfied by knownSolution
        mutable clauseSatisfiedByKnownSolution = false;
        for (varIdx, isNegated) in clauseLiterals {
            let varValueInSolution = knownSolution[varIdx] == One;
            let literalValue = if isNegated { not varValueInSolution } else { varValueInSolution };
            if (literalValue) {
                set clauseSatisfiedByKnownSolution = true;
                // No break here, need to iterate all literals if using 'break' is an issue or for clarity
            }
        }

        // 4. If not satisfied, flip one literal's negation to make it satisfied
        if (not clauseSatisfiedByKnownSolution) {
            let flipIndex = DrawRandomInt(0, 2); // 0, 1, or 2
            let (varToFlip, _originalNegation) = clauseLiterals[flipIndex];
            let varValueInSolution = knownSolution[varToFlip] == One;
            
            let requiredNegation = not varValueInSolution;
            
            mutable newClauseLiterals : (Int, Bool)[] = [];
            for iLit in 0..2 {
                if (iLit == flipIndex) {
                    set newClauseLiterals += [(varToFlip, requiredNegation)];
                } else {
                    set newClauseLiterals += [clauseLiterals[iLit]];
                }
            }
            set clauseLiterals = newClauseLiterals;
        }
        set problem += [clauseLiterals];
    }
    return (problem, knownSolution);
}

// Problem generators (moved here)
operation GenerateStructuredProblem(nQubits : Int) : (Int, Bool)[][] {
    if (nQubits == 10) {
        return Generate10QubitProblem();
    } elif (nQubits == 15) {
        return Generate15QubitProblem();
    } elif (nQubits == 20) {
        return Generate20QubitProblem();
    } elif (nQubits == 25) {
        return Generate25QubitProblem();
    } elif (nQubits > 25) { 
        let numClauses = nQubits * 4; 
        let (problemInstance, _knownSolution) = GenerateProblemWithKnownSolutionOp(nQubits, numClauses);
        return problemInstance;
    } else {
        Message($"Warning: Using GenerateRandomProblem for {nQubits} qubits. Solution not guaranteed by new method.");
        return GenerateRandomProblem(nQubits, nQubits * 4, 0.5); 
    }
}

operation GenerateRandomProblem(nQubits : Int, nClauses : Int, negationProbability : Double) : (Int, Bool)[][] {
    mutable problem = [];
    for _ in 0..nClauses-1 {
        mutable clause = [];
        // Ensure unique variables in a clause
        mutable varsInClause = [-1, -1, -1];
        for iLit in 0..2 {
            mutable varIdx = -1;
            mutable isUnique = false;
            repeat {
                set varIdx = DrawRandomInt(0, nQubits - 1); // 0 to nQubits-1
                set isUnique = true;
                for kChosen in 0..iLit-1 {
                    if varsInClause[kChosen] == varIdx {
                        set isUnique = false;
                    }
                }
            } until isUnique;
            set varsInClause w/= iLit <- varIdx;

            let isNegated = DrawRandomDouble(0.0, 1.0) < negationProbability;
            set clause += [(varIdx, isNegated)];
        }
        set problem += [clause];
    }
    return problem;
}

function Generate10QubitProblem() : (Int, Bool)[][] {
    return [
        [(0, false), (1, false), (2, false)],    // x0 OR x1 OR x2
        [(3, false), (4, false), (5, false)],    // x3 OR x4 OR x5
        [(6, false), (7, false), (8, false)],    // x6 OR x7 OR x8
        [(0, true), (3, true), (6, true)],       // NOT x0 OR NOT x3 OR NOT x6
        [(1, true), (4, true), (7, true)],       // NOT x1 OR NOT x4 OR NOT x7
        [(2, true), (5, true), (8, true)],       // NOT x2 OR NOT x5 OR NOT x8
        [(0, false), (4, false), (8, false)],    // x0 OR x4 OR x8
        [(1, false), (5, false), (6, true)],     // x1 OR x5 OR NOT x6
        [(2, false), (3, false), (7, true)],     // x2 OR x3 OR NOT x7
        [(9, false), (0, true), (5, true)]       // x9 OR NOT x0 OR NOT x5
    ];
}

function Generate15QubitProblem() : (Int, Bool)[][] {
    return [
        [(0, false), (1, false), (2, false)],     // x0 OR x1 OR x2
        [(3, false), (4, false), (5, false)],     // x3 OR x4 OR x5
        [(6, false), (7, false), (8, false)],     // x6 OR x7 OR x8
        [(9, false), (10, false), (11, false)],   // x9 OR x10 OR x11
        [(12, false), (13, false), (14, false)],  // x12 OR x13 OR x14
        [(0, true), (5, true), (10, true)],       // NOT x0 OR NOT x5 OR NOT x10
        [(1, true), (6, true), (11, true)],       // NOT x1 OR NOT x6 OR NOT x11
        [(2, true), (7, true), (12, true)],       // NOT x2 OR NOT x7 OR NOT x12
        [(3, true), (8, true), (13, true)],       // NOT x3 OR NOT x8 OR NOT x13
        [(4, true), (9, true), (14, true)],       // NOT x4 OR NOT x9 OR NOT x14
        [(0, false), (7, false), (14, false)],    // x0 OR x7 OR x14
        [(1, false), (8, false), (12, true)],     // x1 OR x8 OR NOT x12
        [(2, false), (9, false), (13, true)],     // x2 OR x9 OR NOT x13
        [(3, false), (10, false), (11, true)],    // x3 OR x10 OR NOT x11
        [(4, false), (5, false), (6, true)]       // x4 OR x5 OR NOT x6
    ];
}

function Generate20QubitProblem() : (Int, Bool)[][] {
    return [
        [(0, false), (1, false), (2, false)],     // x0 OR x1 OR x2
        [(3, false), (4, false), (5, false)],     // x3 OR x4 OR x5
        [(6, false), (7, false), (8, false)],     // x6 OR x7 OR x8
        [(9, false), (10, false), (11, false)],   // x9 OR x10 OR x11
        [(12, false), (13, false), (14, false)],  // x12 OR x13 OR x14
        [(15, false), (16, false), (17, false)],  // x15 OR x16 OR x17
        [(18, false), (19, false), (0, true)],    // x18 OR x19 OR NOT x0
        [(0, true), (5, true), (10, true)],       // NOT x0 OR NOT x5 OR NOT x10
        [(1, true), (6, true), (11, true)],       // NOT x1 OR NOT x6 OR NOT x11
        [(2, true), (7, true), (12, true)],       // NOT x2 OR NOT x7 OR NOT x12
        [(3, true), (8, true), (13, true)],       // NOT x3 OR NOT x8 OR NOT x13
        [(4, true), (9, true), (14, true)],       // NOT x4 OR NOT x9 OR NOT x14
        [(15, true), (16, true), (17, true)],     // NOT x15 OR NOT x16 OR NOT x17
        [(0, false), (7, false), (14, false)],    // x0 OR x7 OR x14
        [(1, false), (8, false), (15, false)],    // x1 OR x8 OR x15
        [(2, false), (9, false), (16, false)],    // x2 OR x9 OR x16
        [(3, false), (10, false), (17, false)],   // x3 OR x10 OR x17
        [(4, false), (11, false), (18, false)],   // x4 OR x11 OR x18
        [(5, false), (12, false), (19, false)],   // x5 OR x12 OR x19
        [(6, false), (13, false), (18, true)]     // x6 OR x13 OR NOT x18
    ];
}

function Generate25QubitProblem() : (Int, Bool)[][] {
    return [
        [(0, false), (1, false), (2, false)],     // x0 OR x1 OR x2
        [(3, false), (4, false), (5, false)],     // x3 OR x4 OR x5
        [(6, false), (7, false), (8, false)],     // x6 OR x7 OR x8
        [(9, false), (10, false), (11, false)],   // x9 OR x10 OR x11
        [(12, false), (13, false), (14, false)],  // x12 OR x13 OR x14
        [(15, false), (16, false), (17, false)],  // x15 OR x16 OR x17
        [(18, false), (19, false), (20, false)],  // x18 OR x19 OR x20
        [(21, false), (22, false), (23, false)],  // x21 OR x22 OR x23
        [(24, false), (0, true), (12, true)],     // x24 OR NOT x0 OR NOT x12
        [(0, true), (5, true), (10, true)],       // NOT x0 OR NOT x5 OR NOT x10
        [(1, true), (6, true), (11, true)],       // NOT x1 OR NOT x6 OR NOT x11
        [(2, true), (7, true), (12, true)],       // NOT x2 OR NOT x7 OR NOT x12
        [(3, true), (8, true), (13, true)],       // NOT x3 OR NOT x8 OR NOT x13
        [(4, true), (9, true), (14, true)],       // NOT x4 OR NOT x9 OR NOT x14
        [(15, true), (20, true), (24, true)],     // NOT x15 OR NOT x20 OR NOT x24
        [(16, true), (21, true), (0, false)],     // NOT x16 OR NOT x21 OR x0
        [(17, true), (22, true), (1, false)],     // NOT x17 OR NOT x22 OR x1
        [(18, true), (23, true), (2, false)],     // NOT x18 OR NOT x23 OR x2
        [(19, true), (24, true), (3, false)],     // NOT x19 OR NOT x24 OR x3
        [(0, false), (7, false), (14, false)],    // x0 OR x7 OR x14
        [(1, false), (8, false), (15, false)],    // x1 OR x8 OR x15
        [(2, false), (9, false), (16, false)],    // x2 OR x9 OR x16
        [(3, false), (10, false), (17, false)],   // x3 OR x10 OR x17
        [(4, false), (11, false), (18, false)],   // x4 OR x11 OR x18
        [(5, false), (12, false), (19, false)]    // x5 OR x12 OR x19
    ];
}