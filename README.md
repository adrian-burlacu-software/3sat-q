# 3SAT-Q

Quantum 3SAT Solution

---

## Quantum 3SAT Solver using Grover's Algorithm

This Q# program implements Grover's search algorithm to solve instances of the 3SAT problem. The 3SAT problem is a classic NP-complete problem where the goal is to determine if there exists an assignment to boolean variables that satisfies a given set of clauses, each containing three literals.

The program encodes a 3SAT instance as a quantum oracle and uses Grover's algorithm to search for satisfying assignments. It includes:

- Construction of the 3SAT oracle (ReflectAbout3SatSolution)
- Grover's diffusion operator (ReflectAboutUniform)
- Iterative search for all solutions, excluding previously found ones
- Utility functions for result conversion and solution checking

Example usage encodes a 3SAT instance and runs Grover's algorithm to find satisfying assignments.

**Main components:**

- `Main`: Sets up the problem and runs the search
- `GroverSearch`: Core Grover's loop with solution exclusion
- `ReflectAbout3SatSolution`: Oracle marking satisfying assignments
- `ReflectAboutUniform`: Grover diffusion operator
- Utility functions: `ResultArrayAsInt`, `Is3SatSolution`, `ContainsInt`

Adapted from Microsoft Q# samples.

## Example

```qs
    // Example 3SAT problem:
    // (x0 OR NOT x1 OR x2) AND (NOT x0 OR x1 OR x2)
    // Non-unique solution: x0 = false, x1 = true, x2 = true
    [
      // Clause 1: (x0 OR NOT x1 OR x2)
      [(0, false), (1, true), (2, false)],
      // Clause 2: (NOT x0 OR x1 OR x2)
      [(0, true), (1, false), (2, false)]
    ]
```
