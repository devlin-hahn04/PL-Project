import subprocess
import os

test_cases = [
    # ============================================
    # BASIC ARITHMETIC
    # ============================================
    ("Basic Addition", "exec 5 + 3", "8"),
    ("Basic Subtraction", "exec 10 - 4", "6"),
    ("Basic Multiplication", "exec 5 * 3", "15"),
    ("Basic Division", "exec 20 / 4", "5"),
    ("Complex Precedence", "exec 5 + 3 * 2", "11"),
    ("Parentheses Override", "exec (5 + 3) * 2", "16"),
    ("Nested Parentheses", "exec ((5 + 3) * 2) - 4", "12"),
    ("Mixed Operators", "exec 100 - 50 + 25 * 2 / 5", "60"),
    ("Multiple Parentheses", "exec (10 - 3) * (8 / 2)", "28"),
    
    # ============================================
    # LET EXPRESSIONS
    # ============================================
    ("Simple Let", "exec (let val x := 5 end in x * 2 end)", "10"),
    ("Let with Multiple Vars", "exec (let val x := 5 val y := 10 end in x + y end)", "15"),
    ("Let with Subtraction", "exec (let val x := 20 val y := 8 end in x - y end)", "12"),
    ("Let with Division", "exec (let val x := 100 val y := 4 end in x / y end)", "25"),
    ("Nested Let", "exec (let val x := 5 end in (let val y := 3 end in x * y end) end)", "15"),
    ("Let with Expression", "exec (let val x := 5 end in x * x + x end)", "30"),
    ("Let with Multiple Operations", "exec (let val a := 2 val b := 3 val c := 4 end in a * b + c end)", "10"),
    
    # ============================================
    # BASIC FUNCTIONS
    # ============================================
    ("Add Function", "func add[x, y] := x + y end\nexec add[5, 7]", "12"),
    ("Multiply Function", "func mul[x, y] := x * y end\nexec mul[6, 7]", "42"),
    ("Subtract Function", "func sub[x, y] := x - y end\nexec sub[20, 8]", "12"),
    ("Divide Function", "func div[x, y] := x / y end\nexec div[100, 4]", "25"),
    ("Square Function", "func square[x] := x * x end\nexec square[9]", "81"),
    ("Double Function", "func double[x] := x * 2 end\nexec double[25]", "50"),
    ("Half Function", "func half[x] := x / 2 end\nexec half[100]", "50"),
    
    # ============================================
    # COMPLEX FUNCTIONS
    # ============================================
    ("Function with Multiple Ops", "func calculate[x, y] := (x * 2) + (y * 3) end\nexec calculate[5, 4]", "22"),
    ("Chained Functions", "func double[x] := x * 2 end\nfunc triple[x] := x * 3 end\nexec triple[double[5]]", "30"),
    ("Complex Compute", "func compute[a, b] := (a * b) + (a + b) end\nexec compute[5, 3]", "23"),
    ("Nested Function Calls", "func addOne[x] := x + 1 end\nfunc double[x] := x * 2 end\nexec double[addOne[10]]", "22"),
    ("Deep Nested Calls", "func addOne[x] := x + 1 end\nfunc double[x] := x * 2 end\nfunc triple[x] := x * 3 end\nexec triple[double[addOne[10]]]", "66"),
    ("Function with Complex Body", "func complex[a] := (a * a * 2) + a end\nexec complex[5]", "55"),
    
    # ============================================
    # IF STATEMENTS - BASIC
    # ============================================
    ("Simple IF true", "exec if 1 then 100 else 0 end", "100"),
    ("Simple IF false", "exec if 0 then 100 else 0 end", "0"),
    ("IF with true condition", "exec if 5 < 10 then 1 else 0 end", "1"),
    ("IF with false condition", "exec if 10 < 5 then 1 else 0 end", "0"),
    ("IF with greater than true", "exec if 10 > 5 then 1 else 0 end", "1"),
    ("IF with greater than false", "exec if 5 > 10 then 1 else 0 end", "0"),
    ("IF with equals true", "exec if 5 = 5 then 1 else 0 end", "1"),
    ("IF with equals false", "exec if 5 = 10 then 1 else 0 end", "0"),
    
    # ============================================
    # IF STATEMENTS WITH FUNCTIONS
    # ============================================
    ("Max Function", "func max[a, b] := if a > b then a else b end end\nexec max[10, 5]", "10"),
    ("Max Function Equal", "func max[a, b] := if a > b then a else b end end\nexec max[7, 7]", "7"),
    ("Min Function", "func min[a, b] := if a < b then a else b end end\nexec min[10, 5]", "5"),
    ("Abs Function", "func abs[x] := if x < 0 then 0 - x else x end end\nexec abs[-15]", "15"),
    ("Abs Positive", "func abs[x] := if x < 0 then 0 - x else x end end\nexec abs[42]", "42"),
    ("Sign Function", "func sign[x] := if x > 0 then 1 else if x < 0 then -1 else 0 end end end\nexec sign[10]", "1"),
    
    # ============================================
    # LOGICAL OPERATORS
    # ============================================
    ("AND true/true", "exec if (5 < 10) & (10 > 5) then 1 else 0 end", "1"),
    ("AND true/false", "exec if (5 < 10) & (10 < 5) then 1 else 0 end", "0"),
    ("AND false/true", "exec if (5 > 10) & (10 > 5) then 1 else 0 end", "0"),
    ("AND false/false", "exec if (5 > 10) & (10 < 5) then 1 else 0 end", "0"),
    ("OR true/true", "exec if (5 < 10) | (10 > 5) then 1 else 0 end", "1"),
    ("OR true/false", "exec if (5 < 10) | (10 < 5) then 1 else 0 end", "1"),
    ("OR false/true", "exec if (5 > 10) | (10 > 5) then 1 else 0 end", "1"),
    ("OR false/false", "exec if (5 > 10) | (10 < 5) then 1 else 0 end", "0"),
    ("AND in arithmetic", "exec if (5 < 10) & (3 * 2 = 6) then 100 else 0 end", "100"),
    ("OR in arithmetic", "exec if (5 > 10) | (4 * 2 = 8) then 100 else 0 end", "100"),
    
    # ============================================
    # RECURSIVE FUNCTIONS
    # ============================================
    ("Factorial 0", "func fact[n] := if n = 0 then 1 else n * fact[n-1] end end\nexec fact[0]", "1"),
    ("Factorial 1", "func fact[n] := if n = 0 then 1 else n * fact[n-1] end end\nexec fact[1]", "1"),
    ("Factorial 5", "func fact[n] := if n = 0 then 1 else n * fact[n-1] end end\nexec fact[5]", "120"),
    ("Factorial 7", "func fact[n] := if n = 0 then 1 else n * fact[n-1] end end\nexec fact[7]", "5040"),
    ("Fibonacci 0", "func fib[n] := if n < 2 then n else fib[n-1] + fib[n-2] end end\nexec fib[0]", "0"),
    ("Fibonacci 1", "func fib[n] := if n < 2 then n else fib[n-1] + fib[n-2] end end\nexec fib[1]", "1"),
    ("Fibonacci 7", "func fib[n] := if n < 2 then n else fib[n-1] + fib[n-2] end end\nexec fib[7]", "13"),
    ("Fibonacci 10", "func fib[n] := if n < 2 then n else fib[n-1] + fib[n-2] end end\nexec fib[10]", "55"),
    
    # ============================================
    # COMPARISON OPERATORS
    # ============================================
    ("Less than true", "exec 5 < 10", "True"),
    ("Less than false", "exec 10 < 5", "False"),
    ("Greater than true", "exec 10 > 5", "True"),
    ("Greater than false", "exec 5 > 10", "False"),
    ("Equal true", "exec 5 = 5", "True"),
    ("Equal false", "exec 5 = 10", "False"),
    ("Less than or equal (via OR)", "exec (5 < 10) | (5 = 10)", "True"),
    ("Greater than or equal (via OR)", "exec (10 > 5) | (10 = 5)", "True"),
    
    # ============================================
    # STRING OPERATIONS
    # ============================================
    ("Simple String", 'exec "Hello"', "Hello"),
    ("String Concatenation", 'exec "Hello" . " " . "World"', "Hello World"),
    ("String with Multiple Concats", 'exec "A" . "B" . "C" . "D"', "ABCD"),
    ("String and Number Concatenation", 'exec "Number: " . 42', "Number: 42"),
    ("String in Let", 'exec (let val s := "Hello" end in s . " World" end)', "Hello World"),
    
    # ============================================
    # COMBINED FEATURES
    # ============================================
    ("Let with IF", "exec (let val x := 5 end in if x = 5 then 10 else 0 end end)", "10"),
    ("Function returning IF result", "func isPositive[x] := if x > 0 then 1 else 0 end end\nexec isPositive[10]", "1"),
    ("Let with Function Call", "func square[x] := x * x end\nexec (let val x := 5 end in square[x] end)", "25"),
    ("Nested IF in Function", "func compare[a,b] := if a > b then 1 else if a < b then -1 else 0 end end end\nexec compare[10,5]", "1"),
    ("Complex Expression with Everything", "func power[x,n] := if n = 0 then 1 else x * power[x, n-1] end end\nexec (let val base := 2 val exp := 5 end in power[base, exp] end)", "32"),
    
    # ============================================
    # BOOLEAN AND NIL VALUES
    # ============================================
    ("Boolean True", "exec true", "True"),
    ("Boolean False", "exec false", "False"),
    ("Nil Value", "exec nil", "None"),
    ("IF with Boolean", "exec if true then 100 else 0 end", "100"),
    ("IF with False", "exec if false then 100 else 0 end", "0"),
    
    # ============================================
    # EDGE CASES
    # ============================================
    ("Zero Division in IF (not executed)", "exec if 0 then 10 / 0 else 42 end", "42"),
    ("Large Numbers", "exec 1000 * 1000", "1000000"),
    ("Negative Numbers", "exec -5 + 10", "5"),
    ("Multiple Let Bindings", "exec (let val a := 1 val b := 2 val c := 3 val d := 4 end in a + b + c + d end)", "10"),
]

print("=" * 80)
print("🚀 SUPER TEST SUITE - COMPLETE INTERPRETER TESTING")
print("=" * 80)
print(f"Total Tests: {len(test_cases)}")
print("=" * 80)
print()

passed = 0
failed = 0
failed_tests = []

for i, (name, code, expected) in enumerate(test_cases, 1):
    # Create temp file
    temp_file = f"_temp_test_{i}.txt"
    with open(temp_file, 'w') as f:
        f.write(code)
    
    # Run parser
    result = subprocess.run(
        ["python", "main.py", temp_file],
        capture_output=True,
        text=True,
        timeout=10
    )
    
    output = result.stdout
    
    # Check for expected result
    expected_str = f"Result: {expected}"
    if expected_str in output:
        print(f"✅ Test {i:3d}: {name}")
        passed += 1
    else:
        # Also check if it's a runtime error that matches expected behavior
        if "Runtime error" in output and expected == "Error":
            print(f"✅ Test {i:3d}: {name} (Expected Error)")
            passed += 1
        else:
            print(f"❌ Test {i:3d}: {name} - FAILED")
            print(f"   Expected: {expected}")
            print(f"   Got: {output[:200] if output else 'No output'}")
            failed += 1
            failed_tests.append((name, code, expected, output))
    
    # Progress indicator
    if i % 20 == 0:
        print(f"   Progress: {i}/{len(test_cases)} tests completed")
    
    print()  # Add blank line between tests for readability
    
    # Cleanup
    if os.path.exists(temp_file):
        os.remove(temp_file)

print("=" * 80)
print("📊 TEST RESULTS SUMMARY")
print("=" * 80)
print(f"✅ Passed: {passed}")
print(f"❌ Failed: {failed}")
print(f"📈 Total: {len(test_cases)}")
print(f"🏆 Success Rate: {passed/len(test_cases)*100:.1f}%")
print("=" * 80)

if failed > 0:
    print("\n❌ FAILED TESTS DETAILS:")
    print("-" * 60)
    for idx, (name, code, expected, output) in enumerate(failed_tests[:10], 1):
        print(f"{idx}. {name}")
        print(f"   Code: {code[:100]}")
        print(f"   Expected: {expected}")
        print(f"   Got: {output[:150]}")
        print()
    if len(failed_tests) > 10:
        print(f"... and {len(failed_tests) - 10} more failures")

if passed == len(test_cases):
    print("\n🎉🎉🎉 PERFECT SCORE! ALL TESTS PASSED! 🎉🎉🎉")
    print("Your interpreter is FULLY FUNCTIONAL and PRODUCTION READY!")
elif passed >= len(test_cases) * 0.9:
    print(f"\n👍 Good work! {passed}/{len(test_cases)} tests passed.")
    print("Your interpreter is mostly working with minor issues.")
else:
    print(f"\n⚠️ {failed} test(s) failed. Review the failing tests above.")
