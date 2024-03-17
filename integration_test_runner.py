import glob
import re
import subprocess
import os
import sys

# Gets the expected return code from the .wacc file from wacc_examples
def get_return_code(fname):
  with open(fname) as f:
    lines = f.readlines()
    for i in range(len(lines)):
      if lines[i].startswith("# Exit:"):
        return int(re.search("[0-9]+", lines[i+1]).group())
  return 0

def parser_code(fname):
  if fname.startswith("wacc_examples_extension/extension_invalid"):
    return 200
  elif (not fname.startswith("wacc_examples/in")):
    return 0
  else:
    if "syntax" in fname:
      return 100
    elif "semantic" in fname:
      return 200
    else:
      return 0
    
def input_data(fname):
    with open(fname) as f:
        lines = f.readlines()
        for line in lines:
            if line.startswith("# Input:"):
                return line[len("# Input:"):].replace(' ', '\n')
    return ""

# Extract expected output from comments in WACC file
def extract_expected_output(fname):
  expected_output = ""
  with open(fname) as f:
    lines = f.readlines()
    output_line_found = False
    for i, line in enumerate(lines):
      if line.startswith("# Output:"):
        output_line_found = True
      elif output_line_found and (line == "\n"):
        break
      elif output_line_found:
        if (lines[i + 1].startswith("#") and not (lines[i + 1] == "#\n")):
          expected_output += line[2:]
        else:
          expected_output += line[2:-1]
    return expected_output

# Returns a list of all the directories in the base directory
def list_directories_in_directory(base):
  result = [name for name in os.listdir(base) if (os.path.isdir(os.path.join(base, name)) and name != 'whack')]
  return result

# Returns a list of all the files in the base directory
def list_files_in_directory(base):
    file_paths = []
    for root, dirs, files in os.walk(base):
        dirs[:] = [d for d in dirs if (d.lower() != 'whack' and d.lower() != 'advanced')]
        for file in files:
            file_paths.append(os.path.join(root, file))
    return file_paths

def compare_output(expected_output, actual_output):
  if not expected_output.strip() == actual_output:
    if '#' in expected_output:
      expected_output = re.sub(r'#.*#', '.*', expected_output, flags=re.DOTALL)
      expected_output = expected_output.replace("(", "\(").replace(")", "\)")
      pattern = re.compile(expected_output, flags=re.DOTALL)
      return pattern.fullmatch(actual_output.strip()) is not None
    else:
      return False
  return True

def runningExtension():
  for arg in sys.argv:
    if "extension" in arg:
      return True
  if len(sys.argv) < 2 or (len(sys.argv) == 2 and "-o" in sys.argv):
    return True
  return False

# Adds all the tests to the tests dictionary
def add_tests_to_dict(base) :
  tests = dict()
  for directory in list_directories_in_directory(base):
    # Added valid and invalid directories to the tests dictionary
    tests[directory] = list_files_in_directory(base + directory + "/")

    # Added subdirectoried of valid and invalid directories to the tests dictionary
    for subdirectory in list_directories_in_directory(base + directory + "/"):
      subdirectoryName = subdirectory
      if subdirectory.endswith("Err"):
        subdirectoryName = subdirectory[:-3]
      tests[directory + "-" + subdirectoryName] = list_files_in_directory(base + directory + "/" + subdirectory + "/")

      # Added subdirectories of invalid directory to the tests dictionary
      if directory == "invalid":
        for subsubdirectory in list_directories_in_directory(base + directory + "/" + subdirectory + "/"):
          tests[directory + "-" + subdirectory[:-3] + "-" + subsubdirectory] = \
            list_files_in_directory(base + directory + "/" + subdirectory + "/" + subsubdirectory + "/")
  return tests

def compile_run_assembly_file(fname, assembly_file):
  # Compile the assembly file
  print(f"arm-linux-gnueabi-gcc -o execFile -z noexecstack -march=armv6 {assembly_file}")
  subprocess.run(["arm-linux-gnueabi-gcc", "-o", "execFile", "-z", "noexecstack", "-march=armv6", assembly_file])

  if os.path.exists("execFile"):
    input_data_str = input_data(fname)

    # Run the executable file
    print("qemu-arm -L /usr/arm-linux-gnueabi/ execFile")
    try:
      output_str = subprocess.run(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", "execFile"], input=input_data_str.encode(), capture_output=True, timeout=20)
    except subprocess.TimeoutExpired:
      print("Test timed out.")
      errorTests.append(fname)
      return False

    output = output_str.stdout.decode('utf-8', errors='replace')

    expected_output = extract_expected_output(fname)

    if compare_output(expected_output, output.strip()):
      print("Output matches expected!")
      if output_str.returncode != get_return_code(fname):
        print(f"Expected return code {get_return_code(fname)} but got {output_str.returncode}")
        errorTests.append(fname)
        return False
      else:
        return True
    else:
      print("Output does not match expected.")
      print(f"Expected output: {expected_output}")
      print(f"Actual output: {output.strip()}")
      errorTests.append(fname)
      return False
  else:
    print("Executable file not found.")
    errorTests.append(fname)
    return False

def run_tests(tests_to_run, flags):
  syntaxPasses = 0
  semanticPasses = 0
  validPasses = 0
  extensionValidPasses = 0
  extensionInvalidPasses = 0

  # Dictionary of test categories to their respective counts
  validSubTests = {
    "basic": (0, 0, "Basic"),
    "sequence": (0, 0, "Sequence"),
    "IO": (0, 0, "Input/Output"),
    "variables": (0, 0, "Variable"),
    "expressions": (0, 0, "Expression"),
    "array": (0, 0, "Array"),
    "if": (0, 0, "Conditional"),
    "while": (0, 0, "Loop"),
    "scope": (0, 1, "Scope"),
    "simple_functions": (0, 3, "Simple Function"),
    "nested_functions": (0, 0, "Nested Function"),
    "runtimeErr": (0, 0, "Runtime Error"),
    "heap": (0, 0, "Heap")
  }

  # Dictionary of test categories to their respective counts
  validExtensionSubTests = {
    "function_overloading": (0, 0, "Function Overloading"),
    "standard_library": (0, 0, "Standard Library"),
    "full_pair_types": (0, 0, "Full Pair Types")
  }

  for test in tests_to_run:
    for fname in glob.glob(test):
      if flags != "":
        print(f"sh compile {flags} {fname}")
        proc = subprocess.run(["sh", "compile", flags, fname], stdout=subprocess.DEVNULL)
      else:
        print(f"sh compile {fname}")
        proc = subprocess.run(["sh", "compile", fname], stdout=subprocess.DEVNULL)

      actual = proc.returncode
      expected = parser_code(fname)

      if actual == expected:
        if fname.startswith("wacc_examples/in") or fname.startswith("wacc_examples_extension/extension_in"):
          if "syntax" in fname:
            syntaxPasses += 1
          elif "semantic" in fname:
            semanticPasses += 1
          elif "extension" in fname:
            extensionInvalidPasses += 1
        else:
          # If compilation was successful, run the corresponding assembly file
          assembly_file = os.path.basename(fname).replace('.wacc', '.s')

          if os.path.exists(assembly_file):
            validSubDir = ""

            if "simple_functions/" in fname:
              validSubDir = "simple_functions"
            elif "nested_functions/" in fname:
              validSubDir = "nested_functions"
            elif "pairs/" in fname:
              validSubDir = "heap"
            elif "function_overloading/" in fname:
              validSubDir = "function_overloading"
            elif "standard_library/" in fname:
              validSubDir = "standard_library"
            elif "full_pair_types/" in fname:
              validSubDir = "full_pair_types"
            else:
              fileNameParts = fname.split("/")
              validSubDir = fileNameParts[2]

            if fname.startswith("wacc_examples/valid"):
              value = validSubTests[validSubDir]
              validSubTests[validSubDir] = (value[0], value[1] + 1, value[2])
            else:
              value = validExtensionSubTests[validSubDir]
              validExtensionSubTests[validSubDir] = (value[0], value[1] + 1, value[2])

            # Compile the assembly file
            if compile_run_assembly_file(fname, assembly_file):
              if "extension" in fname:
                extensionValidPasses += 1
              else:
                validPasses += 1

              if fname.startswith("wacc_examples/valid"):
                value = validSubTests[validSubDir]
                validSubTests[validSubDir] = (value[0] + 1, value[1], value[2])
              else:
                value = validExtensionSubTests[validSubDir]
                validExtensionSubTests[validSubDir] = (value[0] + 1, value[1], value[2])

            # Remove the assembly and executable files
            os.remove(assembly_file)
            if os.path.exists("execFile"):
              os.remove("execFile")
          else:
            print(f"Assembly file {assembly_file} not found.")
            errorTests.append(fname)
      else:
        print(f"Failed test {fname}. Expected exit code {expected} but got {actual}")
        errorTests.append(fname)
    print("")
  return (syntaxPasses, semanticPasses, validPasses, validSubTests, extensionInvalidPasses, extensionValidPasses)

base = "wacc_examples/"
extension_base = "wacc_examples_extension/"
tests = add_tests_to_dict(base)
tests.update(add_tests_to_dict(extension_base))
runningTests = []

# If no arguments are given, run all tests
if len(sys.argv) < 2 or (len(sys.argv) == 2 and ("-o" in sys.argv or "--graph-colouring" in sys.argv)) or (len(sys.argv) == 3 and "-o" in sys.argv and "--graph-colouring" in sys.argv):
  runningTests = tests["valid"] + tests["invalid"] + tests["extension_invalid"] + tests["extension_valid"]
elif sys.argv[1] == "--syntax":
  runningTests = tests["invalid-syntax"] + tests["valid"]
elif sys.argv[1] == "--semantic":
  runningTests = tests["invalid-semantic"] + tests["valid"]
elif (len(sys.argv) == 2) or (len(sys.argv) == 3 and "-o" in sys.argv) or (len(sys.argv) == 3 and "--graph-colouring" in sys.argv) or (len(sys.argv) == 4 and "-o" in sys.argv and "--graph-colouring" in sys.argv):
  runningTests = tests[sys.argv[1]]
else:
  if sys.argv[1] == "valid":
    if not ('-debug' in sys.argv):
      runningTests = tests["invalid"]
    else:
      sys.argv = sys.argv[:-1]

    if not ('-o' in sys.argv):
      runningTests = tests["invalid"]
    else:
      sys.argv = sys.argv[:-1]

    tags = ['-'.join([sys.argv[1], tag]) for tag in sys.argv[2:]]
  else:
    if len(sys.argv) == 3:
      tags = ['-'.join(sys.argv[1:])]
    else:
      tags = ['-'.join([sys.argv[1], sys.argv[2], tag]) for tag in sys.argv[3:]]
  for tag in tags:
    if tag in tests:
      runningTests += tests[tag]
    else:
      print(f"Test tag {tag} not found")
      sys.exit(1)

errorTests = []
ignoredTests = ["wacc_examples/valid/scope/printAllTypes.wacc",
                "wacc_examples/valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc",
                "wacc_examples/valid/function/simple_functions/manyArgumentsChar.wacc",
                "wacc_examples/valid/function/simple_functions/manyArgumentsInt.wacc",
                "wacc_examples_extension/extension_valid/standard_library/median.wacc",
                "wacc_examples_extension/extension_valid/standard_library/mode.wacc",
                "wacc_examples_extension/extension_valid/standard_library/sort.wacc",
                "wacc_examples/invalid/syntaxErr/pairs/noNesting.wacc"]
runningTests = [test for test in runningTests if test not in ignoredTests]
print("Running tests...")

flags = ""

# Run the tests
if "-o" in sys.argv:
  flags = "-o"

if "--graph-colouring" in sys.argv:
  if flags == "":
    flags = "--graph-colouring"
  else:
    flags = flags + " --graph-colouring"

syntaxPasses, semanticPasses, validPasses, validSubTests, extensionInvalidPasses, extensionValidPasses = run_tests(runningTests, flags)
syntaxTotal = len(tests["invalid-syntax"])
semanticTotal = len(tests["invalid-semantic"])
validTotal = len(tests["valid"])
extentionInvalidTotal = len(tests["extension_invalid"])
extensionValidTotal = len(tests["extension_valid"])
extensionTotal = extentionInvalidTotal + extensionValidTotal

totalPasses = validPasses + syntaxPasses + semanticPasses + extensionValidPasses + extensionInvalidPasses
numberTests = validTotal + syntaxTotal + semanticTotal + extensionTotal
numberIgnored = numberTests - len(runningTests)

for file in glob.glob(os.path.join(".", '*.s')):
  os.remove(file)

if len(sys.argv) >= 2 and sys.argv[1] == "--syntax":
  print(f"Finished running tests. Results: ")
  print(f"    Valid: {validPasses} / {validTotal}")

  for argName, countsAndPrintName in validSubTests.items():
    passCount, totalCount, printName = countsAndPrintName
    print(f"    {printName} Tests: {passCount} / {totalCount}")

  print(f"    Syntax: {syntaxPasses} / {syntaxTotal}")
  print(f"Total: {totalPasses} / {numberTests}")
  print(f"Ignored: {numberIgnored} tests")
elif len(sys.argv) >= 2 and sys.argv[1] == "--semantic":
  print(f"Finished running tests. Results: ")
  print(f"    Valid: {validPasses} / {validTotal}")

  for argName, countsAndPrintName in validSubTests.items():
    passCount, totalCount, printName = countsAndPrintName
    print(f"    {printName} Tests: {passCount} / {totalCount}")

  print(f"    Semantic: {semanticPasses} / {semanticTotal}")
  print(f"Total: {totalPasses} / {numberTests}")
  print(f"Ignored: {numberIgnored} tests")
else:
  print("Finished running tests. Results: ")
  print(f"Valid: {validPasses} / {validTotal}")

  for argName, countsAndPrintName in validSubTests.items():
    passCount, totalCount, printName = countsAndPrintName
    print(f"    {printName} Tests: {passCount} / {totalCount}")

  print(f"Invalid: {syntaxPasses + semanticPasses} / {syntaxTotal + semanticTotal}")
  print(f"    Syntax: {syntaxPasses} / {syntaxTotal}")
  print(f"    Semantic: {semanticPasses} / {semanticTotal}")
  if runningExtension():
    print(f"Extension: {extensionValidPasses + extensionInvalidPasses} / {extensionTotal}")
    print(f"    Invalid: {extensionInvalidPasses} / {extentionInvalidTotal}")
    print(f"    Valid: {extensionValidPasses} / {extensionValidTotal}")
  print(f"Total: {totalPasses} / {numberTests}")
  print(f"Ignored: {numberIgnored} tests")

errorTests = [test for test in errorTests if test not in ignoredTests]
if len(errorTests) > 0:
  print("")
  print("Failed tests:")
  for fname in errorTests:
    print(fname)
  sys.exit(1)
