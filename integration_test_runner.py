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
  if (not fname.startswith("wacc_examples/in")):
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
      elif output_line_found and (line == "# Program:\n" or line == "# Exit:\n"):
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

# Adds all the tests to the tests dictionary
def add_tests_to_dict(base):
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

  input_data_str = input_data(fname)

  # Run the executable file
  print("qemu-arm -L /usr/arm-linux-gnueabi/ execFile")
  output = subprocess.run(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", "execFile"], input=input_data_str, text=True, capture_output=True)

  expected_output = extract_expected_output(fname)

  if output.stdout.strip() == expected_output:
    print("Output matches expected!")
    if output.returncode != get_return_code(fname):
      print(f"Expected return code {get_return_code(fname)} but got {output.returncode}")
      errorTests.append(fname)
      return False
    else:
      return True
  else:
    print("Output does not match expected.")
    print(f"Expected output: {expected_output}")
    print(f"Actual output: {output.stdout.strip()}")
    errorTests.append(fname)
    return False

def run_tests(tests_to_run):
  syntaxPasses = 0
  semanticPasses = 0
  validPasses = 0

  for test in tests_to_run:
    for fname in glob.glob(test):
      print(f"sh compile {fname}")
      proc = subprocess.run(["sh", "compile", fname], stdout=subprocess.DEVNULL)

      actual = proc.returncode
      expected = parser_code(fname)

      if actual == expected:
        if fname.startswith("wacc_examples/in"):
          if "syntax" in fname:
            syntaxPasses += 1
          elif "semantic" in fname:
            semanticPasses += 1
        else:
          # If compilation was successful, run the corresponding assembly file
          assembly_file = os.path.basename(fname).replace('.wacc', '.s')

          if os.path.exists(assembly_file):
            # Compile the assembly file
            if compile_run_assembly_file(fname, assembly_file):
              validPasses += 1

            # Remove the assembly and executable files
            os.remove(assembly_file)
          else:
            print(f"Assembly file {assembly_file} not found.")
            errorTests.append(fname)
      else:
        print(f"Failed test {fname}. Expected exit code {expected} but got {actual}")
        errorTests.append(fname)
    print("")
  return (syntaxPasses, semanticPasses, validPasses)

base = "wacc_examples/"
tests = add_tests_to_dict(base)
runningTests = []

# If no arguments are given, run all tests
if len(sys.argv) < 2:
  runningTests = tests["valid"] + tests["invalid"]
elif sys.argv[1] == "--syntax":
  runningTests = tests["invalid-syntax"] + tests["valid"]
elif sys.argv[1] == "--semantic":
  runningTests = tests["invalid-semantic"] + tests["valid"]
elif len(sys.argv) == 2:
  runningTests = tests[sys.argv[1]]
else:
  if sys.argv[1] == "valid":
    if not ('-debug' in sys.argv):
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

print("Running tests...")

# Run the tests
syntaxPasses, semanticPasses, validPasses = run_tests(runningTests)
syntaxTotal = len(tests["invalid-syntax"])
semanticTotal = len(tests["invalid-semantic"])
validTotal = len(tests["valid"])

totalPasses = validPasses + syntaxPasses + semanticPasses
total = validTotal + syntaxTotal + semanticTotal
numberTests = len(tests['valid'] + tests['invalid'])
numberIgnored = len(tests['valid'] + tests['invalid']) - len(runningTests)

print("")
if len(sys.argv) >= 2 and sys.argv[1] == "--syntax":
  print(f"Finished running tests. Results: ")
  print(f"    Valid: {validPasses} / {validTotal}")
  print(f"    Syntax: {syntaxPasses} / {syntaxTotal}")
  print(f"Total: {totalPasses} / {numberTests}")
  print(f"Ignored: {numberIgnored} tests")
elif len(sys.argv) >=2 and sys.argv[1] == "--semantic":
  print(f"Finished running tests. Results: ")
  print(f"    Valid: {validPasses} / {validTotal}")
  print(f"    Semantic: {semanticPasses} / {semanticTotal}")
  print(f"Total: {totalPasses} / {numberTests}")
  print(f"Ignored: {numberIgnored} tests")
else:
  print("Finished running tests. Results: ")
  print(f"Valid: {validPasses} / {validTotal}")
  print(f"Invalid: {syntaxPasses + semanticPasses} / {syntaxTotal + semanticTotal}")
  print(f"    Syntax: {syntaxPasses} / {syntaxTotal}")
  print(f"    Semantic: {semanticPasses} / {semanticTotal}")
  print(f"Total: {totalPasses} / {numberTests}")
  print(f"Ignored: {numberIgnored} tests")

if len(errorTests) > 0:
  print("")
  print("Failed tests:")
  for fname in errorTests:
    print(fname)
  sys.exit(1)
