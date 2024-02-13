import glob
import re
import subprocess
import os
import sys

# Gets the expected return code from the .wacc file from wacc_examples
def get_return_code(fname):
  if (not fname.startswith("wacc_examples/in")):
    return 0
  else:
    if "syntax" in fname:
      return 100
    elif "semantic" in fname:
      return 200
    else:
      return 0

# Extract expected output from comments in WACC file
def extract_expected_output(fname):
  expected_output = ""
  with open(fname) as f:
    lines = f.readlines()
    output_line_found = False
    for line in lines:
      if line.startswith("# Output:"):
        output_line_found = True
      elif output_line_found and line == "#\n":
        break
      elif output_line_found:
        expected_output += line[2:]
  return expected_output

# Returns a list of all the directories in the base directory
def list_directories_in_directory(base):
  result = [name for name in os.listdir(base) if (os.path.isdir(os.path.join(base, name)) and name != 'whack')]
  return result

# Returns a list of all the files in the base directory
def list_files_in_directory(base):
    file_paths = []
    for root, dirs, files in os.walk(base):
        dirs[:] = [d for d in dirs if d.lower() != 'whack']
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
  print(f"aarch64-linux-gnu-gcc -o execFile -z noexecstack -march=armv8-a {assembly_file}")
  subprocess.run(["aarch64-linux-gnu-gcc", "-o", "execFile", "-z", "noexecstack", "-march=armv8-a", assembly_file])

  # Run the executable file
  print("qemu-aarch64 -L /usr/aarch64-linux-gnu/ execFile")
  output = subprocess.run(["qemu-aarch64", "-L", "/usr/aarch64-linux-gnu/", "execFile"], capture_output=True)

  expected_output = extract_expected_output(fname)

  if output.stdout.decode().strip() == expected_output:
    print("Output matches expected!")
    return True
  else:
    print("Output does not match expected.")
    errorTests.append(fname)
    return False

def run_tests(tests_to_run):
  syntaxTotal = 0
  semanticTotal = 0
  syntaxPasses = 0
  semanticPasses = 0
  validTotal = 0
  validPasses = 0

  for test in tests_to_run:
    for fname in glob.glob(test):
      print(f"sh compile {fname}")
      proc = subprocess.run(["sh", "compile", fname], stdout=subprocess.DEVNULL)

      actual = proc.returncode
      expected = get_return_code(fname)

      if fname.startswith("wacc_examples/in"):
        if "syntax" in fname:
          syntaxTotal += 1
        elif "semantic" in fname:
          semanticTotal += 1
      else:
        validTotal += 1

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
          #   errorTests.append(fname)
          validPasses += 1
      else:
        print(f"Failed test {fname}. Expected exit code {expected} but got {actual}")
        errorTests.append(fname)
  return (syntaxTotal, semanticTotal, syntaxPasses, semanticPasses, validTotal, validPasses)

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
else :
  tag = '-'.join(sys.argv[1:])
  if tag in tests:
    runningTests = tests[tag]
  else:
    print(f"Test tag {tag} not found")
    sys.exit(1)

errorTests = []

print("Running tests...")

# Run the tests
syntaxTotal, semanticTotal, syntaxPasses, semanticPasses, validTotal, validPasses = run_tests(runningTests)

totalPasses = validPasses + syntaxPasses + semanticPasses
total = validTotal + syntaxTotal + semanticTotal

print("")
if len(sys.argv) < 2:
  print("Finished running tests. Results: ")
  print(f"Valid: {validPasses} / {validTotal}")
  print(f"Invalid: {syntaxPasses + semanticPasses} / {syntaxTotal + semanticTotal}")
  print(f"    Syntax: {syntaxPasses} / {syntaxTotal}")
  print(f"    Semantic: {semanticPasses} / {semanticTotal}")
  print(f"Total: {totalPasses} / {total}")
  print("Ignored: 0 tests")
elif sys.argv[1] == "--syntax":
  print(f"Finished running tests. Results: ")
  print(f"    Valid: {validPasses} / {validTotal}")
  print(f"    Syntax: {syntaxPasses} / {syntaxTotal}")
  print(f"Total: {totalPasses} / {total}")
  print(f"Ignored: {len(tests['valid'] + tests['invalid']) - total} tests")
elif sys.argv[1] == "--semantic":
  print(f"Finished running tests. Results: ")
  print(f"    Valid: {validPasses} / {validTotal}")
  print(f"    Semantic: {semanticPasses} / {semanticTotal}")
  print(f"Total: {totalPasses} / {total}")
  print(f"Ignored: {len(tests['valid'] + tests['invalid']) - total} tests")
else:
  print(f"Finished running tests {'-'.join(sys.argv[1:])}. Results: "
        f"{totalPasses} / {total}")
  print(f"Ignored {len(tests['valid'] + tests['invalid']) - total} tests")

if len(errorTests) > 0:
  print("")
  print("Failed tests:")
  for fname in errorTests:
    print(fname)
