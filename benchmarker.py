import glob
import re
import subprocess
import os
import sys
    
def input_data(fname):
    with open(fname) as f:
        lines = f.readlines()
        for line in lines:
            if line.startswith("# Input:"):
                return line[len("# Input:"):].replace(' ', '\n')
    return ""

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


base = "wacc_examples/"
extension_base = "wacc_examples_extension/"
tests = add_tests_to_dict(base)
tests.update(add_tests_to_dict(extension_base))
runningTests = []

# If no arguments are given, run all tests
if len(sys.argv) < 2 or (len(sys.argv) == 2 and "-o" in sys.argv):
  runningTests = tests["valid"] + tests["extension_valid"]

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

def benchmark_tests():
    total_compile_time = 0
    total_optimising_time = 0
    total_time = 0
    subprocess.run(["make", "clean"])
    subprocess.run(["make"])
    for test in runningTests:
        output = subprocess.run(["./wacc-compiler", "-o", test], stdout=subprocess.PIPE).stdout.decode('utf-8')

        compile_time = re.findall(r'Time to generate assembly: (\d+)ns', output)
        optimising_time = re.findall(r'Time to optimise assembly: (\d+)ns', output)
        time = re.findall(r'Time to write assembly to file: (\d+)ns', output)
        if len(compile_time) > 0:
            total_compile_time += int(compile_time[0])
        if len(optimising_time) > 0:
            total_optimising_time += int(optimising_time[0])
        if len(time) > 0:
            total_time += int(time[0])

    # find average times
    average_compile_time = total_compile_time / len(runningTests)
    average_optimising_time = total_optimising_time / len(runningTests)
    average_time = total_time / len(runningTests)
    # delete all files with .s extension
    subprocess.run(["rm", "-f", "*.s"])
    return (average_compile_time, average_optimising_time, average_time)


# run benchmark 50 times and find averages of averages
compile_times = []
optimising_times = []
times = []
for i in range(50):
    compile_time, optimising_time, time = benchmark_tests()
    compile_times.append(compile_time)
    optimising_times.append(optimising_time)
    times.append(time)

print("Average compile time: " + str(sum(compile_times) / len(compile_times)) + "ns")
print("Average optimising time: " + str(sum(optimising_times) / len(optimising_times)) + "ns")
print("Average time: " + str(sum(times) / len(times)) + "ns")

# benchmark change in number of lines of code by optimising

num_lines_diff = []
for test in runningTests:
    output = subprocess.run(["./wacc-compiler", test], stdout=subprocess.PIPE).stdout.decode('utf-8')
    file_name = test.split("/")[-1]
    num_lines = sum(1 for line in open(file_name[:-5] + ".s"))
    print(test + " has " + str(num_lines) + " lines of code")
    # recompile with optimisation
    output = subprocess.run(["./wacc-compiler", "-o", test], stdout=subprocess.PIPE).stdout.decode('utf-8')
    # find number of files in test.s
    num_lines_optimised = sum(1 for line in open(file_name[:-5] + ".s"))
    print(test + " has " + str(num_lines_optimised) + " lines of code after optimisation")
    print("Change in number of lines of code: " + str(num_lines - num_lines_optimised))
    num_lines_diff.append(num_lines - num_lines_optimised)

print("Average change in number of lines of code: " + str(sum(num_lines_diff) / len(num_lines_diff)))
print("Max change in number of lines of code: " + str(max(num_lines_diff)) + "in test " + runningTests[num_lines_diff.index(max(num_lines_diff))])


subprocess.run(["rm", "-f", "*.s"])
