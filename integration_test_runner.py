import glob
import re
import subprocess
import os
import sys

def get_return_code(fname):
  if "valid" in fname:
    return 0
  with open(fname) as f:
    lines = f.readlines()
    for i in range(len(lines)):
      if lines[i].startswith("# Exit:"):
        return int(re.search("[0-9]+", lines[i+1]).group())
  return 0

def list_directories_in_directory(base):
  result = [name for name in os.listdir(base) if (os.path.isdir(os.path.join(base, name)) and name != 'whack')]
  return result

def list_files_in_directory(base):
    file_paths = []
    for root, dirs, files in os.walk(base):
        dirs[:] = [d for d in dirs if d.lower() != 'whack']
        for file in files:
            file_paths.append(os.path.join(root, file))
    return file_paths

base = "wacc_examples/"
tests = dict()

for directory in list_directories_in_directory(base):
  # Added valid and invalid directories to the tests dictionary
  tests[directory] = list_files_in_directory(base + directory + "/")

  # Added subdirectoried of valid and invalid directories
  for subdirectory in list_directories_in_directory(base + directory + "/"):
    subdirectoryName = subdirectory
    if subdirectory.endswith("Err"):
      subdirectoryName = subdirectory[:-3]
    tests[directory + "-" + subdirectoryName] = list_files_in_directory(base + directory + "/" + subdirectory + "/")

    # Added subdirectories of
    if directory == "invalid":
      for subsubdirectory in list_directories_in_directory(base + directory + "/" + subdirectory + "/"):
        tests[directory + "-" + subdirectory[:-3] + "-" + subsubdirectory] = \
          list_files_in_directory(base + directory + "/" + subdirectory + "/" + subsubdirectory + "/")

passes = 0
total = 0
runningTests = []

print("Running tests...")

if len(sys.argv) < 2:
  runningTests = tests["valid"] + tests["invalid"]
else :
  tag = '-'.join(sys.argv[1:])
  if tag in tests:
    runningTests = tests[tag]
  else:
    print(f"Test tag {tag} not found")

for test in runningTests:
  for fname in glob.glob(test):
    print(f"sh compile {fname}")
    proc = subprocess.run(["sh", "compile", fname], stdout=subprocess.DEVNULL)

    actual = proc.returncode
    expected = get_return_code(fname)
    total += 1
    if actual == expected:
      passes += 1
    else:
      print(f"Failed test {fname}. Expected exit code {expected} but got {actual}")

print(f"Finished running tests. {passes} / {total} tests passed.")
