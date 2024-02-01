import glob
import re
import subprocess

tests = ["valid/basic/skip/comment.wacc",
         "invalid/syntaxErr/basic/badComment.wacc",
         "invalid/syntaxErr/if/*"]

base = "wacc_examples/integration/"

def get_return_code(fname):
  with open(fname) as f:
    lines = f.readlines()
    for i in range(len(lines)):
      if lines[i].startswith("# Exit:"):
        return int(re.search("[0-9]+", lines[i+1]).group())
  return 0

passes = 0
total = len(tests)

print("Running tests...")
for test in tests:
  for fname in glob.glob(base + test):
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