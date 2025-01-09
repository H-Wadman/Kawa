import sys
import os
import subprocess

path = os.path.abspath(sys.argv[0])
curr_dir = os.path.dirname(path)
kawai = os.path.join(os.path.dirname(curr_dir), "kawai.exe")


succ = os.path.join(curr_dir, "success/")
fail = os.path.join(curr_dir, "failure/")

# Checks that all files in success succeed and that all files in failure fail appropriately (without anomaly)

succs = os.listdir(succ)


n_succ = 0
for f in succs:
    res = subprocess.run([kawai, os.path.join(succ, f)], capture_output=True)
    if res.returncode != 0:
        print(f"{f} was supposed to succeed but failed")
    else:
        n_succ += 1

print(f"{n_succ}/{len(succs)} succeeded properly")

fails = os.listdir(fail)
n_fail = 0

for f in fails:
    res = subprocess.run([kawai, os.path.join(fail, f)], capture_output=True)
    if res.returncode == 0:
        print(f"{f} was supposed to fail but succeeded")
    else:
        n_fail += 1
print(f"{n_fail}/{len(fails)} succeeded properly")
