import sys

CLEAR_INSTEAD = "-c" in sys.argv
if CLEAR_INSTEAD:
    sys.argv.remove("-c")

if len(sys.argv) < 2:
    print(f"{sys.argv[0]} [-c] <FILE.cob>")
    exit(2)

TARGET_FILE = sys.argv[1]

START = 0
STEP = 10

AREA_LEN = 6
current = START
out = ""

with open(TARGET_FILE) as f:
    original = f.read()

out = []
for line in original.split("\n"):
    line = (
        (" " * AREA_LEN if CLEAR_INSTEAD else str(current).zfill(AREA_LEN))
        + line[AREA_LEN:]
    )
    if line.isspace():
        line = ""
    out.append(line)
    current += STEP

print("\n".join(out), end="")
