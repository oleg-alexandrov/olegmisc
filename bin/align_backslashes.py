#!/usr/bin/env python3

# Align trailing backslash continuation characters to a consistent column
# within a range of lines. Finds the longest content line in the range and
# places all backslashes one space past that, or at the given column. Lines
# ending in "+ \\" (Python string concatenation) are aligned on the "+", so
# this works for both shell continuations and Python help-string blocks.
#
# Usage:
#   align_backslashes.py <file> <start_line> <end_line>              # print
#   align_backslashes.py <file> <start_line> <end_line> --inplace    # edit
#   align_backslashes.py <file> <start_line> <end_line> --column 50  # fixed col

import sys
import re

def usage():
    print("Usage: align_backslashes.py <file> <start> <end> "
          "[--inplace] [--column N]")
    print("Lines are 1-based. Aligns trailing backslashes.")
    sys.exit(1)

def main():
    if len(sys.argv) < 4:
        usage()

    filepath = sys.argv[1]
    start = int(sys.argv[2])
    end = int(sys.argv[3])
    inplace = "--inplace" in sys.argv
    col = None
    if "--column" in sys.argv:
        idx = sys.argv.index("--column")
        if idx + 1 >= len(sys.argv):
            usage()
        col = int(sys.argv[idx + 1])

    with open(filepath, "r") as f:
        lines = f.readlines()

    if start < 1 or end > len(lines) or start > end:
        print(f"Error: invalid range {start}-{end} for file with "
              f"{len(lines)} lines", file=sys.stderr)
        sys.exit(1)

    # Extract the range (0-based internally)
    s = start - 1
    e = end

    # Strip trailing backslash and measure content width. A line ending in
    # "+ \\" (Python string concatenation) is handled as a unit: the "+" is
    # aligned to the column and the backslash follows one space later. A bare
    # trailing "\\" (shell) is aligned directly.
    stripped = []
    for i in range(s, e):
        line = lines[i].rstrip("\n")
        if line.rstrip().endswith("\\"):
            content = line.rstrip()[:-1].rstrip()
            has_plus = content.endswith("+")
            if has_plus:
                content = content[:-1].rstrip()
            stripped.append((i, content, True, has_plus))
        else:
            stripped.append((i, line.rstrip(), False, False))

    # Find target column: longest content + 1 space, or user-specified
    if col is None:
        max_len = 0
        for _, content, has_bs, _ in stripped:
            if has_bs and len(content) > max_len:
                max_len = len(content)
        col = max_len + 1

    # Rebuild lines
    for idx, content, has_bs, has_plus in stripped:
        if has_bs:
            padding = max(1, col - len(content))
            tail = "+ \\\n" if has_plus else "\\\n"
            lines[idx] = content + " " * padding + tail

    if inplace:
        with open(filepath, "w") as f:
            f.writelines(lines)
    else:
        for i in range(s, e):
            sys.stdout.write(lines[i])

if __name__ == "__main__":
    main()
