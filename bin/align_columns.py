#!/usr/bin/env python3

# Align columns in a range of lines in a file. Columns are detected
# by 2+ consecutive spaces in the line that has the most such splits
# (typically a header). Column positions from that line are used to
# split all other lines, handling the case where a long entry has
# only 1 space before the next column.
#
# Usage:
#   align_columns.py <file> <start_line> <end_line>          # print
#   align_columns.py <file> <start_line> <end_line> --inplace # edit

import sys
import re

def usage():
    print("Usage: align_columns.py <file> <start> <end> [--inplace]")
    print("Lines are 1-based. Aligns columns separated by 2+ spaces.")
    sys.exit(1)

def find_column_starts(line):
    """Find column start positions by looking for 2+ space gaps.
    Returns list of character positions where columns begin."""
    starts = [0]
    for m in re.finditer(r'  +(?=\S)', line):
        starts.append(m.end())
    return starts

def split_at_positions(line, positions):
    """Split a line at given character positions, trimming whitespace."""
    parts = []
    for i, pos in enumerate(positions):
        if i + 1 < len(positions):
            parts.append(line[pos:positions[i+1]].rstrip())
        else:
            parts.append(line[pos:].rstrip())
    return parts

def align_lines(lines):
    """Align columns across a list of lines."""
    stripped = [line.rstrip() for line in lines]

    # Find column positions from the line with the most 2+ space gaps
    best_starts = [0]
    for line in stripped:
        starts = find_column_starts(line)
        if len(starts) > len(best_starts):
            best_starts = starts

    if len(best_starts) <= 1:
        return stripped

    # Split all lines using those column positions
    rows = []
    for line in stripped:
        if len(line) == 0:
            rows.append([line])
            continue
        rows.append(split_at_positions(line, best_starts))

    num_cols = len(best_starts)

    # Find max width for each column (except last)
    col_widths = []
    for c in range(num_cols - 1):
        max_w = 0
        for row in rows:
            if c < len(row):
                max_w = max(max_w, len(row[c]))
        col_widths.append(max_w)

    # Rebuild each line with aligned columns
    result = []
    for row in rows:
        parts = []
        for c, cell in enumerate(row):
            if c < len(row) - 1 and c < len(col_widths):
                parts.append(cell.ljust(col_widths[c]))
            else:
                parts.append(cell)
        result.append("  ".join(parts))

    return result

def main():
    if len(sys.argv) < 4:
        usage()

    filepath = sys.argv[1]
    start = int(sys.argv[2]) - 1  # convert to 0-based
    end = int(sys.argv[3])        # end is inclusive, so slice is [start:end]
    inplace = "--inplace" in sys.argv

    with open(filepath, "r") as f:
        all_lines = f.readlines()

    if start < 0 or end > len(all_lines) or start >= end:
        print(f"Error: invalid range {start+1}-{end} "
              f"(file has {len(all_lines)} lines)")
        sys.exit(1)

    # Extract, align, and rejoin
    target = all_lines[start:end]
    aligned = align_lines(target)

    if inplace:
        for i, line in enumerate(aligned):
            all_lines[start + i] = line + "\n"
        with open(filepath, "w") as f:
            f.writelines(all_lines)
    else:
        for line in aligned:
            print(line)

if __name__ == "__main__":
    main()
