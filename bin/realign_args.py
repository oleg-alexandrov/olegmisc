#!/usr/bin/env python3
"""
Realign multi-line function arguments and continuation lines based on diff/git history.
If an opening parenthesis shifted column position between before and after versions,
shift its continuation lines by that exact same delta, preserving any existing relative alignment.
"""

import sys
import os
import subprocess
import difflib
import argparse

def mask_line(line):
  out = list(line)
  in_str = False
  str_char = ''
  i = 0
  n = len(line)
  while i < n:
    c = line[i]
    if not in_str:
      if i + 1 < n and line[i:i+2] == '//':
        for j in range(i, n):
          if out[j] != '\n':
            out[j] = ' '
        break
      elif c in ('"', "'"):
        in_str = True
        str_char = c
        out[i] = ' '
    else:
      if c == '\\' and i + 1 < n:
        out[i] = ' '
        out[i+1] = ' '
        i += 1
      elif c == str_char:
        in_str = False
        out[i] = ' '
      else:
        out[i] = ' '
    i += 1
  return ''.join(out)

def realign_diff(before_text, after_text):
  before_lines = before_text.splitlines()
  after_lines = after_text.splitlines()

  matcher = difflib.SequenceMatcher(None, before_lines, after_lines)

  # Build mapping from after_line_idx -> before_line_idx
  after_to_before = {}
  for tag, i1, i2, j1, j2 in matcher.get_opcodes():
    if tag == 'equal':
      for k in range(j2 - j1):
        after_to_before[j1 + k] = i1 + k
    elif tag == 'replace':
      len_b = i2 - i1
      len_a = j2 - j1
      for k in range(len_a):
        best_b = None
        best_ratio = -1.0
        cur_a = after_lines[j1 + k].strip()
        for m in range(len_b):
          cur_b = before_lines[i1 + m].strip()
          r = difflib.SequenceMatcher(None, cur_a, cur_b).ratio()
          if r > best_ratio:
            best_ratio = r
            best_b = i1 + m
        if best_ratio > 0.4:
          after_to_before[j1 + k] = best_b

  result_lines = list(after_lines)

  # Stack stores: (col_a, delta, line_idx, col_b)
  stack = []

  for j, line in enumerate(result_lines):
    # 1. If inside an unclosed paren from a previous line, check if this line should shift
    if stack and stack[-1][2] < j:
      col_a, delta, opener_j, col_b = stack[-1]
      if delta != 0:
        b_idx = after_to_before.get(j)
        if b_idx is not None and b_idx < len(before_lines):
          orig_line = before_lines[b_idx]
          lead_b = len(orig_line) - len(orig_line.lstrip(' '))
        else:
          lead_b = len(line) - len(line.lstrip(' '))

        # Shift only if lead_b was aligned with or after col_b in before
        if lead_b > 0 and (col_b is None or lead_b >= col_b - 1):
          new_indent = max(0, lead_b + delta)
          line = ' ' * new_indent + line.lstrip(' ')
          result_lines[j] = line

    masked = mask_line(line)

    # 2. Get paren columns in before line
    b_idx = after_to_before.get(j)
    paren_cols_b = []
    if b_idx is not None and b_idx < len(before_lines):
      masked_b = mask_line(before_lines[b_idx])
      paren_cols_b = [idx for idx, c in enumerate(masked_b) if c == '(']

    # Find open parens in this line
    paren_cols_a = [idx for idx, c in enumerate(masked) if c == '(']
    paren_meta = {}
    for k, ca in enumerate(paren_cols_a):
      if k < len(paren_cols_b):
        cb = paren_cols_b[k]
        paren_meta[ca] = (ca - cb, cb)
      else:
        paren_meta[ca] = (0, None)

    # 3. Scan characters in line from left to right, maintaining stack
    for idx, c in enumerate(masked):
      if c == '(':
        delta, cb = paren_meta.get(idx, (0, None))
        stack.append((idx, delta, j, cb))
      elif c == ')':
        if stack:
          stack.pop()

  out = '\n'.join(result_lines)
  if after_text.endswith('\n'):
    out += '\n'
  return out

def get_git_head_text(filepath):
  dir_name = os.path.dirname(os.path.abspath(filepath))
  try:
    cmd = ["git", "-C", dir_name, "rev-parse", "--show-toplevel"]
    repo_root = subprocess.check_output(cmd, text=True, stderr=subprocess.DEVNULL).strip()
    rel_path = os.path.relpath(filepath, repo_root)
    cmd = ["git", "-C", repo_root, "show", f"HEAD:{rel_path}"]
    return subprocess.check_output(cmd, text=True, stderr=subprocess.DEVNULL)
  except Exception:
    return None

def process_file(filepath):
  if not os.path.isfile(filepath):
    return False

  with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
    curr_text = f.read()

  before_text = get_git_head_text(filepath)
  if before_text is None:
    return False

  new_text = realign_diff(before_text, curr_text)
  if new_text != curr_text:
    with open(filepath, 'w', encoding='utf-8') as f:
      f.write(new_text)
    return True
  return False

def main():
  parser = argparse.ArgumentParser(description="Realign function arguments based on diff.")
  parser.add_argument("files", nargs="+", help="Files to realign based on git HEAD")
  args = parser.parse_args()

  changed = 0
  for f in args.files:
    if process_file(f):
      print(f"Realigned: {f}")
      changed += 1
  print(f"Total files realigned: {changed}")

if __name__ == "__main__":
  main()
