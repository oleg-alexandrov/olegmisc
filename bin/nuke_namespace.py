#!/usr/bin/env python3
"""
Tool to nuke a namespace from C++ files.
- Removes 'namespace <ns> {' and its matching closing brace '}'.
- Removes 'using namespace vw::<ns>;' and 'using namespace <ns>;'.
- Optionally replaces 'vw::<ns>::' and '<ns>::' with 'vw::'.
- Realigns multi-line function arguments based on diff against original file.
"""

import sys
import os
import re
import argparse

# Import realign_diff from sibling realign_args
bin_dir = os.path.dirname(os.path.abspath(__file__))
if bin_dir not in sys.path:
  sys.path.insert(0, bin_dir)
from realign_args import realign_diff

def remove_namespace_blocks(text, ns_name):
  """
  Find each 'namespace <ns_name> {' and remove it along with its matching closing brace.
  """
  lines = text.split('\n')

  # Handle single-line namespace declarations, e.g.:
  # namespace ip { struct InterestPoint; } -> struct InterestPoint;
  for idx, l in enumerate(lines):
    if re.search(r'\bnamespace\s+' + re.escape(ns_name) + r'\s*\{[^}]*\}', l):
      m = re.search(r'^(\s*)namespace\s+' + re.escape(ns_name) + r'\s*\{\s*(.*?)\s*\}\s*$', l)
      if m:
        indent = m.group(1)
        inner = m.group(2)
        lines[idx] = (indent + inner) if inner else None
      else:
        lines[idx] = re.sub(r'\bnamespace\s+' + re.escape(ns_name) + r'\s*\{\s*(.*?)\s*\}', r'\1', l)
  lines = [l for l in lines if l is not None]

  # Normalize "namespace <ns>\n{" to "namespace <ns> {"
  norm_lines = []
  i = 0
  while i < len(lines):
    if (re.search(r'^\s*namespace\s+' + re.escape(ns_name) + r'\s*$', lines[i]) and
        i + 1 < len(lines) and lines[i+1].strip() == '{'):
      norm_lines.append(lines[i] + ' {')
      i += 2
    else:
      norm_lines.append(lines[i])
      i += 1
  lines = norm_lines

  while True:
    open_pos = None

    for l_idx, line in enumerate(lines):
      # Match various styles:
      # namespace cartography {
      m_standalone = re.search(r'^\s*namespace\s+' + re.escape(ns_name) + r'\s*\{', line)
      # namespace vw { namespace cartography {
      m_outer = re.search(r'^\s*namespace\s+(\w+)\s*\{\s*namespace\s+' + re.escape(ns_name) + r'\s*\{', line)
      # namespace cartography { namespace vw {
      m_inner = re.search(r'^\s*namespace\s+' + re.escape(ns_name) + r'\s*\{\s*namespace\s+(\w+)\s*\{', line)

      if m_outer:
        open_pos = (l_idx, m_outer.start(), 'outer', m_outer.group(1))
        break
      elif m_inner:
        open_pos = (l_idx, m_inner.start(), 'inner', m_inner.group(1))
        break
      elif m_standalone:
        open_pos = (l_idx, m_standalone.start(), 'standalone', None)
        break

    if not open_pos:
      break

    start_line, start_col, kind, other_ns = open_pos
    line = lines[start_line]

    if kind == 'outer':
      idx1 = line.find('{', start_col)
      open_brace_idx = line.find('{', idx1 + 1)
    else:
      open_brace_idx = line.find('{', start_col)

    depth = 1
    found_close = False
    close_line = None
    close_col = None
    in_multiline_comment = False

    for l_idx in range(start_line, len(lines)):
      cur_line = lines[l_idx]
      col_start = (open_brace_idx + 1) if l_idx == start_line else 0

      i = col_start
      in_str = False
      str_char = ''

      while i < len(cur_line):
        c = cur_line[i]
        if in_multiline_comment:
          if cur_line[i:i+2] == '*/':
            in_multiline_comment = False
            i += 2
            continue
          i += 1
          continue
        if not in_str:
          if cur_line[i:i+2] == '/*':
            in_multiline_comment = True
            i += 2
            continue
          elif cur_line[i:i+2] == '//':
            break
          elif c in ('"', "'"):
            in_str = True
            str_char = c
          elif c == '{':
            depth += 1
          elif c == '}':
            depth -= 1
            if depth == 0:
              found_close = True
              close_line = l_idx
              close_col = i
              break
        else:
          if c == '\\' and i + 1 < len(cur_line):
            i += 1
          elif c == str_char:
            in_str = False
        i += 1
      if found_close:
        break

    if not found_close:
      print(f"Warning: could not find matching brace for namespace {ns_name} at line {start_line+1}")
      break

    # Modify closing line
    cl = lines[close_line]
    if re.search(r'\}\s*\}\s*(//.*)?$', cl):
      # Two braces. Replace with one brace
      comment_match = re.search(r'//.*$', cl)
      comment = comment_match.group(0) if comment_match else ""
      if other_ns:
        new_comment = f" // namespace {other_ns}"
      elif "vw" in comment:
        new_comment = " // namespace vw"
      else:
        new_comment = ""
      lead = cl[:len(cl) - len(cl.lstrip(' '))]
      lines[close_line] = lead + "}" + new_comment
    elif re.search(r'^\s*\}\s*(//.*' + re.escape(ns_name) + r'.*)?$', cl):
      lines[close_line] = None
    elif re.search(r'^\s*\}\s*$', cl) and (close_line != start_line):
      lines[close_line] = None
    else:
      lines[close_line] = cl[:close_col] + cl[close_col+1:]

    # Modify starting line
    sl = lines[start_line]
    if kind == 'outer':
      lead = sl[:len(sl) - len(sl.lstrip(' '))]
      lines[start_line] = lead + f"namespace {other_ns} {{"
    elif kind == 'inner':
      lead = sl[:len(sl) - len(sl.lstrip(' '))]
      lines[start_line] = lead + f"namespace {other_ns} {{"
    elif kind == 'standalone':
      lines[start_line] = None

    lines = [l for l in lines if l is not None]

  # Remove using namespace lines
  cleaned_lines = []
  for l in lines:
    if re.search(r'^\s*using\s+namespace\s+(vw::)?' + re.escape(ns_name) + r'\s*;', l):
      continue
    cleaned_lines.append(l)

  return '\n'.join(cleaned_lines)

def replace_qualifiers(text, ns_name):
  # Replace vw::<ns>:: with vw::
  text = re.sub(r'\bvw::' + re.escape(ns_name) + r'::', 'vw::', text)
  # Replace standalone <ns>:: with vw::
  text = re.sub(r'(?<!::)\b' + re.escape(ns_name) + r'::', 'vw::', text)
  # Clean any accidental vw::vw::
  text = re.sub(r'\bvw::vw::', 'vw::', text)
  return text

def process_file(filepath, ns_name, do_replace_qualifiers, do_realign):
  with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
    orig_text = f.read()

  # Only process files that actually mention the namespace
  if ns_name not in orig_text:
    return False

  text = remove_namespace_blocks(orig_text, ns_name)

  if do_replace_qualifiers:
    text = replace_qualifiers(text, ns_name)

  if do_realign:
    text = realign_diff(orig_text, text)

  # Ensure single trailing newline
  text = text.rstrip('\n') + '\n'

  if text != orig_text:
    with open(filepath, 'w', encoding='utf-8') as f:
      f.write(text)
    return True
  return False

def main():
  parser = argparse.ArgumentParser(description="Nuke a namespace from C++ files.")
  parser.add_argument("--namespace", default="cartography", help="Namespace to remove")
  parser.add_argument("--replace-qualifiers", action="store_true",
                      help="Replace <ns>:: and vw::<ns>:: with vw::")
  parser.add_argument("--realign", action="store_true",
                      help="Realign multi-line function arguments based on diff")
  parser.add_argument("files", nargs="+", help="Files to process")
  args = parser.parse_args()

  changed = 0
  for filepath in args.files:
    if os.path.isfile(filepath):
      if process_file(filepath, args.namespace, args.replace_qualifiers, args.realign):
        print(f"Modified: {filepath}")
        changed += 1
  print(f"Total files modified: {changed}")

if __name__ == "__main__":
  main()
