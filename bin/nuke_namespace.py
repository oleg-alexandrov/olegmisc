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

  def clean_ns_comment(s, ns):
    s = re.sub(r'::' + re.escape(ns) + r'\b', '', s)
    s = re.sub(r'\b' + re.escape(ns) + r'::', '', s)
    s = re.sub(r'\b' + re.escape(ns) + r'\b', '', s)
    # Clean up empty comments like "// namespace " or "// "
    s = re.sub(r'//\s*namespace\s*$', '', s)
    s = re.sub(r'//\s*$', '', s)
    return s.rstrip()

  while True:
    open_pos = None

    for l_idx, line in enumerate(lines):
      m = re.search(r'\bnamespace\s+' + re.escape(ns_name) + r'\s*\{', line)
      if m:
        open_pos = (l_idx, line.find('{', m.start()))
        break

    if not open_pos:
      break

    start_line, open_brace_idx = open_pos

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
    cl = lines[close_line][:close_col] + lines[close_line][close_col+1:]
    if re.sub(r'//.*$', '', cl).strip():
      lines[close_line] = clean_ns_comment(cl, ns_name)
    else:
      lines[close_line] = None

    # Modify starting line
    sl = re.sub(r'\s*namespace\s+' + re.escape(ns_name) + r'\s*\{', '', lines[start_line], count=1)
    if re.sub(r'//.*$', '', sl).strip():
      lines[start_line] = sl
    else:
      lines[start_line] = None

    lines = [l for l in lines if l is not None]

  # Remove using namespace lines and using <ns>::<Symbol> lines
  cleaned_lines = []
  for l in lines:
    if re.search(r'^\s*using\s+namespace\s+(vw::)?' + re.escape(ns_name) + r'\s*;', l):
      continue
    if re.search(r'^\s*using\s+(vw::)?' + re.escape(ns_name) + r'::[a-zA-Z0-9_]+\s*;', l):
      continue
    cleaned_lines.append(l)

  return '\n'.join(cleaned_lines)

def replace_qualifiers(text, ns_name):
  # Replace vw::<ns>:: with vw::
  text = re.sub(r'\bvw::' + re.escape(ns_name) + r'::', 'vw::', text)
  if ns_name == 'ba':
    # Standalone ba:: might be an alias for boost::algorithm or boost::accumulators.
    # Only replace known vw::ba symbols if standalone.
    ba_symbols = [
      'ControlNetwork', 'ControlPoint', 'ControlMeasure', 'add_ground_control_points',
      'build_control_network', 'triangulate_control_point', 'triangulate_control_network',
      'subsample_gcp_by_tri_ratio', 'subsample_control_network', 'JFeature', 'CameraRelationNetwork'
    ]
    for sym in ba_symbols:
      text = re.sub(r'(?<!::)\bba::' + sym + r'\b', f'vw::{sym}', text)
  else:
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
