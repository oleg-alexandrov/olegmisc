#!/usr/bin/env python3
"""
Tool to remove whitespace around parentheses and commas in C++ files:
- Removes whitespace after open parenthesis: '( ' -> '('
- Removes whitespace before close parenthesis: ' )' -> ')'
- Removes whitespace before comma: ' ,' -> ','
- Realigns multi-line function arguments when an open parenthesis shifted its first argument.
- Preserves strings, character literals, raw strings, and comments.
- Ensures each file ends with exactly one newline.
"""

import sys
import os
import re
import argparse

def process_file_content(content, do_open_paren, do_close_paren, do_comma, do_realign):
  lines = content.splitlines(keepends=True)
  
  in_block_comment = False
  raw_string_delim = None
  paren_stack = []
  out_lines = []

  for line_idx, raw_line in enumerate(lines):
    line = raw_line

    # 1. Realignment of continuation lines if requested
    orig_lead = len(line) - len(line.lstrip(' '))
    if do_realign and orig_lead > 0 and paren_stack and not in_block_comment and raw_string_delim is None:
      for entry in reversed(paren_stack):
        open_line, orig_open_col, orig_arg_col, new_open_col, new_arg_col, delta = entry
        if delta != 0 and open_line < line_idx:
          if orig_lead >= orig_arg_col:
            new_lead = max(0, orig_lead + delta)
            line = (' ' * new_lead) + line.lstrip(' ')
          break

    n = len(line)
    i = 0
    out = []
    in_string = False
    in_char = False

    while i < n:
      c = line[i]

      if in_block_comment:
        out.append(c)
        if c == '*' and i + 1 < n and line[i+1] == '/':
          out.append('/')
          in_block_comment = False
          i += 2
          continue
        i += 1
        continue

      if raw_string_delim is not None:
        end_token = ')' + raw_string_delim + '"'
        if line[i:i+len(end_token)] == end_token:
          out.extend(list(end_token))
          raw_string_delim = None
          i += len(end_token)
          continue
        out.append(c)
        i += 1
        continue

      if in_string:
        out.append(c)
        if c == '\\' and i + 1 < n:
          out.append(line[i+1])
          i += 2
          continue
        if c == '"':
          in_string = False
        i += 1
        continue

      if in_char:
        out.append(c)
        if c == '\\' and i + 1 < n:
          out.append(line[i+1])
          i += 2
          continue
        if c == "'":
          in_char = False
        i += 1
        continue

      # Outside strings and comments
      if c == '/' and i + 1 < n and line[i+1] == '/':
        out.extend(list(line[i:]))
        break

      if c == '/' and i + 1 < n and line[i+1] == '*':
        in_block_comment = True
        out.extend(['/', '*'])
        i += 2
        continue

      if c == 'R' and i + 1 < n and line[i+1] == '"':
        m = re.match(r'R"([a-zA-Z0-9_]*)\(', line[i:])
        if m:
          raw_string_delim = m.group(1)
          out.extend(list(m.group(0)))
          i += len(m.group(0))
          continue

      if c == '"':
        in_string = True
        out.append(c)
        i += 1
        continue

      if c == "'":
        in_char = True
        out.append(c)
        i += 1
        continue

      if c == '(':
        cur_col = len(out)
        out.append('(')
        i += 1
        ws_start = i
        while i < n and line[i] in ' \t':
          i += 1
        if do_open_paren and i > ws_start:
          if i == n or line[i] in '\r\n':
            # Trailing whitespace after (
            pass
          else:
            num_spaces = i - ws_start
            orig_open_col = cur_col
            orig_arg_col = cur_col + 1 + num_spaces
            new_open_col = cur_col
            new_arg_col = cur_col + 1
            delta = -num_spaces
            paren_stack.append((line_idx, orig_open_col, orig_arg_col, new_open_col, new_arg_col, delta))
        else:
          # Did not strip whitespace after (
          if not do_open_paren and i > ws_start:
            # Preserve the spaces if do_open_paren is False
            out.extend(list(line[ws_start:i]))
          orig_open_col = cur_col
          orig_arg_col = cur_col + 1
          new_open_col = cur_col
          new_arg_col = cur_col + 1
          delta = 0
          paren_stack.append((line_idx, orig_open_col, orig_arg_col, new_open_col, new_arg_col, delta))
        continue

      if c == ')':
        if paren_stack:
          paren_stack.pop()
        if do_close_paren:
          out_str = ''.join(out)
          if out_str.strip() != '':
            while out and out[-1] in ' \t':
              out.pop()
        out.append(')')
        i += 1
        continue

      if c == ',':
        if do_comma:
          out_str = ''.join(out)
          if out_str.strip() != '':
            while out and out[-1] in ' \t':
              out.pop()
        out.append(',')
        i += 1
        continue

      out.append(c)
      i += 1

    out_lines.append(''.join(out))

  res = ''.join(out_lines)
  res = res.rstrip('\r\n') + '\n'
  return res

def process_file(filepath, do_open_paren, do_close_paren, do_comma, do_realign):
  try:
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
      orig_text = f.read()
  except Exception as e:
    print(f"Error reading {filepath}: {e}", file=sys.stderr)
    return False

  new_text = process_file_content(orig_text, do_open_paren, do_close_paren, do_comma, do_realign)

  if new_text != orig_text:
    with open(filepath, 'w', encoding='utf-8') as f:
      f.write(new_text)
    return True
  return False

def main():
  parser = argparse.ArgumentParser(description="Clean up whitespace around parens and commas in C++ code.")
  parser.add_argument("--strip-open-paren", action="store_true", help="Strip space after ( and realign")
  parser.add_argument("--strip-close-paren", action="store_true", help="Strip space before )")
  parser.add_argument("--strip-comma", action="store_true", help="Strip space before ,")
  parser.add_argument("--no-realign", action="store_true", help="Disable argument realignment")
  parser.add_argument("files", nargs="+", help="Files to process")
  args = parser.parse_args()

  # If none specified, do all
  if not args.strip_open_paren and not args.strip_close_paren and not args.strip_comma:
    do_open = True
    do_close = True
    do_comma = True
  else:
    do_open = args.strip_open_paren
    do_close = args.strip_close_paren
    do_comma = args.strip_comma

  do_realign = not args.no_realign

  modified_count = 0
  for f in args.files:
    if os.path.isfile(f):
      if process_file(f, do_open, do_close, do_comma, do_realign):
        print(f"Modified: {f}")
        modified_count += 1

  print(f"Total files modified: {modified_count}")

if __name__ == '__main__':
  main()
