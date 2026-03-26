#!/usr/bin/env python3

# Update the Port line for a given Host entry in ~/.ssh/config.
# Usage: set_port.py <host_alias> <port>

import sys, os, re

if len(sys.argv) != 3:
    print("Usage: set_port.py <host_alias> <port>")
    sys.exit(1)

alias = sys.argv[1]
port = sys.argv[2]

config_path = os.path.expanduser("~/.ssh/config")

with open(config_path, "r") as f:
    lines = f.readlines()

# Find the Host block and update its Port line
in_block = False
updated = False
for i, line in enumerate(lines):
    stripped = line.strip()
    # Detect start of a Host block
    if re.match(r'^Host\s+', stripped):
        in_block = (stripped.split()[1] == alias)
        continue
    if in_block and re.match(r'^Port\s+', stripped):
        indent = line[:len(line) - len(line.lstrip())]
        lines[i] = indent + "Port " + port + "\n"
        updated = True
        in_block = False

if not updated:
    print("For host %s, setting port to %s" % (alias, port))
    print("Warning: could not find Host %s with a Port line in %s" % (alias, config_path))
    sys.exit(1)

print("For host %s, setting port to %s" % (alias, port))
print("Writing: %s" % config_path)

with open(config_path, "w") as f:
    f.writelines(lines)
