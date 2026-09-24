#!/usr/bin/env python3

# Update Hostname, Port, and ProxyJump in the Host pfx block of ~/.ssh/config.
# Used by tunnel.sh to switch between two routes:
#   tunnel  - normal: localhost:<port> via the Mac->pfx ssh tunnel.
#             No ProxyJump (direct loopback to forwarded port).
#   athfe   - fallback when pfe is down: athfe01.nas.nasa.gov:22 via sfe.
#             ProxyJump sfe is required so the connection goes through the
#             NAS sfe gateway. pfe and athfe share the same NAS filesystem,
#             so all rsync/ssh paths used in scripts still work.
#
# Usage: set_pfx_target.py tunnel <port>
#        set_pfx_target.py athfe

import sys, os, re

def usage():
    print("Usage: set_pfx_target.py tunnel <port>")
    print("       set_pfx_target.py athfe")
    sys.exit(1)

if len(sys.argv) < 2:
    usage()

mode = sys.argv[1]
if mode == "tunnel":
    if len(sys.argv) != 3:
        usage()
    new_host = "localhost"
    new_port = sys.argv[2]
    want_proxyjump = False
elif mode == "athfe":
    new_host = "athfe01.nas.nasa.gov"
    new_port = "22"
    want_proxyjump = True
else:
    usage()

config_path = os.path.expanduser("~/.ssh/config")
with open(config_path) as f:
    lines = f.readlines()

# Find the Host pfx block: from its `Host pfx` line up to (but not including)
# the next `Host ` line or end of file.
start = None
end = None
for i, line in enumerate(lines):
    stripped = line.strip()
    m = re.match(r'^Host\s+(\S+)', stripped)
    if m:
        if start is None and m.group(1) == "pfx":
            start = i
        elif start is not None:
            end = i
            break
if start is None:
    print("Error: could not find 'Host pfx' block in %s" % config_path)
    sys.exit(1)
if end is None:
    end = len(lines)

# Determine indent from existing body lines (first non-Host, non-blank).
indent = "  "
for line in lines[start + 1:end]:
    if line.strip() and not line.lstrip().startswith("#"):
        indent = line[:len(line) - len(line.lstrip())]
        break

block = lines[start:end]
host_done = False
port_done = False
proxyjump_idx = None
for j, line in enumerate(block):
    if j == 0:
        continue  # the `Host pfx` line itself
    stripped = line.strip()
    if re.match(r'^Hostname\s+', stripped, re.IGNORECASE):
        block[j] = indent + "Hostname " + new_host + "\n"
        host_done = True
    elif re.match(r'^Port\s+', stripped, re.IGNORECASE):
        block[j] = indent + "Port " + new_port + "\n"
        port_done = True
    elif re.match(r'^ProxyJump\s+', stripped, re.IGNORECASE):
        proxyjump_idx = j

if not (host_done and port_done):
    print("Error: Host pfx block missing Hostname/Port lines (host=%s, port=%s)"
          % (host_done, port_done))
    sys.exit(1)

# Add or remove ProxyJump sfe.
if want_proxyjump:
    if proxyjump_idx is not None:
        block[proxyjump_idx] = indent + "ProxyJump sfe\n"
    else:
        # Insert right after Hostname line (keeps block tidy).
        for j, line in enumerate(block):
            if re.match(r'^Hostname\s+', line.strip(), re.IGNORECASE):
                block.insert(j + 1, indent + "ProxyJump sfe\n")
                break
else:
    if proxyjump_idx is not None:
        block.pop(proxyjump_idx)

lines = lines[:start] + block + lines[end:]
with open(config_path, "w") as f:
    f.writelines(lines)

proxy_str = " (ProxyJump sfe)" if want_proxyjump else ""
print("pfx -> %s:%s%s (mode: %s)" % (new_host, new_port, proxy_str, mode))
