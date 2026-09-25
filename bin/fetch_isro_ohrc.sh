#!/bin/bash
# Fetch ISRO Chandrayaan-2 OHRC data products via PRADAN.
#
# Usage:
#   fetch_isro_ohrc.sh <PRODUCT_ID_OR_ZIP_OR_LIST.txt> [outdir]
#
# Environment variables:
#   export PRADAN_JSESSIONID="<value from Chrome DevTools>"
#   export PRADAN_FGTSERVER="<value from Chrome DevTools>" (required for FortiGate load balancing)
#
# Example:
#   fetch_isro_ohrc.sh ch2_ohr_ncp_20260103T1005176450_d_img_d18.zip data/ohrc
#   fetch_isro_ohrc.sh my_candidates.txt data/ohrc

set -euo pipefail

TARGET="${1:-}"
OUTDIR="${2:-.}"

if [ -z "$TARGET" ]; then
  echo "Usage: $0 <PRODUCT_ID_OR_ZIP_OR_LIST.txt> [outdir]"
  echo "Requires PRADAN_JSESSIONID and PRADAN_FGTSERVER environment variables."
  exit 1
fi

if [ -z "${PRADAN_JSESSIONID:-}" ]; then
  echo "Error: PRADAN_JSESSIONID is not set."
  echo "Log into https://pradan.issdc.gov.in/ch2/ in Chrome, open DevTools (F12) -> Application -> Cookies,"
  echo "and export PRADAN_JSESSIONID=<value>."
  exit 1
fi

COOKIE_HEADER="Cookie: JSESSIONID=${PRADAN_JSESSIONID}"
if [ -n "${PRADAN_FGTSERVER:-}" ]; then
  COOKIE_HEADER="Cookie: JSESSIONID=${PRADAN_JSESSIONID}; FGTServer=${PRADAN_FGTSERVER}"
fi

mkdir -p "$OUTDIR"

# Background keepalive loop
KEEPALIVE_PID=""
start_keepalive() {
  ( while true; do
      sleep 600
      curl -k -s -o /dev/null -H "$COOKIE_HEADER" \
        "https://pradan.issdc.gov.in/ch2/protected/payload.xhtml" || true
    done ) &
  KEEPALIVE_PID=$!
}
stop_keepalive() {
  if [ -n "$KEEPALIVE_PID" ]; then
    kill "$KEEPALIVE_PID" 2>/dev/null || true
  fi
}
trap stop_keepalive EXIT INT TERM

fetch_single_product() {
  local item="$1"
  # Strip path and .zip if present
  local basename
  basename=$(basename "$item")
  local prod="${basename%.zip}"
  local zipname="${prod}.zip"
  local dest="${OUTDIR}/${zipname}"

  if [ -s "$dest" ]; then
    echo "Already downloaded: $dest"
    return 0
  fi

  # Determine level: ncp -> calibrated, nrp -> raw
  local level="calibrated"
  if [[ "$prod" == *"_nrp_"* ]]; then
    level="raw"
  fi

  # Extract 8-digit date YYYYMMDD
  # Format: ch2_ohr_ncp_YYYYMMDDTHHMMSS...
  local date_part
  date_part=$(echo "$prod" | sed -n 's/.*_[a-z0-9]*_\([0-9]\{8\}\)T.*/\1/p')
  if [ -z "$date_part" ]; then
    echo "Warning: Could not parse date from $prod, skipping."
    return 1
  fi

  local base_url="https://pradan.issdc.gov.in/ch2/protected/downloadData/POST_OD/isda_archive/ch2_bundle/cho_bundle/nop/ohr_collection/data"
  local url="${base_url}/${level}/${date_part}/${zipname}?ohrc"

  echo "Fetching ${zipname} (${level}, ${date_part}) ..."
  local tmp="${dest}.part"

  local code
  code=$(curl -k -sS --max-redirs 0 --max-time 3600 \
    -H "$COOKIE_HEADER" \
    -o "$tmp" -w '%{http_code}' "$url" 2>/dev/null || echo "000")

  if [ "$code" != "200" ] || [ ! -s "$tmp" ]; then
    rm -f "$tmp"
    echo "Error: HTTP $code for $zipname"
    echo "Check if your session expired or if FGTServer cookie is missing."
    return 1
  fi

  # Verify zip signature PK
  local sig
  sig=$(head -c 2 "$tmp" || true)
  if [ "$sig" != "PK" ]; then
    rm -f "$tmp"
    echo "Error: Server returned non-zip response for $zipname (possible login redirect)."
    return 1
  fi

  mv "$tmp" "$dest"
  local sz
  sz=$(ls -lh "$dest" | awk '{print $5}')
  echo "Saved: $dest ($sz)"

  # Rate limit politeness
  sleep 5
}

start_keepalive

if [ -f "$TARGET" ] && [[ "$TARGET" == *.txt ]]; then
  echo "Reading product list from $TARGET ..."
  count=0
  while IFS= read -r line || [ -n "$line" ]; do
    line=$(echo "$line" | tr -d '\r' | xargs)
    [ -z "$line" ] && continue
    [[ "$line" =~ ^# ]] && continue
    fetch_single_product "$line"
    count=$((count + 1))
  done < "$TARGET"
  echo "Done. Processed $count product(s)."
else
  fetch_single_product "$TARGET"
  echo "Done."
fi
