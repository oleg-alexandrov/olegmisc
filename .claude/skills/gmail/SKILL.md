---
name: gmail
description: Reading Oleg's email through the claude_ai_Gmail MCP tools. Carries the hard lesson that search_threads returns a STALE, truncated snapshot of a thread (it relevance-matches the thread but serves an OLD message set, missing everything newer), so you must open the thread with get_thread and read it NEWEST-FIRST; plus the gmail-vs-NASA two-mailbox split, the "probe with a known recent word" debug trick, and how to recover a big thread that get_thread saved to a file. Load whenever searching for or reading Oleg's email via the Gmail MCP (search_threads / get_thread), or when a recent message "cannot be found".
---

# Reading Oleg's Gmail via the MCP

The `mcp__claude_ai_Gmail__*` tools read ONE mailbox: **oleg.alexandrov@gmail.com**.
Do not assume they see everything. The rules below were paid for in a long, painful
session (2026-08-31) where I wrongly concluded a just-delivered email did not exist.

## The #1 bug: search_threads serves a STALE thread snapshot (CRITICAL)

`search_threads` relevance-matches a thread but returns an **old cached copy of its
message list**. In the burn case a live thread of 21 messages (through Aug 31) came
back frozen at its **5-message Aug-8 state** for EVERY query variant. So the search
made it look like "nothing arrived after Aug 8" when in fact the newest message was
from last night. NEVER trust the message array that `search_threads` returns as
current or complete.

Consequences and the rule:
- The moment you have a threadId for a relevant thread, call **`get_thread(threadId)`**
  and sort its `messages[]` by `date` / `internalDate` **DESCENDING**. The NEWEST
  message is the operative one. Read newest-first; do not stop at what search shows.
- By definition we want the newest email in a thread. Sticking to the old
  search-provided messages is the exact failure to avoid.
- Date operators (`newer_than:`, `after:`) are ALSO unreliable here (a 23-day-old
  thread matched `newer_than:6h`). Do not rely on them to bound recency; sort by
  `internalDate` yourself.

## Two mailboxes: gmail vs NASA

Correspondents (e.g. Paul Schenk <schenk@lpi.usra.edu>) write to Oleg's NASA address
**oleg.alexandrov@nasa.gov**, which this MCP CANNOT see. Only messages Oleg forwards
or CCs to gmail appear. A thread titled `Fw: [EXTERNAL] ...` is such a forward. If the
thread exists in gmail at all, `get_thread` it fully (see the stale-snapshot bug) —
its cached search view may hide the very messages you need.

## Debug trick: probe with a word you KNOW is in a recent email

When "I can't find a recent email" happens, do NOT conclude it is missing from a few
keyword searches. Prove whether search can see today's mail at all: query a word you
KNOW appears in some other recent message (Oleg's cue words were `nas` and `rgsw`,
both in that morning's calendar mail). If those surface today's mail but your target
word does not, then either the target truly is not in gmail (it went to NASA), or it
is trapped in a stale-snapshot thread — in which case `get_thread` the thread and read
newest-first. Also probe a correspondent's known misspelling (Paul writes "isisi", not
"isis") — an exact-word hit is the tell that their new mail is or is not indexed.

## get_thread on a big thread is SAVED TO A FILE — use it

A large thread exceeds the tool's token cap; the result is written to a file and the
path is printed in the "error". That file holds the **FULL** thread: every message with
`plaintext_body`. This is often the ONLY way to read the recent messages when search is
stale. Recipe:

    jq -r '.messages[] | "\(.date)  \(.sender) :: \(.subject)"' <file>   # list, then eye the newest
    # then pull recent bodies (strip quoted history / signature blocks):
    python3 - <<'PY'
    import json,re,html
    d=json.load(open("<file>")); 
    for m in d["messages"][-6:]:
        b=html.unescape(m.get("plaintext_body") or m.get("snippet") or "")
        cut=[]; 
        for ln in b.splitlines():
            if re.match(r'^(On .*wrote:|From: |Sent: |-----Original|_{4,}|CAUTION:|Paul Schenk \()', ln.strip()): break
            if ln.strip().startswith('>'): continue
            cut.append(ln)
        print("="*70, m["date"], m["sender"], "\n", "\n".join(cut).strip()[:1500])
    PY

## Sending mail to Oleg

For SENDING (not reading), the msmtp path is in the machines-tools skill; this skill is
only about reading via the MCP.
