---
name: google-drive
description: Fetch data from Google Drive to pfe/Lou or the Mac - rclone setup with headless OAuth (authorize on a browser machine, transplant the token to the headless host), shared-folder access by folder-id, and when to use the Drive MCP vs rclone. Load whenever downloading/reading Google Drive data (a shared folder of imagery, zips, rasters), setting up an rclone Drive remote, or deciding MCP-vs-rclone for Drive.
---

## Google Drive: MCP vs rclone - pick the right tool

- **Drive MCP tools** (`mcp__claude_ai_Google_Drive__*`: search_files, list_recent_files,
  get_file_metadata, read_file_content, download_file_content) act as the user's
  claude.ai-connected Google account. They pull file CONTENT into the conversation -
  great for LISTING a folder and READING small text files (metadata, CSV, docs). They
  are USELESS for bulk binary (multi-GB zips/imagery): there is no stream-to-disk path.
  Use the MCP only to SCOPE (list names/sizes/ids) and read small text.
- **rclone** is the tool for actually DOWNLOADING data (large or many files) to pfe/Lou
  or the Mac. Private/shared folders need OAuth; public "anyone with link" files can use
  gdown, but a restricted (USGS/org) shared folder REQUIRES rclone OAuth - gdown fails.

## Where to run the download
- **On pfe/Lou, run rclone on a HEAD/front-end node** - those have internet; compute
  nodes do not (see the `pfe-nas` skill). A download is network-bound, not compute, so
  it does NOT trip the head-node >1-thread gate. (2026-08-26: 4.9 GB pulled at ~294
  MiB/s on the pfe head node.) Put the data on `/nobackup...`, not the home dir.

## rclone headless OAuth (pfe has no browser) - the working recipe
The OAuth browser step MUST happen on a machine WITH a browser (the Mac); then the
token is transplanted to the headless host (pfe). rclone is a single static binary
(no root): `curl -sL https://downloads.rclone.org/rclone-current-linux-amd64.zip -o
/tmp/r.zip && cd /tmp && unzip -oq r.zip && cp rclone-*-linux-amd64/rclone ~/bin/`.
1. On the Mac (has rclone + browser): `rclone authorize "drive"` - a browser opens,
   log in with the account that can see the shared folder, authorize (click through
   the "app not verified" advanced warning), and rclone prints a token JSON blob.
   (Running it backgrounded via the Bash tool works: the browser still opens; the
   token lands in the task output. It is a SECRET - do not echo it around.)
2. Build the remote on the target host. Handle the token as a secret and avoid
   shell-quoting it (it has `{}"` chars): read it programmatically and either
   `rclone config create gdrive drive scope drive.readonly token <TOK>` (via a python
   subprocess arg-list, NOT a shell string), or write `~/.config/rclone/rclone.conf`
   directly and `chmod 600` it:
       [gdrive]
       type = drive
       scope = drive.readonly
       token = {"access_token":...,"refresh_token":...,...}
       root_folder_id = <FOLDER_ID>      # optional; see below
   `rclone config create <name>` APPENDS/updates without clobbering other remotes;
   writing the file wholesale would overwrite existing remotes - check first.
- Same token works on Mac and pfe (transplant the same blob to both).

## Accessing a SHARED folder (shared-with-me, not "My Drive")
- A Drive link `drive.google.com/drive/.../folders/<ID>` -> use `<ID>`.
- Either bake it into the remote: `root_folder_id = <ID>` (then `gdrive:` == that
  folder), or pass per-command: `rclone lsf gdrive: --drive-root-folder-id <ID>`.
- For items under "Shared with me": `--drive-shared-with-me`. For a Shared Drive:
  `--drive-team-drive <id>`.
- List/size first: `rclone lsf gdrive:`, `rclone ls gdrive:` (files+bytes),
  `rclone size gdrive:`. Then copy: `rclone copy gdrive: /nobackup/.../dest/
  --transfers 4 --multi-thread-streams 4 --stats 15s --log-file <log>`.

## Uploading / delivering a doc TO Drive (write path)
- The `gdrive:` remote is `scope = drive.readonly`, so any WRITE fails with
  `ACCESS_TOKEN_SCOPE_INSUFFICIENT` / "Insufficient Permission". To push, the remote
  must be re-authorized with a writable scope (`drive.file`), a browser step. Leave the
  read-only default unless a write is actually needed.
- The Drive MCP `create_file` CAN write (native Docs/folders, or upload+convert content),
  and the MCP acts as the user's own account, so it reaches their shared folders. But a
  binary upload means passing the file as a base64 param, which for a multi-MB file is
  impractical to hand-author into the tool call. Use the MCP for small text or to create
  an empty native Doc, not for a 2 MB binary.
- **To deliver a native Google Doc WITH figures**: do NOT convert a figures-heavy HTML to
  a Doc. Google's HTML-to-Docs import DROPS base64 data-URI images, so the Doc arrives
  with text but no figures. Instead build a `.docx` with the images EMBEDDED
  (python-docx: `add_picture`, side-by-side via a 1x2 table), verify with
  `unzip -l x.docx | grep word/media`, then either (a) have the user drag the docx into
  the Drive folder (Drive converts it to a Doc, embedded images survive), or (b) push it
  via rclone with a writable scope and `--drive-import-formats docx` to convert on upload.
  Confirm folder write access first: MCP `get_file_metadata <folderId>` returns
  `canAddChildren` and `readOnly`.

## Caveats
- rclone's DEFAULT shared client_id is being retired during 2026 (a NOTICE prints, but
  it still works for now). For durable use, make a personal client_id (rclone drive
  docs) and add `client_id`/`client_secret` to the remote. Not required for a one-off.
- `~/.config/rclone/rclone.conf` holds a live OAuth refresh token - keep it 0600, and
  NEVER git-add it (it can sit under a tracked home dir).
- `drive.readonly` scope is enough for downloads and is the safe default. Note: the
  token minted by `rclone authorize "drive"` carries the full `drive` scope; setting
  `scope = drive.readonly` in the config still works (rclone uses the token as-is).
