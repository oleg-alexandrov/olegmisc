---
name: google-workspace
description: >-
  Plan and operational runbook for Google Workspace REST API automation (Gmail drafts, Google Docs editing and viewing, Google Drive integration) using personal OAuth credentials and local Python tools. Load whenever resuming work on Google Workspace / Docs / Gmail draft automation or setting up credentials.json.
---

# Google Workspace Automation Runbook and Plan

This skill records the plan and setup instructions for enabling programmatic access to Gmail drafts, Google Docs, and Google Drive for the assistant.

## Motivation and Goal

- **Gmail drafts**: Create and modify draft emails directly in Gmail via the Gmail API, eliminating manual copy-pasting of drafted messages.
- **Google Docs**: Read and inspect document structure, append content, or make automated edits to Google Docs (*.gdoc*) via the Google Docs API.
- **Google Drive**: Complement existing *rclone* workflows with direct REST API operations for file management and sharing.

## Prerequisites and Cost

- **Cost**: Completely free. Google Cloud Platform personal projects and Google Workspace API quotas for personal use incur no charge.
- **Tools**: Python with *google-auth-oauthlib* and *google-api-python-client*.

## Implementation Plan

### Step 1: Google Cloud Console Configuration

Perform this one-time setup in the web browser at *console.cloud.google.com*:
1. Create a project named *Local Agent* or *Workspace Tools*.
2. Navigate to *APIs & Services > Library* and enable:
  - *Gmail API*
  - *Google Docs API*
  - *Google Drive API*
3. Configure the OAuth Consent Screen (*APIs & Services > OAuth consent screen*):
  - User Type: *External*.
  - Fill in application name and user support email.
  - In *Test users*, add the user's Google account email.
4. Create Credentials (*APIs & Services > Credentials > Create Credentials > OAuth client ID*):
  - Application type: *Desktop app*.
  - Name: *Local Desktop Client*.
  - Download the resulting JSON file and save it locally as *~/.config/google-workspace/credentials.json*.

### Step 2: Local Environment Setup

Install required Google client libraries:
```bash
pip install --upgrade google-api-python-client google-auth-httplib2 google-auth-oauthlib
```

### Step 3: Authorization Flow (Token Generation)

A Python script will run locally to prompt authorization:
- Scopes to request:
  - *https://www.googleapis.com/auth/gmail.compose* (allows draft creation and sending without full mailbox access)
  - *https://www.googleapis.com/auth/documents* (allows reading and writing Google Docs)
  - *https://www.googleapis.com/auth/drive.file* (access to files created or opened by the app)
- The script launches a local web server, opens the browser consent prompt, and saves *~/.config/google-workspace/token.json*.
- Like *rclone authorize*, this requires clicking through the unverified app warning once.

### Step 4: Python Helper Scripts and Tooling

Create helper scripts under *~/.claude/skills/google-workspace/scripts/* (or in a dedicated tools path):
- *gmail_draft.py*: Takes subject, recipient, and markdown or plain text body, creates an RFC 2822 MIME message, and calls the drafts create endpoint.
- *gdoc_tool.py*:
  - *read*: Extracts clean plain text or markdown from a Google Doc using its document ID.
  - *append*: Inserts text at the end of a document via *batchUpdate* (*InsertTextRequest*).
  - *replace*: Replaces text patterns within a document.

## Technical Caveats

- **Google Docs API structural complexity**: Unlike local text files, Google Docs are tree-structured objects. Editing arbitrary paragraphs requires calculating exact UTF-16 character indices. Appending to the end of a document is straightforward; inline editing requires caution with index offsets.
- **Gmail draft creation**: The Gmail API requires base64url-encoded RFC 2822 MIME messages. Proper handling of UTF-8 encoding in headers and body text is required.
- **Token security**: Never commit *credentials.json* or *token.json* to any git repository. Store them under *~/.config/google-workspace/* with permissions set to *0600*.
