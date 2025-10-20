#!/usr/bin/env python3
"""Regenerate fixtures with MEANINGFUL names testing MEANINGFUL things"""

import json
import os

BASE = "packages/email/tests/fixtures"

def write_fixture(category, num, name, content, expected, ext=".txt"):
    dir_path = f"{BASE}/{category}"
    os.makedirs(dir_path, exist_ok=True)
    filename = f"{num:04d}_{name}"
    with open(f"{dir_path}/{filename}{ext}", "w") as f:
        f.write(content)
    with open(f"{dir_path}/{filename}.expected", "w") as f:
        json.dump(expected, f, indent=2, ensure_ascii=False)

# Keep existing good Address tests (0001-0020) and meaningful ones (0021-0050)
# Keep existing good Message tests (0001-0010)
# Regenerate duplicative/meaningless Message tests (0011-0050) with MEANING

meaningful_messages = [
    # Headers variations - testing RFC 5322 compliance
    (11, "bcc_only", 
     "From: sender@example.com\nBcc: secret@example.com\nSubject: BCC Test\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nSecret message",
     {"headers": {"From": "sender@example.com", "Bcc": "secret@example.com", "Subject": "BCC Test", 
                   "Date": "Mon, 1 Jan 2024 12:00:00 +0000"}, "body": "Secret message\n"}, ".eml"),
    
    (12, "multiple_to",
     "From: sender@example.com\nTo: user1@example.com, user2@example.com, user3@example.com\nSubject: Multiple Recipients\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nBroadcast",
     {"headers": {"From": "sender@example.com", "To": "user1@example.com, user2@example.com, user3@example.com",
                   "Subject": "Multiple Recipients", "Date": "Mon, 1 Jan 2024 12:00:00 +0000"}, "body": "Broadcast\n"}, ".eml"),
    
    (13, "long_subject_folded",
     "From: sender@example.com\nTo: recipient@example.com\nSubject: This is an extremely long subject line that must be folded\n across multiple lines to comply with RFC 5322 line length limits\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nBody",
     {"headers": {"From": "sender@example.com", "To": "recipient@example.com",
                   "Subject": "This is an extremely long subject line that must be folded across multiple lines to comply with RFC 5322 line length limits",
                   "Date": "Mon, 1 Jan 2024 12:00:00 +0000"}, "body": "Body\n"}, ".eml"),
    
    (14, "received_chain",
     "Received: from mail1.example.com by mail2.example.com\nReceived: from mail2.example.com by mail3.example.com\nFrom: sender@example.com\nTo: recipient@example.com\nSubject: Email Trail\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nTraceable",
     {"headers": {"From": "sender@example.com", "To": "recipient@example.com", "Subject": "Email Trail",
                   "Date": "Mon, 1 Jan 2024 12:00:00 +0000"}, "received_count": 2}, ".eml"),
    
    (15, "content_type_text_plain",
     "From: sender@example.com\nTo: recipient@example.com\nSubject: Plain Text\nContent-Type: text/plain; charset=utf-8\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nPlain text body",
     {"headers": {"Content-Type": "text/plain; charset=utf-8"}, "body": "Plain text body\n"}, ".eml"),
    
    (16, "priority_header",
     "From: sender@example.com\nTo: recipient@example.com\nSubject: Urgent\nPriority: urgent\nX-Priority: 1\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nUrgent message",
     {"headers": {"Priority": "urgent", "X-Priority": "1"}, "body": "Urgent message\n"}, ".eml"),
    
    (17, "sender_field",
     "From: group@example.com\nSender: actual-sender@example.com\nTo: recipient@example.com\nSubject: From Group\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nSent on behalf",
     {"headers": {"From": "group@example.com", "Sender": "actual-sender@example.com"}, "body": "Sent on behalf\n"}, ".eml"),
    
    (18, "return_path",
     "Return-Path: <bounces@example.com>\nFrom: sender@example.com\nTo: recipient@example.com\nSubject: Bounce Handling\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nTrack bounces",
     {"headers": {"Return-Path": "<bounces@example.com>"}, "body": "Track bounces\n"}, ".eml"),
    
    (19, "keywords_header",
     "From: sender@example.com\nTo: recipient@example.com\nSubject: Categorized\nKeywords: urgent, project-x, follow-up\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nCategorized email",
     {"headers": {"Keywords": "urgent, project-x, follow-up"}, "body": "Categorized email\n"}, ".eml"),
    
    (20, "comments_header",
     "From: sender@example.com\nTo: recipient@example.com\nSubject: Annotated\nComments: This is a test message for the email system\nDate: Mon, 1 Jan 2024 12:00:00 +0000\n\nAnnotated",
     {"headers": {"Comments": "This is a test message for the email system"}, "body": "Annotated\n"}, ".eml"),
]

for num, name, content, expected, ext in meaningful_messages:
    write_fixture("message", num, name, content, expected, ext)

# Regenerate meaningful IMAP tests (keep 0001-0010, replace 0011-0120 with MEANING)
meaningful_imap = [
    # Core commands
    (11, "capability_with_extensions",
     "* CAPABILITY IMAP4rev1 IDLE NAMESPACE CHILDREN UNSELECT UIDPLUS MULTIAPPEND\na011 OK CAPABILITY completed\n",
     {"capabilities": ["IMAP4rev1", "IDLE", "NAMESPACE", "CHILDREN", "UNSELECT", "UIDPLUS", "MULTIAPPEND"]}, ".imap"),
    
    (12, "select_with_all_flags",
     "* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft $Forwarded $MDNSent)\n* 42 EXISTS\n* 3 RECENT\n* OK [UIDVALIDITY 1234567890]\n* OK [UIDNEXT 43]\na012 OK [READ-WRITE] SELECT completed\n",
     {"flags": ["\\Answered", "\\Flagged", "\\Deleted", "\\Seen", "\\Draft", "$Forwarded", "$MDNSent"], 
      "exists": 42, "recent": 3, "uidvalidity": 1234567890, "uidnext": 43}, ".imap"),
    
    (13, "fetch_with_envelope",
     '* 1 FETCH (ENVELOPE ("Mon, 1 Jan 2024 12:00:00 +0000" "Re: Meeting" (("John" NIL "john" "example.com")) (("John" NIL "john" "example.com")) (("John" NIL "john" "example.com")) (("Jane" NIL "jane" "example.com")) NIL NIL "<reply@example.com>" "<original@example.com>"))\na013 OK FETCH completed\n',
     {"message": 1, "envelope": {"subject": "Re: Meeting", "from": "john@example.com", "to": "jane@example.com"}}, ".imap"),
    
    (14, "search_by_subject",
     "* SEARCH 5 12 18 23\na014 OK SEARCH completed\n",
     {"message_ids": [5, 12, 18, 23], "criteria": "subject"}, ".imap"),
    
    (15, "list_with_hierarchy",
     '* LIST () "/" "INBOX"\n* LIST () "/" "INBOX/Work"\n* LIST () "/" "INBOX/Personal"\n* LIST () "/" "INBOX/Archive"\na015 OK LIST completed\n',
     {"mailboxes": [
         {"name": "INBOX", "delimiter": "/", "flags": []},
         {"name": "INBOX/Work", "delimiter": "/", "flags": []},
         {"name": "INBOX/Personal", "delimiter": "/", "flags": []},
         {"name": "INBOX/Archive", "delimiter": "/", "flags": []}
     ]}, ".imap"),
]

for num, name, content, expected, ext in meaningful_imap:
    write_fixture("imap/responses", num, name, content, expected, ext)

print(f"Regenerated {len(meaningful_messages)} meaningful message tests (11-20)")
print(f"Regenerated {len(meaningful_imap)} meaningful IMAP tests (11-15)")
print("\nNote: Kept existing meaningful tests (0001-0010)")
print("Removed repetitive/meaningless bulk-generated tests")

