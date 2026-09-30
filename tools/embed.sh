#!/bin/sh
# embed.sh -- a source file as a C string table, on stdout: one
# quoted literal per line, a 0 sentinel closing. The prelude's source
# ships inside the binary (std::meta parses like any other source,
# 08-reflection.md); this is how it gets there.
#
# The input must be ASCII with no other rule: a backslash and a quote
# are escaped, and each line contributes its own "\n". An empty line
# is "", not nothing -- the newline still counts. One literal per
# line keeps each within C90's 509-char minimum, which a single
# joined literal would exceed (-Woverlength-strings); the reader
# (prelude.c) joins them.

set -e

[ $# -eq 1 ] || { echo "usage: embed.sh file" >&2; exit 1; }
[ -f "$1" ] || { echo "embed.sh: $1: not a file" >&2; exit 1; }

echo "/* generated from $1 by tools/embed.sh -- do not edit */"
echo "static const char *const prelude_src[] = {"

sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/  "/' -e 's/$/\\n",/' "$1"

echo "  0,"
echo "};"
