#!/bin/sh
# embed.sh -- the std sources as C string tables, on stdout: one
# table per input file, one quoted literal per line, a 0 sentinel
# closing each. The prelude's source ships inside the binary
# (std::meta parses like any other source, 08-reflection.md) and so
# does std's runtime half -- panic is a plain fn, the calls the
# checks make name it (01-types.md); these tables are how both get
# there.
#
# A table's name spells its file's own: std/meta.xyz the
# prelude_src_meta below, prelude.c's registry reading it there.
#
# The input must be ASCII with no other rule: a backslash and a quote
# are escaped, and each line contributes its own "\n". An empty line
# is "", not nothing -- the newline still counts. One literal per
# line keeps each within C90's 509-char minimum, which a single
# joined literal would exceed (-Woverlength-strings); the reader
# (prelude.c) joins them.

set -e

[ $# -ge 1 ] || { echo "usage: embed.sh file..." >&2; exit 1; }

for f in "$@"; do
  [ -f "$f" ] || { echo "embed.sh: $f: not a file" >&2; exit 1; }
  b=$(basename "$f")
  b=${b%.xyz}

  echo "/* generated from $f by tools/embed.sh -- do not edit */"
  echo "static const char *const prelude_src_$b[] = {"

  sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/  "/' -e 's/$/\\n",/' "$f"

  echo "  0,"
  echo "};"
  echo
done
