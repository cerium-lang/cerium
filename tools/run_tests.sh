#!/bin/sh
# run_tests.sh -- the golden tests.
#
# tests/lex, tests/parse and tests/check split by pass: every lex/ok
#/*.xyz must dump exactly its .golden as a token stream (regenerate
# one with: ./xyz -t tests/lex/ok/NN.xyz > tests/lex/ok/NN.golden --
# after checking the dump by hand), every parse/ok/*.xyz as an AST
# (ditto with -a), every check/ok/*.xyz as resolved declarations
# (ditto with -T), and every err/*.xyz under any of them must be
# rejected with a diagnostic on stderr and a nonzero exit.
set -u
cd "$(dirname "$0")/.."

fail=0

golden() { # $1: the directory, $2: the dump flag
  for f in "$1"/*.xyz; do
    [ -e "$f" ] || continue
    g="${f%.xyz}.golden"
    if [ ! -f "$g" ]; then
      echo "FAIL $f (no .golden)"
      fail=1
      continue
    fi
    if ./xyz "$2" "$f" 2>/dev/null | diff -u "$g" - >/dev/null; then
      echo "ok   $f"
    else
      echo "FAIL $f"
      ./xyz "$2" "$f" 2>/dev/null | diff -u "$g" - | sed 's/^/     /'
      fail=1
    fi
  done
}

rejected() { # $1: the directory, $2: the dump flag (-a or -T)
  for f in "$1"/*.xyz; do
    [ -e "$f" ] || continue
    if ./xyz "${2:--a}" "$f" >/dev/null 2>&1; then
      echo "FAIL $f (accepted; an error was expected)"
      fail=1
    else
      echo "ok   $f"
    fi
  done
}

golden tests/lex/ok -t
golden tests/parse/ok -a
golden tests/check/ok -T
rejected tests/lex/err -t
rejected tests/parse/err -a
rejected tests/check/err -T

exit $fail
