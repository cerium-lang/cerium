#!/bin/sh
# run_tests.sh -- the golden tests.
#
# Every tests/ok/*.xyz must dump exactly its .golden as a token
# stream (regenerate one with: ./xyz -t tests/ok/NN.xyz >
# tests/ok/NN.golden -- after checking the dump by hand). Every
# tests/parse/*.xyz must dump exactly its .golden as an AST (ditto
# with -a). Every tests/err/*.xyz must be rejected with a diagnostic
# on stderr and a nonzero exit.
set -u
cd "$(dirname "$0")/.."

fail=0

for f in tests/ok/*.xyz; do
  g="${f%.xyz}.golden"
  if [ ! -f "$g" ]; then
    echo "FAIL $f (no .golden)"
    fail=1
    continue
  fi
  if ./xyz -t "$f" 2>/dev/null | diff -u "$g" - >/dev/null; then
    echo "ok   $f"
  else
    echo "FAIL $f"
    ./xyz -t "$f" 2>/dev/null | diff -u "$g" - | sed 's/^/     /'
    fail=1
  fi
done

for f in tests/parse/*.xyz; do
  g="${f%.xyz}.golden"
  if [ ! -f "$g" ]; then
    echo "FAIL $f (no .golden)"
    fail=1
    continue
  fi
  if ./xyz -a "$f" 2>/dev/null | diff -u "$g" - >/dev/null; then
    echo "ok   $f"
  else
    echo "FAIL $f"
    ./xyz -a "$f" 2>/dev/null | diff -u "$g" - | sed 's/^/     /'
    fail=1
  fi
done

for f in tests/err/*.xyz; do
  if ./xyz -t "$f" >/dev/null 2>&1; then
    echo "FAIL $f (lexed cleanly; an error was expected)"
    fail=1
  else
    echo "ok   $f"
  fi
done

exit $fail
