#!/bin/sh
# run-tests.sh -- the golden tests.
#
# tests/lex, tests/parse and tests/check split by pass: every lex/ok
#/*.xyz must dump exactly its .golden as a token stream (regenerate
# one with: ./xyz -t tests/lex/ok/NN.xyz > tests/lex/ok/NN.golden --
# after checking the dump by hand), every parse/ok/*.xyz as an AST
# (ditto with -a), every check/ok/*.xyz as resolved declarations
# (ditto with -T), and every err/*.xyz under any of them must be
# rejected with a diagnostic on stderr and a nonzero exit.
#
# A directory in place of the .xyz is a project (12-projects.md):
# every .xyz under it, each in the namespace its path spells. The
# project tests live the same way -- check/ok/NN-name/ diffs against
# the NN-name.golden beside it (one (file ...) block per file, -T
# only), check/err/NN-name/ must reject, and tests/run/NN-name/
# compiles to a binary whose exit code the NN-name.expect names. -t
# and -a read one file; a project tests through -T, -s and -c.
#
# tests/run is codegen's: every *.xyz compiles to a binary whose
# exit code the matching .expect names; an optional .stdout holds
# the bytes it must print (#[extern(C)] write is how the language
# prints until M3c). It needs qbe/qbe built -- a checkout without
# it skips the section rather than failing.
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
  for d in "$1"/*/; do # a project: the .golden beside the directory
    [ -d "$d" ] || continue
    g="${d%/}.golden"
    if [ ! -f "$g" ]; then
      echo "FAIL $d (no .golden)"
      fail=1
      continue
    fi
    if ./xyz "$2" "$d" 2>/dev/null | diff -u "$g" - >/dev/null; then
      echo "ok   $d"
    else
      echo "FAIL $d"
      ./xyz "$2" "$d" 2>/dev/null | diff -u "$g" - | sed 's/^/     /'
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
  for d in "$1"/*/; do # a project: -T reads the whole thing
    [ -d "$d" ] || continue
    if ./xyz "${2:--T}" "$d" >/dev/null 2>&1; then
      echo "FAIL $d (accepted; an error was expected)"
      fail=1
    else
      echo "ok   $d"
    fi
  done
}

runthem() { # $1: the directory; an .expect of "!" wants rejection,
  # an optional .stdout holds the bytes the binary must print
  tmp=$(mktemp -d)
  for f in "$1"/*.xyz; do
    [ -e "$f" ] || continue
    g="${f%.xyz}.expect"
    if [ ! -f "$g" ]; then
      echo "FAIL $f (no .expect)"
      fail=1
      continue
    fi
    exp=$(cat "$g")
    if [ "$exp" = "!" ]; then
      if ./xyz -c "$f" -o "$tmp/out" 2>/dev/null; then
        echo "FAIL $f (compiled; a rejection was expected)"
        fail=1
      else
        echo "ok   $f"
      fi
      continue
    fi
    if ! ./xyz -c "$f" -o "$tmp/out" 2>"$tmp/err"; then
      echo "FAIL $f (rejected: $(head -1 "$tmp/err"))"
      fail=1
      continue
    fi
    "$tmp/out" >"$tmp/stdout"
    got=$?
    if [ "$got" != "$exp" ]; then
      echo "FAIL $f (exit $got, want $exp)"
      fail=1
      continue
    fi
    s="${f%.xyz}.stdout"
    if [ -f "$s" ] && ! cmp -s "$s" "$tmp/stdout"; then
      echo "FAIL $f (stdout $(head -c 40 "$tmp/stdout" | tr '\n' ' ')...)"
      fail=1
      continue
    fi
    echo "ok   $f"
  done
  for d in "$1"/*/; do # a project: one binary, the walk's every file
    [ -d "$d" ] || continue
    g="${d%/}.expect"
    if [ ! -f "$g" ]; then
      echo "FAIL $d (no .expect)"
      fail=1
      continue
    fi
    exp=$(cat "$g")
    if [ "$exp" = "!" ]; then
      if ./xyz -c "$d" -o "$tmp/out" 2>/dev/null; then
        echo "FAIL $d (compiled; a rejection was expected)"
        fail=1
      else
        echo "ok   $d"
      fi
      continue
    fi
    if ! ./xyz -c "$d" -o "$tmp/out" 2>"$tmp/err"; then
      echo "FAIL $d (rejected: $(head -1 "$tmp/err"))"
      fail=1
      continue
    fi
    "$tmp/out" >"$tmp/stdout"
    got=$?
    if [ "$got" != "$exp" ]; then
      echo "FAIL $d (exit $got, want $exp)"
      fail=1
      continue
    fi
    s="${d%/}.stdout"
    if [ -f "$s" ] && ! cmp -s "$s" "$tmp/stdout"; then
      echo "FAIL $d (stdout $(head -c 40 "$tmp/stdout" | tr '\n' ' ')...)"
      fail=1
      continue
    fi
    echo "ok   $d"
  done
  rm -rf "$tmp"
}

golden tests/lex/ok -t
golden tests/parse/ok -a
golden tests/check/ok -T
rejected tests/lex/err -t
rejected tests/parse/err -a
rejected tests/check/err -T
if [ -x qbe/qbe ]; then
  runthem tests/run
else
  echo "skipped tests/run -- build qbe first: make qbe/qbe"
fi

exit $fail
