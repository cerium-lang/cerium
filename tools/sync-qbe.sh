#!/bin/sh
# sync-qbe.sh -- move the qbe submodule pointer forward.
#
# The submodule points at the mivinci fork; upstream is self-hosted at
# c9x.me. This merges upstream master into the fork, pushes it, and stages
# the new gitlink. Run from the repo root; commit the pointer with whatever
# change rides along.
set -e

cd qbe
git fetch git://c9x.me/qbe.git master
git merge FETCH_HEAD
git push origin master
cd ..
git add qbe

echo "staged: qbe -> $(git diff --cached qbe | sed -n 's/^+Subproject commit //p')"
echo "commit it together with the change that wants the bump"
