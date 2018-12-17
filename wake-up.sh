#!/bin/sh
#
# Create a directory and generate template files for a given day. Ex:
#
#   ./wake-up.sh 01 "Chronal Calibration"

set -e

ROOT=$(dirname "$0")

if [ $# -ne 2 -a $# -ne 3 ]; then
    echo "usage $(basename "$0") DAY NAME" > /dev/stderr
    exit 1
fi

DAY=$1
NAME=$2

DAYDIRNAME="Day ${DAY} - ${NAME}"
DAYDIRPATH="${ROOT}/${DAYDIRNAME}"
MAINFILENAME="Main.hs"
MAINFILEPATH="${DAYDIRPATH}/${MAINFILENAME}"
CABALFILEPATH="${ROOT}/adventofcode2018.cabal"
DOCTESTFILEPATH="${ROOT}/doctest.hs"

mkdir -p "$DAYDIRPATH"

# input.txt
: > "${DAYDIRPATH}/input.txt"

# README.md
: > "${DAYDIRPATH}/README.md"

# README.part2.md
: > "${DAYDIRPATH}/README.part2.md"

# answer.md
cat <<EOF > "${DAYDIRPATH}/answer.md"
Your puzzle answer was \`?\`.
EOF

# answer.part2.md
cat <<EOF > "${DAYDIRPATH}/answer.part2.md"
Your puzzle answer was \`?\`.
EOF

# Main file
cat <<EOF > "$MAINFILEPATH"
module Main (main) where

import Text.Printf (printf)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- | TODO
main :: IO ()
main = do
    input <- getContents
    printf "%s" input
EOF

# adventofcode2018.cabal
cat <<EOF >> "$CABALFILEPATH"

executable Day${DAY}
  hs-source-dirs:      "${DAYDIRNAME}"
  main-is:             ${MAINFILENAME}
  build-depends:       base   >=4.9 && <4.10,
                       parsec >=3.1 && <3.2
  default-language:    Haskell2010
EOF

# doctest.hs
cat <<EOF >> "$DOCTESTFILEPATH"
    doctest ["${DAYDIRNAME}/${MAINFILENAME}"]
EOF
