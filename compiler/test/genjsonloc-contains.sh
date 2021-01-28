#! /bin/bash -e

# Copyright (c) Facebook, Inc. and its affiliates.

# Compare EmitJSON WithoutLoc to EmitJSON WithLoc
#
# Expect EmitJSON WithLoc to be contained in EmitJSON WithoutLoc output
#
# Funny enough, we can still include .options.out_path since since jq
# does substring checks for constains on string, and adding "-loc" to the
# directory passes this substring check.

echo "$0"
here="$1"

if [ -z "$here" ]; then
  echo "FAIL : $0 : Need directory argument";
  exit 2;
fi

pre="${here}/fixtures"

echo "Prefix: ${pre}"

# Takes directory 1 (WithoutLoc) and directory 2 (WithLoc) and filename
check() {
    d1="$1";
    d2="$2";
    f="$3";
    path1="${pre}/${d1}/${f}";
    path2="${pre}/${d2}/${f}";
    json1=$(jq . "${path1}");
    r=$(jq "contains(${json1})" "${path2}");
    if [ "true" = "${r}"  ]; then
      echo "PASS : $0  : check ${d1} ${d2} ${f}"
    else
      echo "FAILED : $0 : check ${d1} ${d2} ${f}"
      exit 1;
    fi
}

check "gen-basic" "gen-basic-loc" "a.ast"
check "gen-basic" "gen-basic-loc" "b.ast"
check "gen-single-out" "gen-single-out-loc" "a.ast"
