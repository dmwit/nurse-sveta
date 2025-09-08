#!/bin/bash

set -euo pipefail

for i in "$@"
do
	grep -v '^\s*//' "$i" >"${i%.cjson}.json"
done
