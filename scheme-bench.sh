#!/bin/bash

set -e

sbt 'test:run-main crypto.cipher.AesBench -CresultDir scheme-bench'
rsync -r --remove-source-files tmp/ scheme-bench/
