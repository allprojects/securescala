#!/bin/bash

set -e

sbt 'test:run-main crypto.dsl.WordCountBench -CresultDir word-count-bench'
rsync -r --remove-source-files tmp/ word-count-bench/
