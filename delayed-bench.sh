#!/bin/bash

set -e

sbt 'test:run-main crypto.dsl.DelayedBench -CresultDir delayed-bench'
rsync -r --remove-source-files tmp/ delayed-bench/
