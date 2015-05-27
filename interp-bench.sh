#!/bin/bash

set -e

sbt 'test:run-main crypto.dsl.*InterpreterBench -CresultDir interp-bench'
rsync -r --remove-source-files tmp/ interp-bench/
