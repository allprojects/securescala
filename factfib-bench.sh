#!/bin/bash

set -e

sbt 'test:run-main crypto.dsl.FactFibBench -CresultDir factfib-bench'
rsync -r --remove-source-files tmp/ factfib-bench/
