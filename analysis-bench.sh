#!/bin/bash

set -e

sbt 'test:run-main crypto.dsl.AnalysisBench -CresultDir analysis-bench'
rsync -r --remove-source-files tmp/ analysis-bench/
