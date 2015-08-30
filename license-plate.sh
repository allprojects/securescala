#!/bin/bash

set -e

sbt 'test:run-main crypto.dsl.LicensePlateBenchSuiteRunner -CresultDir license-plate-bench'
rsync -r --remove-source-files tmp/ license-plate-bench/
