#!/bin/bash

sbt assembly
/home/chen/projects/spark/bin/spark-submit --class crypto.casestudies.WordCountSpark ./target/scala-2.11/master_thesis_source-assembly-1.0.jar 
