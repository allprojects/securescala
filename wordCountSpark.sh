#!/bin/bash

usage()
{
    echo "Usage: $0 -m [<master_url>] -f [<file_name>]" 1>&2;
    exit -1;
}

MASTER_URL="spark://$(hostname):7077"
FILE=./ghci-debugger.txt

while getopts ":m:h:f:" o; do
    case "${o}" in
	m)
	    MASTER_URL=${OPTARG} ;;
	f)
	    FILE=${OPTARG} ;;
	h)
	    usage ;;
	*) ;;
    esac
done

sbt assembly
if [ -n ${SPARK_HOME} ]
then ${SPARK_HOME}/bin/spark-submit --master ${MASTER_URL} --class crypto.casestudies.WordCountSpark ./target/scala-2.11/master_thesis_source-assembly-1.0.jar ${FILE}
else echo "Please set your SPARK_HOME variable" 1>&2
fi

rm -f encrypted_test.txt
