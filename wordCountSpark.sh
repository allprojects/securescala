#!/bin/bash

usage()
{
    echo "Usage: $0 -m [<master_url>] -f [<file_name>] -b" 1>&2;
    exit -1;
}

MASTER_URL="spark://$(hostname):7077"
FILE=./ghci-debugger.txt
CLASS=crypto.casestudies.WordCountSpark 
BENCH=
PLAINBENCH=

while getopts ":m:h:f:b" o; do
    case "${o}" in
	m)
	    MASTER_URL=${OPTARG} ;;
	f)
	    FILE=${OPTARG} ;;
	b)
	    BENCH=crypto.casestudies.WordCountSparkBench
	    PLAINBENCH=crypto.casestudies.WordCountPlainSparkBench
	    ;;
	h)
	    usage ;;
	*) ;;
    esac
done

sbt assembly
if [ -n ${SPARK_HOME} ]
then
    if [ -n ${BENCH} ]
    then ${SPARK_HOME}/bin/spark-submit --master ${MASTER_URL} --class ${BENCH} ./target/scala-2.11/master_thesis_source-assembly-1.0.jar ${FILE} 
	 ${SPARK_HOME}/bin/spark-submit --master ${MASTER_URL} --class ${PLAINBENCH} ./target/scala-2.11/master_thesis_source-assembly-1.0.jar ${FILE}
    else ${SPARK_HOME}/bin/spark-submit --master ${MASTER_URL} --class ${CLASS} ./target/scala-2.11/master_thesis_source-assembly-1.0.jar ${FILE}
    fi
else echo "Please set your SPARK_HOME variable" 1>&2
fi


rm -f encrypted_test.txt
