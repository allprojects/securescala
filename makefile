# makefile for libOPE.so
#
# requires header for jni interface:
# javah -classpath ".:$SCALA_HOME/lib/*:../target/scala-2.11/classes" crypto.cipher.Ope
#
# sudo apt-get install libntl-dev libssl-dev

SO_NAME = libope.so
JVM_JNI_PATH=$(wildcard /usr/lib/jvm/java*/include)

# Define a virtual path for .class in the bin directory
vpath %.class $(CLASS_PATH)

all : $(SO_NAME)

# $@ matches the target, $< matches the first dependency
$(SO_NAME) : ope.o hgd.o prng.o
	g++ -Wl,--gc-sections -fPIC -shared -o $@ $^ -lntl -lssl -lcrypto

ope.o : ope.cc
	g++ -std=c++0x -fPIC -I $(JVM_JNI_PATH) -I /usr/lib/jvm/java-1.8.0-openjdk-amd64/include -I /usr/lib/jvm/java-8-openjdk-amd64/include -I /usr/lib/jvm/java-1.8.0-openjdk-amd64/include -I /usr/lib/jvm/java-8-openjdk-amd64/include/linux $< -lntl -lssl -lcrypto -c

hgd.o : hgd.cc
	g++ -std=c++0x -fPIC $^ -lntl -lssl -lcrypto -c

prng.o : prng.cc
	g++ -std=c++0x  -fPIC $^ -lntl -lssl -lcrypto -c

clean :
	rm -f *.o $(SO_NAME)
