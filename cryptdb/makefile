# makefile for libOPE.so
#
# requires header for jni interface:
# javah -classpath ".:$SCALA_HOME/lib/*:../target/scala-2.11/classes" crypto.cipher.Ope
#
# sudo apt-get install libntl-dev libssl-dev

SO_NAME = libope.so
JVM_JNI_PATH=/usr/lib/jvm/java-7-openjdk-amd64/include

# Define a virtual path for .class in the bin directory
vpath %.class $(CLASS_PATH)

all : $(SO_NAME)

# $@ matches the target, $< matches the first dependency
$(SO_NAME) : ope.o hgd.o prng.o
	g++ -Wl,--gc-sections -fPIC -shared -o $@ -I/usr/include $^ -L/usr/lib -lntl -lssl -lcrypto

ope.o : ope.cc
	g++ -std=c++0x -fPIC -I$(JVM_JNI_PATH) -I$(JVM_JNI_PATH)/linux -I/usr/include $< -L/usr/lib -lntl -lssl -lcrypto -c

hgd.o : hgd.cc
	g++ -std=c++0x -fPIC -I/usr/include $^ -L/usr/lib -lntl -lssl -lcrypto -c

prng.o : prng.cc
	g++ -std=c++0x  -fPIC -I/usr/include $^ -L/usr/lib  -lntl -lssl -lcrypto -c

clean :
	rm -f *.o $(SO_NAME)
