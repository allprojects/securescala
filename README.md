# libope

The libope library provides the basic cryptographic functions for securescala (https://github.com/azanov/securescala, https://github.com/allprojects/securescala).

### Building

Install the `g++` compiler and `make`

```
sudo apt install build-essential
```

Install OpenJDK 8

```
sudo apt-get install openjdk-8-jdk openjdk-8-demo openjdk-8-doc openjdk-8-jre-headless openjdk-8-source
```

Install NTL

```
sudo apt-get install libntl-dev
```

Install openssl

```
sudo apt-get install libssl1.0-dev
```

Build the project:

```
make
```

### Installing

Copy **libope.so** to `/usr/lib` and run `ldconfig -n -v /usr/lib` to install it and create all necessary links.