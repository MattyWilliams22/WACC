#!/bin/bash

# Get the file name from the command line arguments
fname=$1

# Strip the file extension from the file name
filename=$(basename -- "$fname")
filename="${filename%.*}"

echo "make"
make

# Print and run the compile command
echo "./compile $fname"
./compile $fname

# Print and run the gcc command
echo "arm-linux-gnueabi-gcc -o execFile -z noexecstack -march=armv6 $filename.s"
arm-linux-gnueabi-gcc -o execFile -z noexecstack -march=armv6 $filename.s

# Print and run the qemu-arm command
echo "qemu-arm -L /usr/arm-linux-gnueabi/ execFile"
qemu-arm -L /usr/arm-linux-gnueabi/ execFile

# rm $filename.s
# rm execFile