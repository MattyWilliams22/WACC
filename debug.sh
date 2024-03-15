#!/bin/bash

# Initialize our own variables
optimise=""

# Get the options
while getopts "o" opt; do
  case "$opt" in
  o) optimise="-o"
     ;;
  esac
done

# Shift off the options and optional --.
shift $((OPTIND-1))

# Get the file name from the command line arguments
fname=$1

# Strip the file extension from the file name
filename=$(basename -- "$fname")
filename="${filename%.*}"

echo "make clean"
make clean
echo "make"
make

# Print and run the compile command
echo "./compile $optimise $fname"
./compile $optimise $fname

# Print and run the gcc command
echo "arm-linux-gnueabi-gcc -o execFile -z noexecstack -march=armv6 $filename.s"
arm-linux-gnueabi-gcc -o execFile -z noexecstack -march=armv6 $filename.s

# Print and run the qemu-arm command
echo "qemu-arm -L /usr/arm-linux-gnueabi/ execFile"
qemu-arm -L /usr/arm-linux-gnueabi/ execFile

# rm $filename.s
# rm execFile