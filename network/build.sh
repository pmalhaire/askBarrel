#! /bin/bash
# check that the file exists if given
usage () {
	echo ./build.sh [name]
	echo build [name].hs to bin/[name].
	echo "with name one of the following [name] (no .hs extension):"
	for file in ./*.hs
	do
		echo -ne "\t$(basename $file .hs)\n"
	done
	echo if [name] is ommited will build all files.
}

[ ! -z "$1" ] && [ ! -f ./$1.hs ] && \
	echo -e "\e[41m$1.hs not found\e[0m" && usage && exit 1

mkdir -p bin
for file in $1*.hs
do
	ghc -dynamic -no-keep-hi-files -no-keep-o-files $file -o bin/$(basename $file .hs)
done
