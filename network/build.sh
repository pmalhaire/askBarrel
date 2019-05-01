mkdir -p bin
for file in *.hs
do
	ghc -dynamic -no-keep-hi-files -no-keep-o-files $file -o bin/$(basename $file .hs)
done
