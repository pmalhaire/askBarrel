file=$1
count=$2
if [[ -z "$3" ]];then
	delay=.1
else
	delay=$3
fi

spaces() {
	for ((n=0;n<$1;n++)); do
			echo -ne " "
	done
}

for ((i=0;i<$count;i++)); do
  while IFS= read -r line
	do
		spaces $i
		echo -e "$line"
	done < $file
	sleep $delay
	while read line; do echo -ne "\033[1A\033[K\r";done < $file
done

for ((i=$count;i>0;i--)); do
  while IFS= read -r line
	do
		spaces $i
		echo -e "$line"
	done < $file
	sleep $delay
	while read line; do echo -ne "\033[1A\033[K\r";done < $file
done
