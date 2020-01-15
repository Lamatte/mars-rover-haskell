inotifywait -r test src -m -e attrib |
while read dir ev; do
	echo "Change detected, testing... ($ev)"
	stack test
done
