# word count, excluding lines starting with a space, which are usually code
cat week$1.rst | grep '^[[:space:]]' | wc -w
cat week$1.rst | grep '^[^ ]' | wc -w
