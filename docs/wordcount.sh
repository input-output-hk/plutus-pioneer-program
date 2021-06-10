# word count, excluding lines starting with a space, which are usually code
cat week*.rst | grep '^[[:space:]]' | wc -w
cat week*.rst | grep '^[^ ]' | wc -w
