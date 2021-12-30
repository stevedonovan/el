cat readme.md functions.md | el match L ^'^%$ (.+)' > test.sh
bash -v test.sh > output.txt 2>&1
