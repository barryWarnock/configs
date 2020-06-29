#!bash
cd `dirname $0`
#if there's been changes commit them and push to remote
if (( `git diff | wc -l` > 0 )); then
    git commit . -m `date +%D`_auto
    git push
fi
git pull
