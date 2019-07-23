#!/bin/bash

# bash tips: https://twitter.com/b0rk/status/1000208860060307456

# run 2 commands
# semi-colon separated (independent commands)
echo "foo"; echo "bar"
# double ampersand (run cmd2 iff cmd1 successful)
echo "foo" && echo "bar"

# list files in directory (human-readable)
ls -l
ls -lh
# reverse sort
ls -ltr
# count number (n.b. +1)
ls -l | wc -l
# or for larger dirs (includes hidden files)
ls -f | wc -l

# size of directory (summary, human-readable)
du -hs dir/path

# argvars
X=$1
Y=$2
Z=$3

# check number of argvars
if [ "$#" -ne 1 ]
then
  echo "Usage: ..."
  exit 1
fi

# do nothing
if [ "$foo" == "foo" ]; then
    :
fi

# floating point comparison
# https://stackoverflow.com/questions/8654051/how-to-compare-two-floating-point-numbers-in-bash
DUR1=10
DUR2=5
if (( $(echo "$DUR2 > $DUR1" |bc -l) )); then
    echo "foobar"
fi



# printf
printf "."
printf ".\n"  # does not add newline by default

# make an alias
add to ~/.bashrc
e.g. alias praat='/Applications/Praat.app/Contents/MacOS/Praat'


# wait for keypress
read -n 1 -s  # one key, silent response


# or take input as new variable
read CP


# read file line by line
while read p; do
  echo $p
done < filein.txt


# while loop
COUNTER=0
while [  $COUNTER -lt 10 ]; do
    echo The counter is $COUNTER
    let COUNTER=COUNTER+1 
done

# if equals
if [ $VAR -eq 0 ]; then
    echo "foo"
fi

# IF/ELSE + evaluate variable as string (note whitespace)
if [ "$CP" == "c" ]; then
    echo "C"
else
    echo "OTHER"
fi
# else if
if [ "$CP" == "c" ]; then
    echo "C"
elif [ "$CP" == "b" ]; then
    echo "B"
fi

# and operator
if [[ "$A" == "$B" && "$A" == "$C" ]]; then
    echo "blah"
fi

# or operator
if [[ "$A" == "foo" || "$B" == "bar" ]]; then
    echo "blah"
fi

# if contains
string='My long string';
if [[ $string == *"My long"* ]]
then
  echo "It's there!";
fi


# sleep (seconds)
sleep 2


# print newlines
echo -e "Hello\nWorld\n"


# check for dir, mkdir if doesn't exist
mkdir -p foo


# check if file exists
if [ ! -f foo.txt ]; then
    echo "file not found"
fi


# double check with user before taking action (i.e. overwrite); http://stackoverflow.com/questions/226703/how-do-i-prompt-for-input-in-a-linux-shell-script
echo "existing pass2 file found: are you sure you want to overwrite? (y = 1, n = 2)"
select yn in "Yes" "No";
do
case $yn in
    Yes ) cp -v $TXTPATH1 $TXTPATH2; break;;
    No ) break;;
esac
done


# rerun command if program exits with 1 (error) rather than 0 (success); $? captures exit value [careful of infinite loops!]
STATUS=1
until [ $STATUS == 0 ]
do
    perl foo.pl  # e.g. perl
    STATUS=$?
done


# declare array
declare -a ARRAY=("a" "b" "c")


# fetch array of filenames from directory
FILEARRAY=( $SOURCEDIR/* )

# loop thru array
for FILE in "${FILEARRAY[@]}"
do
    echo $FILE
done

# multi line output of piped command as array
IFS=$'\n'
ADVS=( `egrep -v "^\%mor" $FILE | egrep -n "ad.\|just"` )

# randomise array (takes a while with a large array)
# https://delightlylinux.wordpress.com/2012/08/16/how-to-shuffle-a-bash-array/
IFS=$'\n'
src=(red green blue yellow orange black 'hello world')
dest=()
# Show original array
echo -e "\xe2\x99\xa6Original array\xe2\x99\xa6"
echo "${src[@]}"
# Function to check if item already exists in array
function checkArray
{
 for item in ${dest[@]}
 do
 [[ "$item" == "$1" ]] && return 0 # Exists in dest
 done
 return 1 # Not found
}
# Main loop
while [ "${#dest[@]}" -ne "${#src[@]}" ]
do
 rand=$[ $RANDOM % ${#src[@]} ] 
 checkArray "${src[$rand]}" || dest=(${dest[@]} "${src[$rand]}")
done
# Show result
echo -e "\n\xe2\x99\xa6Shuffled Array\xe2\x99\xa6"
echo "${dest[@]}"



# loop through range of numbers
for i in {1..10}
    do
    echo $i
done

# alternatively
for CANDDIR in $INDIR/*; do
    for INFILE in $CANDDIR/*_syntactic.xml; do
        echo $INFILE
    done
done


# fetch name versus extension parts of filename
FILENAMEFULL=$(basename "$FILE")
FILENAMESHORT="${FILENAMEFULL%.*}"
EXTENSION="${FILENAMEFULL##*.}"


# substitution using sed
NEW=$(echo -n "foo," | sed 's/,//')  # remove trailing comma, n.b. echo -n means no newline

# delete empty lines
cat file.txt | sed '/^$/d'

# sed chained and including deletion / greedy subs
cat test.txt | sed "s/^<s> <s> <\\/s>$//g; /^$/d"

# sed in place: https://robots.thoughtbot.com/sed-102-replace-in-place
sed -i 's/foo/f/' foo.txt
# with back up of original file
sed -i .bak 's/hello/bonjour/' greetings.txt
# or 'zero length extension' for no back up
sed -i '' 's/hello/bonjour/' greetings.txt

# return matched string
echo "foo" | sed 's/^(.)/\1/'

# sed in place by line number
LINENUM=3
gsed -i '${LINENUM}s/ad.|just/adv_just|retro/' $FILE

# sed for line number (range)
gsed -n -e ${LINENUM}p $FILE
gsed -n -e $WINDOW,${LINENUM}p $FILE

# strip newlines
TRANSCR=`cat $FILE | sed -e ':a' -e 'N' -e '$!ba' -e 's/.\n/ /g'`

# capitalised casing using gsed
echo $TRANSCR | gsed -e 's/.*/\L&/' | gsed -e 's/^./\U&/'

# trim whitespace
gsed -e 's/^[[:space:]]*//'  # leading
gsed -e 's/[[:space:]]*$//'  # trailing


## replace spaces with tabs (-i: in place)
perl -p -i -e 's/\s{2,}/\t/g' file.txt


# arithmetic using bc, incl floating point; http://phoxis.org/2009/12/23/floatmathbash/
echo "$var + 0.5" | bc


# maths with variables
newnum=$(($num1 + $num2))


# open file with specified program
open -a /Applications/Audacity/Audacity.app/Contents/MacOS/Audacity foo.wav


# pause and resume a running job
Ctrl-Z  # suspends job
jobs  # lists suspended jobs
fg %1  # restarts job #1
http://stackoverflow.com/questions/20649783/pause-a-running-script-in-mac-terminal-and-then-resume-later


# convert Ctrl-M (^M) DOS newlines to UNIX
perl -pe 's/^M/\n/g' < filein > fileout  # with -pi -e for in place editing, where ^M = ctrl+V+M
# or
tr -d '\r' < filein > fileout


# check machine name
hostname


# create tar gzip file (extension .gz)
tar -czvf [output] [target]
# (decompress is -xzvf)


# substrings: cut
INPUT=someletters_12345_moreleters.ext
SUBSTRING=`echo $INPUT| cut -d'_' -f 2`  # where -d is the delimiter and -f the field num.
echo $SUBSTRING  # 12345
# cut by tab
echo "foo    tab" | cut -d$'\t' -f 1

# or by index (zero based)
echo ${SUBSTRING:0:2}  # from zero, length of 2


# get filename from full path: basename
FILENAME=$(basename $FULLPATH)


# get number of lines via wc and awk
wc $file | awk {'print $4" "$2" "$1'}  # $4: filename, $2: words, $1: lines


# decompress all tar files in directory
for filename in *.tar.gz
do
  tar zxf $filename
done


# get timestamp: see https://www.gnu.org/software/coreutils/manual/html_node/Date-conversion-specifiers.html
T=`date +"%F_%T"`
echo $T

# time process
# user, real, sys http://stackoverflow.com/questions/556405/what-do-real-user-and-sys-mean-in-the-output-of-time1
time echo "hello world"
# store time as var http://unix.stackexchange.com/questions/12068/how-to-measure-time-of-program-execution-and-store-that-inside-a-variable
rtime="$( TIMEFORMAT='%lR';time ( ls ) 2>&1 1>/dev/null )"
echo $rtime


## OS upgrade: update macports, see: https://trac.macports.org/wiki/Migration

## install command line tools
xcode-select --install

## checksum
md5 download


## search by filename
find ./ -name xyling.sty

## another text editor
nano


## grep across multiple lines (-l for filenames only)
pcregrep -l -M 'record_eng-ger.*(\n|.)*<status>app' ~/Corpora/CrowdeeDeutschEnglish/resultsXML/*

## grep with line number as output
grep -n "foo" file


## append to existing string
FOO=$FOO"bar"


## split string
## http://stackoverflow.com/questions/918886/how-do-i-split-a-string-on-a-delimiter-in-bash
#!/usr/bin/env bash
IN="bla@some.com;john@home.com"
mails=$(echo $IN | tr ";" "\n")
for addr in $mails
do
    echo "> [$addr]"
done


## check if string is whitespace only
## if -z checks for zero string length
if [ -z "${SU// }" ]; then
    echo "foo"
else
    echo "poo"
fi


## argument list too long
# use find $dir $filename pass to -exec
find formattedTranscripts/*/ -name '*_S[CDE]_*' -exec ls {} \;
# or pipe to xargs
find formattedTranscripts/*/ -name '*_S[CDE]_*' -print0 | xargs -0 ls
# tar from filelist
find formattedTranscripts/*/ -name '*_S[CDE]_*' -exec ls {} \; > filelist.txt
tar -cvz -T filelist.txt -f tarball.tar.gz


## get random number
## random is an internal var 0..32767
## http://stackoverflow.com/questions/1194882/how-to-generate-random-number-in-bash
RANDNUM=$((1 + RANDOM * 10))


## run command N times
for i in {1..5000}; do echo $i; Rscript annotateTrainingData_rasp.R; done


## do not open the GUI in Emacs: in ~/.bash_profile
alias emacs='emacs -nw'

## prevent auto backups in emacs
# https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
$ emacs ~/.emacs.d/init.el
# add
(setq make-backup-files nil)


## Unicode: check for non-ASCII characters
# https://stackoverflow.com/questions/13596531/how-to-search-for-non-ascii-characters-with-bash-tools
nonascii() { LANG=C grep --color=always '[^ -~]\+'; }
# e.g.
printf 'ŨTF8\n' | nonascii


## add an ssh key
# https://confluence.atlassian.com/bitbucketserver/creating-ssh-keys-776639788.html#CreatingSSHkeys-CreatinganSSHkeyonLinux&macOS
# check for existing keys first
cd ~/.ssh
ls id_*
# use them or back up if necessary
mkdir key_backup
cp id_rsa* key_backup
# make a new key
ssh-keygen -t rsa -C "your_email@example.com"
# add to ssh-agent (on Linux only?)
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa
## copy public key
pbcopy < ~/.ssh/id_rsa.pub

# ssh reset
ssh -o StrictHostKeyChecking=No foo.bar.com


# length of soundfile with sox
sox out.wav -n stat 2>&1 | sed -n 's#^Length (seconds):[^0-9]*\([0-9.]*\)$#\1#p'

