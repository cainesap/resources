#! python

# install modules
pip2 install modulename
pip3 install modulename
pip3 install --ignore-installed modulename (to deal with Mac user permission issues with sys packages)
# from list of requirements
pip2 install -r requirements.txt

## set up virtualenv in Python 3
python3 -m venv /path/to/new/virtual/environment
# or
virtualenv --system-site-packages -p python3 directory_name

## install specific module path
import sys
sys.path.append('my/path/to/module')

# get current date and time
import datetime
d = str(datetime.datetime.now())
# date formatting
d.strftime('%Y-%m-%d_%H%M%S')
# date only
str(datetime.date.today())

# get epoch time
import calendar, time
calendar.timegm(time.gmtime())

# timer
from timeit import default_timer as timer
start = timer()
# ...
end = timer()
print(end-start)


# module version?
import tweepy
tweepy.__version__
# or command line
$ python -c "import lxml; print(lxml.__version__)"
# or with pip
sudo pip freeze | grep nltk


# get filename
import os
thisfile = os.path.realpath(__file__)


# read from std.in
import sys
for line in sys.stdin:
    print line


# go to specific line number
import linecache
linecache.getline(filein, n)


# get username
import getpass
uname = getpass.getuser()
dirpath = "/Users/" + uname + "/path/"


# list attributes of object
dir(obj)


# take a system argument: remember that sys.argv[0] is the script name! (so length of args will always be >=1)
import sys
if len(sys.argv) > 1:
    print( "~ Arg1: " + sys.argv[1] )
    print( "~ Arg2   : " + sys.argv[2] )
else:
    print(" No arguments ")


# take user input
person = raw_input('Enter your name: ')
print('Hello', person)


# change working dir
import os
os.chdir("path")

# get current directory
os.getcwd()

# or directory basename of file
# https://stackoverflow.com/questions/31258561/get-current-directory-python
import os
os.path.basename(os.path.dirname(os.path.realpath(__file__)))

# get list of files from directory
import glob, os
os.chdir("/mydir")
for file in glob.glob("*.txt"):
    print(file)


# check file exists
import os
os.path.exists(file_path)

# make directory
os.mkdir(file_path)


# check file size >0
import os
os.stat(file_path).st_size


# open file for read/write
f = open('file', 'r')  # read
f = open('file', 'r+')  # read/write
f = open('file', 'w')  # write
f = open('file', 'a')  # append
sometext="Hello, world!"
f.write(sometext)
f.close()

# open file and read line by line
with open(filein, 'r') as f:
    for line in f:
        print line

# skip line 1
with open(fname) as f:
    next(f)
    for line in f:
        #do something

# or use codecs to read file and specify unicode
import codecs
with codecs.open(filename, 'r', encoding='utf8') as f:
    text = f.read()

# write to unicode
with codecs.open(filename, 'w', encoding='utf8') as f:
    f.write(text)

# read 2 files line by line
with open("textfile1") as textfile1, open("textfile2") as textfile2: 
    for x, y in zip(textfile1, textfile2):
        x = x.strip()
        y = y.strip()
        print("{0}\t{1}".format(x, y))

# write json
import json
with open('data.txt', 'w') as outfile:
    json.dump(data, outfile)

# read json
# http://stackoverflow.com/questions/2835559/parsing-values-from-a-json-file-in-python
import json
from pprint import pprint

with open('data.json') as data_file:    
    data = json.load(data_file)

pprint(data)
print(len(data))
print(data[0])


# strip trailing newline
texts.append(text.rstrip('\n'))

# reduce multiple spaces to single space, trim trailing space
import re
re.sub( '\s+', ' ', mystring ).strip()

# trim leading space
text.lstrip()  # all kinds of whitespace
text.lstrip(' ')  # spaces only

text.strip()  # rm leading and trailing space

# substitute (part of a) string a number of times
n = 1  # i.e. replace a pattern only once
text.replace('del', 'sub', n)

# replace characters in substring
line = line[:10].replace(';', ':') + line[10:]

# rebuild string by character indices and replace
newmor[:strpos] + 'adv-retro|just' + newmor[strpos+ttlength:]

# read csv
import csv
f = open(filename, 'rt')
try:
    reader = csv.reader(f)
    for row in reader:
        print(row)
finally:
    f.close()
# write csv
csvfile = 'results/classify-clc-2009-b1_nb_by-topic.csv'
fileout = open(csvfile,'wt')
fileout = open(csvfile,'a')  # or append
csvwrite = csv.writer(fileout)
csvwrite = csv.writer(fileout, delimiter = ',', quotechar = '"', quoting = csv.QUOTE_ALL)  # more parameters
csvwrite.writerow(('Title 1', 'Title 2', 'Title 3'))  # dbl brackets do matter (each one is a line)
fileout.close()

# printf
print "%s, %d, %.3f" % (f[0],f[1],f[2])

# frequency distribution and most common items
from collections import Counter
c = Counter([1, 2, 1, 2, 1])
c.most_common()
# iterate over counter
for (a, b) in counter.most_common():
    print a, b

# run system command
import os
cmd = "echo Hello"
os.system(cmd)

# capture output
cmd = "py syllable3.py linguistics"
output = os.popen(cmd).read()
print(output)

# or use subprocess (includes stdout capture and piping (piping via 2 commands; 'shell = True' allows speech marks within cmd))
import subprocess
echo = subprocess.Popen('echo This is a test'.split(), stdout=subprocess.PIPE)
rasp = subprocess.Popen("/Applications/rasp3/scripts/rasp.sh -m -p'-n3 -oGITR -ph -s' -w".split(), stdin=echo.stdout, stdout=subprocess.PIPE, shell = True)
output = rasp.communicate()
# or with check_output(), and without split() -- not sure why
perplex = subprocess.Popen("echo 'perplexity -text segment.txt'", stdout=subprocess.PIPE, shell = True)
arpa = subprocess.check_output("/Users/andrewcaines/Dropbox/workspace/nlp-tools/CMU-Cam_Toolkit_v2/bin/evallm -arpa /Users/andrewcaines/Corpora/BNC/cmu-cam/sbnc.arpa".split(), stdin=perplex.stdout)


# split string on white space
textstring = "hello world   how   are you"
listofitems = textstring.split()
url = "http://stackoverflow.com/questions/1059559/python-split-strings-with-multiple-delimiters"
url.split('/')
# multiple delimiters
import re
re.split('[\/\.]', url)

# split on multiple characters
import re
DATA = "Hey, you - what are you doing here!?"
print re.findall(r"[\w']+", DATA)
POSTAG = "um_NNP internet_NNP would_MD be_VB"
print re.findall(r"[^\s_]+", POSTAG)


# regex match (exact) / search
import re
patt = re.compile('_xyz', re.IGNORECASE)  # note case insensitive
for i, (line) in enumerate(reader):
    if patt.match(line):
        print "exact"
    elif patt.search(line):
        print "at least one"

# extract substring using regex group
patt = re.compile('polyline\s+=\s+(\[\[.*\]\]);')
match = patt.search(resp)
match.group(1)

# find string position of all matches, iterate thru
iter = re.finditer(r"foo", String)
indices = [m.start(0) for m in iter]

# substitute substring in string using regex
import re
test = "<S>this is a test</S> and this is not"
def repl(m):
    return "<NS type=\"" + m.group(1) + "\">"
re.sub(r'<(\w)>', repl, test)

# or more simple case of substitution
def repl(m):
    return m.group(1)

textlines = [re.sub(r'\s(\W+)$', repl, line) for line in textlines]


# collapse list to string
test = ' '.join(list)
test = '_'.join(list)


# to lower case
test = "TeSt"
test.lower()

# to upper case
test.upper()

# capitalize first chr only
test = "test"
test.capitalize()

# identify if punctuation present
import string
test = "hello, world"
if test in string.punctuation:
    print "foo"

# exclude punctuation from string
exclude = set(string.punctuation)
s = ''.join(ch for ch in s if ch not in exclude)


# build string line by line, note double parentheses
a = "foo"
b = "noo"
"\n".join((a, b))


# concatenate string and integer (convert integer to string)
string = "johnny"
integer = 5
newstring = string + " " + str(integer)

# convert string to integer
string = "5"
int(string)

# convert string to float (decimal points)
string = "-5.5"
float(string)


# import pandas data frame
import pandas as pd
panda = pd.read_csv(filename)
print "number of tokens loaded:", len(panda)
print(panda.columns.values[0])  # column name
print(panda.iloc[0][0])  # value of row1 col1

# n.rows in panda
len(panda)
# note panda enumerate starts at 0, so final row is
len(panda)-1

# subset rows of df (remember python stop bound is 1 less than the number you give)
panda[1:10]
panda[10:len(panda)]

# panda to CSV
df.to_csv(file_name, encoding='utf-8', index=False)

# panda table of counts for a column
gapminder['continent'].value_counts()

# new data frame
df = pd.DataFrame()
df = pd.DataFrame(columns = ('a', 'b', 'c'))
# list as row in df
headers = ['a', 'b', 'c']
values = [1, 2, 3]
df = pd.DataFrame(data=[values], columns=headers)

# copy columns from old to new df
new = old[['A', 'C', 'D']].copy()

# concat 2 data frames
df3 = concat([df1, df2])

# add new column
df['foo'] = ''
df['bar'] = np.nan

# append new row
df.append({'a': 0, 'b': 1, 'c': 2}, ignore_index = True)

# list from column values
df.colname.tolist()


# what type of object?
type(foo)


## dictionaries
# empty dict
new_dict = dict()
new_dict = {}

# merge (see http://stackoverflow.com/questions/38987/how-can-i-merge-two-python-dictionaries-in-a-single-expression)
# see also http://treyhunner.com/2016/02/how-to-merge-dictionaries-in-python/
def merge_two_dicts(x, y):
    '''Given two dicts, merge them into a new dict as a shallow copy.'''
    z = x.copy()
    z.update(y)
    return z

z = merge_two_dicts(x, y)


# look up by key
dict.get('key')

# use get to avoid key errors: specify a default first
# https://wiki.python.org/moin/KeyError
default = 'Scruffy'
a = adict.get('dogname', default)

# iterate thru keys (and values): http://stackoverflow.com/questions/3294889/iterating-over-dictionaries-using-for-loops-in-python
for key in dict:
for key in sorted(dict):
for key, value in dict.iteritems():


## lists
# merge lists: http://stackoverflow.com/questions/252703/python-append-vs-extend
x = [1,2,3]
x.append([4,5])  # append a list as an item in list x
x.extend([4,5])  # adds 4 and 5 as separate elements in list x

# frequency count over a list
from collections import Counter
Counter(['apple','red','apple','red','red','pear'])


## to train Max Entropy classifier using MegaM
from nltk import MaxentClassifier
nltk.config_megam('/Users/andrewcaines/Downloads/megam_0.92/megam')
classifier = MaxentClassifier.train(trainfeats, 'megam')


## strings with (u'x') for unicode
[item.decode('UTF-8') if isinstance(item, basestring) else item for item in listx])

## range of numbers
range(0, 10)

## sequence of numbers
import numpy
numpy.arange(0, 10, 2)


## average
numpy.mean([1, 2, 3])


## logical: any/all in array
if foolist.any() == 0:
    print "foo"
elif foolist.all() == 1:
    print "boo"


## one line else if
test = 0 if littlepanda.poetic_format.all() == 1 else 1


## division of integers: make the first a float
1./10
float(var1) / var2
# or turn on 'true division'
from __future__ import division


# check if object exists
try:
  x
except NameError:
  x_exists = False
else:
  x_exists = True

# check if key in dictionary
# in is the intended way to test for the existence of a key in a dict.
d = dict()
for i in xrange(100):
    key = i % 10
    if key in d:
        d[key] += 1
    else:
        d[key] = 1

# nested dictionaries
people = {1: {'name': 'John', 'age': '27', 'sex': 'Male'},
          2: {'name': 'Marie', 'age': '22', 'sex': 'Female'}}
people[3] = {}
people[3]['name'] = 'Luna'
people[3]['age'] = '24'
people[3]['sex'] = 'Female'
people[3]['married'] = 'No'
print(people[3])


# test performance speed
from timeit import timeit
corpus = yourtext[:100000]  # maybe try 50k, 100k, 1m tokens?
timeit('normalise(corpus)', setup='import nsw', number=1000) / 1000  # import whatever libraries etc are needed .. run 1000 times and get mean secs.


## count number of words in string
s = 'I am having a very nice day.'
len(s.split())


## define a function
def process_corpus(foo):
  foo = 'bar'
  return foo


## pickle a classifier
import pickle
filename = 'finalized_model.sav'
pickle.dump(model, open(filename, 'wb'))


# unique set of items from a list
set(mylist)

# index of element in set / list
list(set).index('foo')

## NOTES
# Installing Python 2 and 3 http://joebergantine.com/blog/2015/apr/30/installing-python-2-and-python-3-alongside-each-ot/

# Python for Linguists (Dirk) https://github.com/dirkhovy/python_for_linguists

# Good logging practice (not print statements) http://victorlin.me/posts/2012/08/26/good-logging-practice-in-python

# Implementing Principal Component Analysis in Python http://sebastianraschka.com/Articles/2014_pca_step_by_step.html

# PDF concatenation https://pythonhosted.org/PyPDF2/Easy%20Concatenation%20Script.html

# pathlib for file path operations https://medium.com/@ageitgey/python-3-quick-tip-the-easy-way-to-deal-with-file-paths-on-windows-mac-and-linux-11a072b58d5f, https://twitter.com/randal_olson/status/959112885237460992
