# start with:
#!/usr/bin/perl
use strict;
use warnings;


## get user name
my $user = $ENV{ LOGNAME };


## read file
open (IN, "<", $dirpath."/".$filein) or die;
while ($line = <IN>) {
    if ($line =~ //) {
	# do...;
    }
}


## install modules using cpanminus
# $ sudo cpanm Module::Name


## change directory
chdir "path/to/dir";


# take argument variables / check number of argv
if ($#ARGV == -1){
    # do something;
}
else {
    $date = $ARGV[0];
}


## open file with utf-8 encoding
open (IN, "<:encoding(UTF-8)", $filepath) or die;


# get directory list
opendir(D, "/path/to/directory") || die "Can't open directory: $!\n";
my @list = readdir(D);  # all in
# OR
my @list = grep(/regex/, readdir(D));  # pattern match
closedir(D);
for my $f (@list) {
    print "\$f = $f\n";
}


# get file modified info
my $epoch_timestamp = (stat($fh))[9];
my $timestamp       = localtime($epoch_timestamp);


# get localtime
$now_string = localtime;  # e.g., "Thu Oct 13 04:54:34 1994"


# subroutine syntax
sub sendEmail {
    my $foo = $_[0];  # pass vars
    my ($to, $from, $subject, $message) = @_;
}


# split a string
my @times = split(/\|/, $query);
my @sentences = split(/[\'\!\?]/, $text);  # multiple patterns


# create hash
my %hash = (
    a => "foo",
    b => "poo",
    c => "woo",
);

# does a key exist in hash?
if (exists $hash{$key}) {

}

# sort a hash by its keys numerically
for $counter (sort {$a <=> $b} keys %gsWords){
    # etc
}

# sort hash by values
foreach my $key  (sort { $hash{$a} <=> $hash{$b} } keys %hashes) {  do something; }


# sort a hash one level down
for $segNo (sort {$a <=> $b} keys %{$raspScores{$mode}}) {
    # etc
}


# clear a hash
%hash = ();


# arrays
http://www.tizag.com/perlT/perlarrays.php
# range
@array = (1..10);
# add to array
push(@array, $var);
# sort
@array = sort(@array);
# length of array
scalar @array;


# prompt user input
print "Press any key to continue";
$x = <STDIN>;


# sleep
sleep(2);


# filehandle operations: http://perldoc.perl.org/functions/-X.html
# if file exists
if (-e $file) { }
# or is of non-zero size
if (-s $file) { }


# system command
@cmd = ('ls', $somepath);
system(@cmd);


# get output of sys command
$var = `ls -ltr ~/filepath`


# formatting numbers
$rounded = sprintf("%.3f", $number);


# chop off last chr if a newline
chomp($var);


# trim leading/trailing whitespace
#$text = trim($text);
$text =~ s/^\s+//;  # left edge
$text =~ s/\s+$//;  # right edge


# use sed to remove lines after match
'sed -n '/^2/q;p' S2BWWT9EVS_SC_01.csv'


# pattern match, for strings
if ($var =~ /foo/) { print "something"; }  # positive
if ($var !~ /foo/) { print "something"; }  # negative
if (($var =~ /foo/) || ($var =~ /poo/)) { print "something"; }  # disjunction
if (($var =~ /foo/) && ($var =~ /poo/)) { print "something"; }  # conjunction

# substitution
$_ = $1; # this comes from an if regex statement maybe
s/</&lt;/g;  # where first item is pattern to look for, second is string to replace with
s/>/&gt;/g;
$text = $_;
# or add escape to apostrophes
$text =~ s/\'/\\\'/g;
# reduce double spaces
s/\s{2,}/ /g;
# or square brackets
$token =~ tr/[]//d;


# substring
my $x = substr $y, 1, 4;  # where 1 = offset (remember chr #1 = 0), 4 = length


# check for empty string and exit
if (($rspScore eq '') || ($treeTypeeq '')) {
    print "RASP output parse failure\n\n" and exit;
}



# loops
foreach (@things) {
    if ($_ =~ /something/) {
        last;
    }
    else {
        redo;
    }
}


# loops with labels
LOOP1: foreach (@things) {
    foreach (@morethings) {
        redo LOOP1;
    }
}


# check variable is not empty
if (length $var) {  # equivalent to length $var > 0
    # do something
}
# or
if ($var) { foo }


# check for non-match
if ($foo1 != $foo2) ...


## text to lower case
lc($text);
## text to upper case
uc($text);
## first character upper cased
ucfirst $text;

## perl one line command line
# -n = line by line
# -p = line by line and print output (i.e. supercedes -n)
# -i = edit inline and overwrite!!
# -e = evaluate
perl -pi -e 'code' filename
# regex
perl -ne 'print "$1,$2,$3\n" if /(syntactic)\s+(A)\s+(\d+)/' filename


## end of file?
if (eof()) {
    print "foo"
}


## get rid of Ctrl+M
$str =~ s/\r//g;
