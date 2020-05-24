#This script reads in a CSV file with two columns (finite verb, infinitival form) and returns a CSV file with one column in which the two entries for each row are presented as a pair in this format: 'X' : 'Y', .

#Based on a script by Marie, modified by Judith in August 2014

import csv
import re
import sys


def tidyString(str):
	tmp1 = re.sub(r'(O|o)riginally posted by .* ', "", str)
	# remove leading spaces
	tmp2 = re.sub(r'^(\s)*', "", tmp1)
	# remove \n
	tmp3 = re.sub(r'(\n)*', "", tmp2)
	# if string doesn't end with punctuation sign, add it (for the parser)
	last = tmp3[-1:]
	print "last character: "+last
	if last != "." and last != "?" and last != "!":
		tmp3 = tmp3 + '.'
	return tmp3


def infinitive(str):
	#inf = str.replace(str[0], dictionary[str[0]])
	#infString = " ".join([inf, str])
	firstWord = str.partition(' ')[0]
	inf = str.replace(firstWord, mydict[firstWord])
	return inf
	#return infString 
	
#takes a string consisting of words, removes the first (pronoun), replaces the second with 
#an infinitival form, and adds "to" to the beginning, removes dot from end
def makeVP(str):
	#print str
	firstWord = str.split(' ', 1)[0]
	#print firstWord
	#delete first word
	tmp1 = str.split(' ', 1)[1]
	#print tmp1
	
	#replace first word (verb) with infinitival form
	tmp2 = infinitive(tmp1)
	#print tmp2
	
	#remove dot from the end of the string
	#last = tmp2[-1:]
	#print "last character: "+last
	tmp2 = tmp2[:-1]
	
	#add "to" to beginning of string
	tmp3 = " ".join(["to", tmp2])
	#print tmp3
	return tmp3

	

src_filename = sys.argv[1]
output_filename = sys.argv[2]

reader = csv.reader(file(src_filename))
writer = csv.writer(file(output_filename, 'w'), delimiter=',')

# header is a list containing the first line:
header = reader.next()

newHeader = []

newHeader.append("Pairs")
#newHeader.append(header[3])
#newHeader.append(header[27])
#newHeader.append(header[28])

print newHeader

writer.writerow(newHeader)

# Iterate through the rows, each a list:
for row in reader:

	newRow1 = []
	newRow1.append("".join(["'",row[0],"'"," ",":"," ","'",row[1],"'",","]))
	writer.writerow(newRow1)
	print newRow1
	
	
	
	