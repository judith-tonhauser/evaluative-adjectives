#This script extracts the finite verbs from a CSV file with several rows of stimuli to create a dictionary in which each finite verb is paired with its infinitival form

import csv
import re
import sys


def infinitive(str):
	#inf = str.replace(str[0], dictionary[str[0]])
	#infString = " ".join([inf, str])
	firstWord = str.partition(' ')[0]
	inf = str.replace(firstWord, mydict[firstWord])
	return inf
	#return infString 
	
#takes a string consisting of words, returns the finite verb (second word), removes dot if verb ends with dot
def getVerb(str):
	#print str
	#get the words in the string
	secondWord = str.split()[1]
	#remove dot from the end of the word if there is one
	last = secondWord[-1:]
	if last == ".":
		secondWord = secondWord[:-1]
	else: pass
	#print secondWord
	return secondWord

	

src_filename = sys.argv[1]
output_filename = sys.argv[2]

reader = csv.reader(file(src_filename))
writer = csv.writer(file(output_filename, 'w'), delimiter=',')

# header is a list containing the first line:
header = reader.next()

newHeader = []

newHeader.append("Finite verb")
#newHeader.append(header[3])
#newHeader.append(header[27])
#newHeader.append(header[28])

print newHeader

writer.writerow(newHeader)

entries = set()

for row in reader:
	
	#Extract verb from VP1 Consonant column
	newRow1 = []
	sentence = row[3]
	finVerb = getVerb(sentence)
	if finVerb not in entries:
		entries.add(finVerb)
		newRow1.append(finVerb)
		writer.writerow(newRow1)
		print finVerb
	
	#print newRow1
	#writer.writerow(newRow1)
	
	#Extract verb from VP1 Dissonant column
	newRow2 = []
	sentence = row[4]
	finVerb = getVerb(sentence)
	if finVerb not in entries:
		entries.add(finVerb)
		newRow2.append(finVerb)
		writer.writerow(newRow2)
		print finVerb
	
	#print newRow2
	#writer.writerow(newRow2)
	
	#Extract verb from VP2
	newRow3 = []
	sentence = row[8]
	finVerb = getVerb(sentence)
	if finVerb not in entries:
		entries.add(finVerb)
		newRow3.append(finVerb)
		writer.writerow(newRow3)
		print finVerb
	
	#print newRow3
	#writer.writerow(newRow3)
	
print entries

#Iterate through the rows, each a list:
# for row in reader:
# 	# Row is a list. You can work with it directly. I like to 
# 	# create a dictionary for each row, allowing easy access 
# 	# to keys and values:
# 	#d = dict(zip(header, row))
# 	
# 	#Extract verb from VP1 Consonant column
# 	newRow1 = []
# 	sentence = row[3]
# 	finVerb = getVerb(sentence)
# 	newRow1.append(finVerb)
# 	
# 	#print newRow1
# 	writer.writerow(newRow1)
# 	
# 	#Extract verb from VP1 Dissonant column
# 	newRow2 = []
# 	sentence = row[4]
# 	finVerb = getVerb(sentence)
# 	newRow2.append(finVerb)
# 	
# 	#print newRow2
# 	writer.writerow(newRow2)
# 	
# 	#Extract verb from VP2
# 	newRow3 = []
# 	sentence = row[7]
# 	finVerb = getVerb(sentence)
# 	newRow3.append(finVerb)
# 	
# 	#print newRow3
# 	writer.writerow(newRow3)
	