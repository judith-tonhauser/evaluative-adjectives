#This script reads in a CSV file with one experiment stimulus per line (10 adjectives, 24 stimuli/lines per adjective) and returns a CSV file in which each stimulus item is assigned to one of 24 lists of experiment stimuli 

#Based on a script by Marie, modified by Judith in August 2014

import csv
import re
import sys

src_filename = sys.argv[1]
output_filename = sys.argv[2]

reader = csv.reader(file(src_filename))
writer = csv.writer(file(output_filename, 'w'), delimiter=',')

# header is a list containing the first line:
header = reader.next()

newHeader = []

newHeader.append("List")
newHeader.append(header[0])
newHeader.append(header[1])
newHeader.append([3])
newHeader.append([4])
newHeader.append([5])
newHeader.append([6])
newHeader.append([7])

print newHeader

writer.writerow(newHeader)

counter = 0

# Iterate through the rows, each a list:
for row in reader:

	
	counter += 1
	#Content example #1
	newRow1 = []
	#Adjective
	newRow1.append(row[0])
	#Name
	newRow1.append(row[1])
	#ID (CnC = Content study consonant VP)
	id = [str(counter), newRow1[0], "CnC"]
	#print id
	newRow1.append("-".join(id))
	#Condition
	newRow1.append("CnC")
	#Context
	newRow1.append(row[2])
	#Target (Consonant VP)
	newRow1.append(row[3])
	
	#Create a "to VP" from the target sentence
	vp = row[3]
	#print vp
	newVP = makeVP(vp)
	#print "New Quote: "+newQuote
	newRow1.append(newVP)
	
	# After some work, you can write to your new file (where the argument is a list you created)
	#print newRow1
	writer.writerow(newRow1)
	
	#Content example #2
	newRow2 = []
	#Adjective
	newRow2.append(row[0])
	#Name
	newRow2.append(row[1])
	#ID (CnD = Content study dissonant VP)
	id = [str(counter), newRow2[0], "CnD"]
	#print id
	newRow2.append("-".join(id))
	#Condition
	newRow2.append("CnD")
	#Context
	newRow2.append(row[2])
	#Target (Dissonant VP)
	newRow2.append(row[4])
	
	#Create a "to VP" from the target sentence
	vp = row[4]
	#print vp
	newVP = makeVP(vp)
	#print "New Quote: "+newQuote
	newRow2.append(newVP)
	
	#print newRow2
	
	writer.writerow(newRow2)
	
	#Context example #1
	newRow3 = []
	#Adjective
	newRow3.append(row[0])
	#Name
	newRow3.append(row[1])
	#ID (CxC = Context study consonant context)
	id = [str(counter), newRow3[0], "CxC"]
	#print id
	newRow3.append("-".join(id))
	#Condition
	newRow3.append("CxC")
	#Context Consonant
	newRow3.append(row[6])
	#Target VP
	newRow3.append(row[8])
	
	#Create a "to VP" from the target sentence
	vp = row[8]
	#print vp
	newVP = makeVP(vp)
	newRow3.append(newVP)
	
	#print newRow3
	
	writer.writerow(newRow3)
	
	#Context example #2
	newRow4 = []
	#Adjective
	newRow4.append(row[0])
	#Name
	newRow4.append(row[1])
	#ID (CxD = Context study dissonant context)
	id = [str(counter), newRow4[0], "CxD"]
	#print id
	newRow4.append("-".join(id))
	#Condition
	newRow4.append("CxD")
	#Context Dissonant
	newRow4.append(row[7])
	#Target VP
	newRow4.append(row[8])
	
	#Create a "to VP" from the target sentence
	vp = row[8]
	newVP = makeVP(vp)
	#print "New Quote: "+newQuote
	newRow4.append(newVP)
	
	#print newRow4
	
	writer.writerow(newRow4)
	
	
	
	