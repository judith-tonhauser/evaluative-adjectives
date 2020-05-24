#This script reads in a CSV file with experiment stimuli organized in rows of 4, and returns a CSV file in which each row is one stimulus item. The script also adds unique IDs to each stimulus, conditions to each stimulus, creates a new stimulus column in which the finite clause is turned into a "to VP", and removes the dot from that stimulus column.

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
	
	
mydict = {'got' : 'get',
'dropped' : 'drop',
'turned' : 'turn',
'told' : 'tell',
'had' : 'have',
'called' : 'call',
'updated' : 'update',
'lied' : 'lie',
'went' : 'go',
'kept' : 'keep',
'left' : 'leave',
'bought' : 'buy',
'changed' : 'change',
'brought' : 'bring',
'broke' : 'break',
'walked' : 'walk',
'found' : 'find',
'drove' : 'drive',
'married' : 'marry',
'used' : 'use',
'injected' : 'inject',
'refused' : 'refuse',
'took' : 'take',
'stole' : 'steal',
'started' : 'start',
'ordered' : 'order',
'gambled' : 'gamble',
'invested' : 'invest',
'drank' : 'drink',
'lost' : 'lose',
'hit' : 'hit',
'spoke' : 'speak',
'was' : 'be',
'developed' : 'develop',
'stayed' : 'stay',
'shared' : 'share',
'fell' : 'fall',
'jumped' : 'jump',
'watched' : 'watch',
'admitted' : 'admit',
'pulled' : 'pull',
'agreed' : 'agree',
'talked' : 'talk',
'stood' : 'stand',
'made' : 'make',
'faced' : 'face',
'cried' : 'cry',
'missed' : 'miss',
'escaped' : 'escape',
'won' : 'win',
'canceled' : 'cancel',
'waited' : 'wait',
'asked' : 'ask',
'wore' : 'wear',
'promoted' : 'promote',
'ignored' : 'ignore',
'believed' : 'believe',
'read' : 'read',
'worked' : 'work',
'insulted' : 'insult',
'closed' : 'close',
'applauded' : 'applaud',
'gave' : 'give',
'swore' : 'swear',
'laughed' : 'laugh',
'knocked' : 'knock',
'listened' : 'listen',
'apologized' : 'apologize',
'moved' : 'move',
'smiled' : 'smile',
'yelled' : 'yell',
'explained' : 'explain',
'frowned' : 'frown',
'pushed' : 'push',
'helped' : 'help',
'baked' : 'bake',
'pretended' : 'pretend',
'fed' : 'feed',
'slowed' : 'slow',
'paid' : 'pay',
'woke' : 'wake',
'kicked' : 'kick',
'cut' : 'cut'}

#replaces the finite verb with the infinitival form by looking in the dictionary above
def infinitive(str):
	#inf = str.replace(str[0], dictionary[str[0]])
	#infString = " ".join([inf, str])
	firstWord = str.partition(' ')[0]
	last = firstWord[-1:]
	if last == ".":
		firstWord = firstWord[:-1]
	else: pass
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

newHeader.append(header[0])
newHeader.append(header[1])
newHeader.append("ID")
newHeader.append("Condition")
newHeader.append("Context")
newHeader.append("Target")
newHeader.append("VPinf")
#newHeader.append(header[3])
#newHeader.append(header[27])
#newHeader.append(header[28])

print newHeader

writer.writerow(newHeader)

counter = 0

# Iterate through the rows, each a list:
for row in reader:
	# Row is a list. You can work with it directly. I like to 
	# create a dictionary for each row, allowing easy access 
	# to keys and values:
	#d = dict(zip(header, row))
	
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
	
	
	
	