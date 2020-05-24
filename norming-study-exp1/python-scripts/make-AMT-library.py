#This script reads in a CSV file with two columns: a list number and a lexical entry. It outputs a TXT file in which each row is a list, i.e. it collects the lexical entries with the same list number into a list/row. 

#Based on a script by Marie, modified by Judith in August 2014

import csv
import re
import sys
import os
	
src_filename = sys.argv[1]
output_filename = sys.argv[2]

reader = csv.reader(file(src_filename))
writer = csv.writer(file(output_filename, 'w'), delimiter=',')

# header is a list containing the first line:
header = reader.next()

newHeader = []

newHeader.append("List number")
newHeader.append("List")

print newHeader

writer.writerow(newHeader)

#Desired output format for each list:
#Each list is a row with a set in it
#[{id:"...",condition:"...",context:"...",target:"...",name:"...",adj:"...",VPinf:"..."},
#{id:"...",context:"...",target:"...",name:"...",adj:"...",VPinf:"..."},
#{id:"...",context:"...",target:"...",name:"...",adj:"...",VPinf:"..."},
#...],

#clumsy way of making 24 lists for the experiment
list1 = set()
list2 = set()
list3 = set()
list4 = set()
list5 = set()
list6 = set()
list7 = set()
list8 = set()
list9 = set()
list10 = set()
list11 = set()
list12 = set()
list13 = set()
list14 = set()
list15 = set()
list16 = set()
list17 = set()
list18 = set()
list19 = set()
list20 = set()
list21 = set()
list22 = set()
list23 = set()
list24 = set()

#create set for each list
for row in reader:

	newRow = []
	
	if row[0] == "1":
		list1.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "2":
		list2.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
		
	if row[0] == "3":
		list3.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "4":
		list4.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
				
	if row[0] == "5":
		list5.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "6":
		list6.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
				
	if row[0] == "7":
		list7.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "8":
		list8.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
				
	if row[0] == "9":
		list9.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "10":
		list10.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
				
	if row[0] == "11":
		list11.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "12":
		list12.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
				
	if row[0] == "13":
		list13.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "14":
		list14.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
				
	if row[0] == "15":
		list15.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "16":
		list16.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
				
	if row[0] == "17":
		list17.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "18":
		list18.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
		
	if row[0] == "19":
		list19.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "20":
		list20.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
				
	if row[0] == "21":
		list21.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "22":
		list22.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
		
	if row[0] == "23":
		list23.add(row[1])
		#print row[1]
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
	
	if row[0] == "24":
		list24.add(row[1])
		newRow.append(row[0])
		newRow.append(row[1])
		writer.writerow(newRow)
		


#turn each set into a string so that I can use append
list1 = str(list1)
list2 = str(list2)
list3 = str(list3)
list4 = str(list4)
list5 = str(list5)
list6 = str(list6)
list7 = str(list7)
list8 = str(list8)
list9 = str(list9)
list10 = str(list10)
list11 = str(list11)
list12 = str(list12)
list13 = str(list13)
list14 = str(list14)
list15 = str(list15)
list16 = str(list16)
list17 = str(list17)
list18 = str(list18)
list19 = str(list19)
list20 = str(list20)
list21 = str(list21)
list22 = str(list22)
list23 = str(list23)
list24 = str(list24)

tmpLibrary = ",\n".join([list1,list2,list3,list4,list5,list6,list7,list8,list9,list10,list11,list12,list13,list14,list15,list16,list17,list18,list19,list20,list21,list22,list23,list24])

print list1
print list2
print list3

tmpLibrary = tmpLibrary.replace("'{", "{")
tmpLibrary = tmpLibrary.replace("}'", "}")
tmpLibrary = tmpLibrary.replace("set(", "")
tmpLibrary = tmpLibrary.replace("])", "]")

Library = tmpLibrary
print Library

#curpath = os.path.abspath(os.curdir)
#print curpath

os.chdir(r'/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/2evaluative-adjectives/norming-experiment/intermediate-stimuli-files/')

file = open("library.txt", "w+")
file.write(Library)
file.close()




	
	
	
	