import csv
import itertools
import random
import ast

# put files in the order you want concatentated
csv_names = ["../experiment.results"] 

lines = []
headers = []

readers = [csv.DictReader(open(fn, 'rb'),delimiter="\t",quotechar='\"') for fn in csv_names]

for r in readers:
	lines.extend(list(r))
	headers = r.fieldnames
	
#print headers	

for (k,l) in enumerate(lines):
       	responselist = ast.literal_eval(l['Answer.responses'])
	for i in range(14):
		l['Response'+str(i)] = responselist[i]

	triallist = ast.literal_eval(l['Answer.trials'])
	#print triallist
	for i in range(14):	
		l['List'+str(i)] = triallist[i]['list']
		l['ID'+str(i)] = triallist[i]['id']
		l['Condition'+str(i)] = triallist[i]['condition']
		l['Adj'+str(i)] = triallist[i]['adj']
		l['Context'+str(i)] = "_".join(triallist[i]['context'].split(" "))
		l['Target'+str(i)] = "_".join(triallist[i]['target'].split(" "))
	
	del l['Answer.responses']
	del l['Answer.trials']	


headers.remove('Answer.responses')
headers.remove('Answer.trials')

for i in range(14):
	headers.append('Response'+str(i))
	headers.append('List'+str(i))		
	headers.append('ID'+str(i))
	headers.append('Condition'+str(i))
	headers.append('Adj'+str(i))
	headers.append('Context'+str(i))
	headers.append('Target'+str(i))


w = csv.DictWriter(open('../parsed-experiment-results.csv', 'wb'),fieldnames=headers,restval="NA",delimiter="\t")
w.writeheader()
w.writerows(lines)
