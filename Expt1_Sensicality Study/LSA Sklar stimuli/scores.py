import requests
import pandas as pd
from bs4 import BeautifulSoup

def clean(text):
	text = text.splitlines()
	text = [i.split() for i in text]
	text = [n for i, n in enumerate(text) if i%2 != 0]
	
	new = []
	for t in text:
		if len(t) == 4:
			new.append([t[1], t[3]])
		if len(t) > 4 and t[2] != 'the':
			new.append([t[1] + " " + t[2], t[4]])
		if len(t) > 4 and t[2] == 'the':
			new.append([t[1], t[3] + " " + t[4]])
	return new

def get_score(terms):
	txt1 = "\r\n\r\n".join(terms)
	url = "http://lsa.colorado.edu/cgi-bin/LSA-matrix-x.html"
	space = "General_Reading_up_to_1st_year_college + (300 factors)"
	factors = ''
	comp_type = 'term2term'

	r = requests.post(url,
		data={
		'LSAspace': space,
		'LSAFactors': factors,
		'txt1': txt1,
		'CmpType': comp_type}
		).text

	data = BeautifulSoup(r, 'lxml').table

	data = data.text.split()

	return [terms[0], terms[1], data[-2]]


violations = [
	['broke', 'water'],
	['drank', 'chair'],
	['crumpled', 'river'],
	['shampooed', 'key'],
	['laundered', 'refrigerator'],
	['lit', 'rain'],
	['wore', 'way'],
	['built', 'rice'],
	['smelled', 'melody'],
	['scrubbed', 'thunder'],
	['cut', 'sand'],
	['spilled', 'moon'],
	['turned off', 'door'],
	['burnt', 'clouds'],
	['locked', 'ground'],
	['gathered', 'shower'],
	['swallowed', 'shed'],
	['cut', 'wine'],
	['woke up', 'steam'],
	['interviewed', 'vinegar'],
	['solved', 'peas'],
	['tied', 'chasm'],
	['tore', 'juice'],
	['broke', 'hat'],
	['washed', 'dance'],
	['planted', 'hammer'],
	['sewed', 'smoke'],
	['fried', 'shirt'],
	['dug', 'paint'],
	['cooked', 'stairs'],
	['turned up', 'bread'],
	['ironed', 'coffee'],
	['folded', 'diamond'],
	['shaved', 'glue']
	]

controls_a = [
	['heated', 'water'],
	['moved', 'chair'],
	['photographed', 'river'],
	['found', 'key'],
	['opened', 'refrigerator'],
	['drew', 'rain'],
	['looked for', 'way'],
	['cooked', 'rice'],
	['played', 'melody'],
	['heard', 'thunder'],
	['sprinkled', 'sand'],
	['photographed', 'moon'],
	['closed', 'door'],
	['saw', 'clouds'],
	['watered', 'ground'],
	['fixed up', 'shower'],
	['fixed up', 'shed'],
	['tasted', 'wine'],
	['wiped', 'steam'],
	['tasted', 'vinegar'],
	['threw', 'peas'],
	['found', 'chasm'],
	['wiped', 'juice'],
	['moved', 'hat'],
	['liked', 'dance'],
	['lifted', 'hammer'],
	['smelled', 'smoke'],
	['wore', 'shirt'],
	['looked for', 'paint'],
	['fixed', 'stairs'],
	['baked', 'bread'],
	['made', 'coffee'],
	['sold', 'diamond'],
	['spread', 'glue']
	]

controls_b = [
	['broke', 'glass'],
	['drank', 'chocolate milk'],
	['crumpled', 'paper'],
	['shampooed', 'hair'],
	['laundered', 'scarf'],
	['lit', 'lamp'],
	['wore', 'gloves'],
	['built', 'building'],
	['smelled', 'pie'],
	['scrubbed', 'house'],
	['cut', 'string'],
	['spilled', 'juice'],
	['turned off', 'computer'],
	['burnt', 'stew'],
	['locked', 'cabinet'],
	['gathered', 'dice'],
	['swallowed', 'food'],
	['cut', 'bread'],
	['woke up', 'child'],
	['interviewed', 'woman'],
	['solved', 'exercise'],
	['tied', 'shoelace'],
	['tore', 'map'],
	['broke', 'border'],
	['washed', 'floor'],
	['planted', 'plant'],
	['sewed', 'sock'],
	['fried', 'squash'],
	['dug', 'well'],
	['cooked', 'tomatoes'],
	['turned up', 'volume'],
	['ironed', 'clothes'],
	['folded', 'vest'],
	['shaved', 'head']
	]

fillers = [
	['baked', 'pie'],
	['cooked', 'tomatoes'],
	['imagined', 'well'],
	['imagined', 'border'],
	['lifted', 'Squash'],
	['changed', 'sock'],
	['changed', 'computer'],
	['prepared', 'stew'],
	['sweetened', 'juice'],
	['sweetened', 'chocolate milk'],
	['heard', 'sound'],
	['saw', 'light'],
	['sold', 'string'],
	['wore', 'vest'],
	['warmed up', 'food'],
	['threw', 'bread'],
	['watered', 'plant'],
	['fixed', 'gloves'],
	['liked', 'scarf'],
	['found', 'exercise'],
	['cleaned', 'house'],
	['cleaned', 'floor'],
	['drew', 'map'],
	['closed', 'cabinet'],
	['met', 'child'],
	['met', 'woman'],
	['scattered', 'dice'],
	['painted', 'building'],
	['colored', 'paper'],
	['smelled', 'hair'],
	['bought', 'clothes'],
	['bought', 'shoe lace'],
	['washed', 'cup'],
	['washed', 'head']
	]

violations_scores = []
controls_a_scores = []
controls_b_scores = []
fillers_scores = []

scores = [violations_scores, controls_a_scores, controls_b_scores, fillers_scores]

score_names = ['violations_scores', 'controls_a_scores', 'controls_b_scores', 'fillers_scores']

for i, condition in enumerate([violations, controls_a, controls_b, fillers]):	
	for pair in condition:
		scores[i].append(get_score(pair))

data = pd.DataFrame()

scores_flat = []

for i, x in enumerate(scores):
	for s in x:
		scores_flat.append([s[0], s[1], score_names[i], s[2]])

data['word1'] = 	[x[0] for x in scores_flat]
data['word2'] = 	[x[1] for x in scores_flat]
data['condition'] = [x[2] for x in scores_flat]
data['score'] = 	[float(x[3]) for x in scores_flat]

v = data.loc[data['condition'] == 'violations_scores']

a = data.loc[data['condition'] == 'controls_a_scores']

b = data.loc[data['condition'] == 'controls_b_scores']

f = data.loc[data['condition'] == 'fillers_scores']

print(v.describe())
print(a.describe())
print(b.describe())
print(f.describe())
