import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

data = pd.read_csv('SUBTLEX-UK.csv', low_memory=False)

data = data.set_index('Spelling')

viol = ['I broke the water',
 'I drank the chair',
 'I crumpled the river',
 'I shampooed the key',
 'I laundered the refrigerator',
 'I lit the rain',
 'I wore the way',
 'I built the rice',
 'I smelled the melody',
 'I scrubbed the thunder',
 'I cut the sand',
 'I spilled the moon',
 'I turned off the door',
 'I burnt the clouds',
 'I locked the ground',
 'I gathered the shower',
 'I swallowed the shed',
 'I cut the wine',
 'I woke up the steam',
 'I interviewed the vinegar',
 'I solved the peas',
 'I tied the chasm',
 'I tore the juice',
 'I broke the hat',
 'I washed the dance',
 'I planted the hammer',
 'I sewed the smoke',
 'I fried the shirt',
 'I dug the paint',
 'I cooked the stairs',
 'I turned up the bread',
 'I ironed the coffee',
 'I folded the diamond',
 'I shaved the glue']

contA = ['I heated the water',
 'I moved the chair',
 'I photographed the river',
 'I found the key',
 'I opened the refrigerator',
 'I drew the rain',
 'I looked for the way',
 'I cooked the rice',
 'I played the melody',
 'I heard the thunder',
 'I sprinkled the sand',
 'I photographed the moon',
 'I closed the door',
 'I saw the clouds',
 'I watered the ground',
 'I fixed up the shower',
 'I fixed up the shed',
 'I tasted the wine',
 'I wiped the steam ',
 'I tasted the vinegar',
 'I threw the peas',
 'I found the chasm',
 'I wiped the juice',
 'I moved the hat',
 'I liked the dance',
 'I lifted the hammer',
 'I smelled the smoke',
 'I wore the shirt',
 'I looked for the paint',
 'I fixed the stairs',
 'I baked the bread',
 'I made the coffee',
 'I sold the diamond',
 'I spread the glue']

contB = ['I broke the glass',
 'I drank the chocolate milk',
 'I crumpled the paper',
 'I shampooed the hair',
 'I laundered the scarf',
 'I lit the lamp',
 'I wore the gloves',
 'I built the building',
 'I smelled the pie',
 'I scrubbed the house',
 'I cut the string',
 'I spilled the juice',
 'I turned off the computer',
 'I burnt the stew',
 'I locked the cabinet',
 'I gathered the dice',
 'I swallowed the food',
 'I cut the bread',
 'I woke up the child',
 'I interviewed the woman',
 'I solved the exercise',
 'I tied the shoelace',
 'I tore the map',
 'I broke the border',
 'I washed the floor',
 'I planted the plant',
 'I sewed the sock',
 'I fried the squash',
 'I dug the well',
 'I cooked the tomatoes',
 'I turned up the volume',
 'I ironed the clothes',
 'I folded the vest',
 'I shaved the head']

fill = ['I baked the pie',
 'I cooked the tomatoes',
 'I imagined the well',
 'I imagined the border',
 'I lifted the squash',
 'I changed the sock',
 'I changed the computer',
 'I prepared the stew',
 'I sweetened the juice',
 'I sweetened the chocolate milk ',
 'I heard the sound',
 'I saw the light',
 'I sold the string',
 'I wore the vest',
 'I warmed up the food',
 'I threw the bread',
 'I watered the plant',
 'I fixed the gloves',
 'I liked the scarf',
 'I found the exercise',
 'I cleaned the house',
 'I cleaned the floor',
 'I drew the map',
 'I closed the cabinet',
 'I met the child',
 'I met the woman',
 'I scattered the dice',
 'I painted the building',
 'I colored the paper',
 'I smelled the hair',
 'I bought the clothes',
 'I bought the shoe lace',
 'I washed the cup',
 'I washed the head']

words = {
	"violations":set(),
	"controlA":set(),
	"controlB":set(),
	"fillers":set(),
}

for s in viol:
	for w in s.split():
		words["violations"].add(w)

for s in cont_a:
	for w in s.split():
		words["controlA"].add(w)

for s in cont_b:
	for w in s.split():
		words["controlB"].add(w)

for s in fill:
	for w in s.split():
		words["fillers"].add(w)

freq_data = pd.DataFrame()

for k,v in words.items():
	temp = pd.DataFrame(data.loc[list(v), :]['LogFreqBNC(Zipf)'])
	temp['condition'] = [k,]*len(temp)

	freq_data = pd.concat([freq_data, temp])

freq_data = freq_data.sort("condition")

freq_data.to_csv('freq_dataframe.csv')

graph = sns.factorplot(
	x='condition', 
	y="LogFreqBNC(Zipf)", 
	data=freq_data,
	size=6, 
	kind="bar", 
	palette="muted")

graph.despine(left=True)
graph.axes.flat[0].set_title('Median word frequency per condition')

#Saved
# Now do all the ngram stats

import json

with open('data.json', 'r') as file:
	all_stats = json.load(file)

ngram_data = pd.DataFrame()
phrase_data = pd.DataFrame()

for k, v in all_stats.items():
	for stim in v:
		index_to_use = [i[0] for i in stim['bigram_permutation_scores']]

		temp = pd.DataFrame([i[1] for i in stim['bigram_permutation_scores']], columns=['ngram_score'])
		temp['condition'] = [k,] * len(temp)
		temp.index = index_to_use

		ngram_data = pd.concat([ngram_data, temp])

		temp = pd.DataFrame([stim['full_phrase_score'][1]], columns=['full_phrase_score'])
		index_to_use = [stim['full_phrase_score'][0],]
		temp['condition'] = k
		temp.index = index_to_use

		phrase_data = pd.concat([phrase_data, temp])

phrase_data = phrase_data.reset_index()

phrase_data.drop('index', axis=1, inplace=True)

phrase_data = phrase_data.sort("condition")

ngram_data = ngram_data.reset_index()

ngram_data.drop('index', axis=1, inplace=True)

ngram_data = ngram_data.sort("condition")

ngram_data.to_csv('ngram_dataframe.csv')
phrase_data.to_csv('phrase_dataframe.csv')

#Draw graphs

ngram_graph = sns.factorplot(
	x='condition', 
	y="ngram_score", 
	data=ngram_data,
	size=6, 
	kind="bar", 
	palette="muted")

ngram_graph.despine(left=True)

ngram_graph.axes.flat[0].set_title('Frequency of permuted bigrams within stimuli phrase per condition')

plt.show()

phrase_graph = sns.factorplot(
	x='condition', 
	y="full_phrase_score", 
	data=phrase_data,
	size=6, 
	kind="bar", 
	palette="muted")

phrase_graph.despine(left=True)

phrase_graph.axes.flat[0].set_title('Frequency of whole stimuli phrase per condition')

plt.show()












