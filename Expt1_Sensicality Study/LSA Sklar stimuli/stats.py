from pandas import DataFrame
import re
import time
import pickle
import requests
from sklearn import metrics
from itertools import combinations

corpora = dict(eng_us_2012=17, eng_us_2009=5, eng_gb_2012=18, eng_gb_2009=6,
               chi_sim_2012=23, chi_sim_2009=11, eng_2012=15, eng_2009=0,
               eng_fiction_2012=16, eng_fiction_2009=4, eng_1m_2009=1,
               fre_2012=19, fre_2009=7, ger_2012=20, ger_2009=8, heb_2012=24,
               heb_2009=9, spa_2012=21, spa_2009=10, rus_2012=25, rus_2009=12,
               ita_2012=22)

def getNgrams(query, corpus='eng_2012', startYear=1800, endYear=2015, smoothing=3, caseInsensitive=True):
    params = dict(
        content=query, 
        year_start=startYear, 
        year_end=endYear,
        corpus=corpora[corpus], 
        smoothing=smoothing,
        case_insensitive=caseInsensitive)

    req = requests.get('http://books.google.com/ngrams/graph', params=params)
    res = re.findall('var data = (.*?);\\n', req.text)
    if res:
        data = {qry['ngram']: qry['timeseries']
                for qry in literal_eval(res[0])}
        df = DataFrame(data)
        #df.insert(0, 'year', list(range(startYear, endYear + 1)))
    else:
        df = DataFrame.from_dict({query:[0,0]}, orient='columns')
 
    return df

def areas_under_graphs(df):
    if type(df) == list:
        return df
    x = [i for i in range(len(df))]
    scores = []
    for c in df.columns:
        y = df[c]
        scores.append([c, metrics.auc(x, y, reorder=False)])        
    return scores

def bigrams(sentence):
    sentence = sentence.split()
    store = []
    current_index = 0
    for i in range(0,len(sentence)-1):
        store.append(sentence[i:current_index+2])
        current_index += 1
    return store

def bigram_permutations(sentence):
    store = []
    sentence = sentence.lower().split()
    store.extend(permutations(sentence, 2))
    return [i for i in store if i[0] != i[1]]

def process_stimuli(stim_list):
    queries = []

    for i, s in enumerate(stim_list):
        print('Doing', i, 'of', len(v), ':', s)
        
        data = {}
        
        full_score = areas_under_graphs(getNgrams(s))

        if full_score:
            data['full_phrase_score'] = full_score[0]
        else:
            data['full_phrase_score'] = [s, 0.0]
        
        data['bigram_permutations'] = [" ".join(i) for i in bigram_permutations(s)]

        data['bigram_permutation_scores'] = []

        print('\t', len(data['bigram_permutations']), 'combos to check')

        for combo in data['bigram_permutations']:
            print('\t\tChecking:', combo)
            
            time.sleep(10)
            
            ngram_data = getNgrams(combo)
            
            if ngram_data.shape[1] == 0:
                #print('Shape is 0')
                data['bigram_permutation_scores'].append([combo, 0.0])
                #print(data['bigram_permutation_scores'])
            
            if ngram_data.shape[1] == 1:
                #print('Shape is 1')
                data['bigram_permutation_scores'].append(areas_under_graphs(ngram_data)[0])
                #print(data['bigram_permutation_scores'])

            if ngram_data.shape[1] > 1:
                #print('Shape is 1+')
                for c in ngram_data.columns:
                    if not c.endswith('(All)'):
                        ngram_data.drop(c, axis=1, inplace=True)
                data['bigram_permutation_scores'].extend(areas_under_graphs(ngram_data))
                #print(data['bigram_permutation_scores'])

        queries.append(data)
        #print(data)

    return queries

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

cont_a = ['I heated the water',
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

cont_b = ['I broke the glass',
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
 'I lifted the Squash',
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

tasks = [("violations", viol), ("cont_a", cont_a), ("cont_b", cont_b), ("fillers", fill)]

all_stats = {}

for t in tasks:
    all_stats[t[0]] = process_stimuli(t[1])
    time.sleep(360)


def compile_stats(condition):
    phrase_scores = []
    
    bigram_scores_per_stimuli = []

    for x in condition:
        phrase_scores.append(x['full_phrase_score'][1])

        bigram_scores_per_stimuli.extend(x['bigram_permutation_scores'])

    phrase_scores = pd.DataFrame(phrase_scores)

    bigram_scores_per_stimuli = pd.DataFrame(bigram_scores_per_stimuli)

    return {'phrase_scores':phrase_scores.describe(), 'bigram_scores':bigram_scores_per_stimuli.describe()}


descriptive_stats = {}

tasks2 = ["violations", "cont_a", "cont_b", "fillers"]

for t in tasks2:
    descriptive_stats[t] = compile_stats(all_stats[t])







