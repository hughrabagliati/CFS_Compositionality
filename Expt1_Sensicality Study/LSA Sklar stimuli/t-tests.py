import pandas as pd
import scipy.stats

#ngram scores
ngram_data = pd.read_csv('ngram_dataframe.csv', index_col=0)

scipy.stats.ttest_ind(ngram_data.loc[ngram_data['condition'] == 'violations']['ngram_score'],
	ngram_data.loc[ngram_data['condition'] == 'cont_a']['ngram_score'], equal_var=False)

scipy.stats.ttest_ind(ngram_data.loc[ngram_data['condition'] == 'violations']['ngram_score'],
	ngram_data.loc[ngram_data['condition'] == 'cont_b']['ngram_score'], equal_var=False)

scipy.stats.ttest_ind(ngram_data.loc[ngram_data['condition'] == 'cont_a']['ngram_score'],
	ngram_data.loc[ngram_data['condition'] == 'cont_b']['ngram_score'], equal_var=False)

scipy.stats.ttest_ind(ngram_data.loc[ngram_data['condition'] == 'violations']['ngram_score'],
	ngram_data[ngram_data['condition'].isin(['cont_a', 'cont_b'])]['ngram_score'], equal_var=False)


	#Violations vs control_a
	{'t-statistic': -1.5325884514389578,
	'pscore': 0.12574082490338087}

	#Violations vs control_b
	{'t-statistic': 0.043199768422555074,
	'pscore': 0.96555220221167282}

	#Control_a vs control_b
	{'t-statistic': 1.5539850177523924,
	'pscore': 0.12054835470028535}

	#Merged control_a/control_b vs violations
	{'t-statistic': -1.0685064517497855,
	'pscore': 0.285488974307336}

	#equal_var = False

#ngram scores
Violations vs control_a = (array(-1.5462776917298013), 0.12273508902395282)
Violations vs control_b = (array(0.0432047696408912), 0.96554821701818838)
Control_a vs control_b = (array(1.5539850177523924), 0.12088544594587128)
Violations vs merged control a/b = (array(-1.4896874256744124), 0.13662556291518219)



#phrase scores
phrase_data = pd.read_csv('phrase_dataframe.csv', index_col=0)

scipy.stats.ttest_ind(phrase_data.loc[phrase_data['condition'] == 'violations']['full_phrase_score'],
	phrase_data.loc[phrase_data['condition'] == 'cont_a']['full_phrase_score'], equal_var=False)

scipy.stats.ttest_ind(phrase_data.loc[phrase_data['condition'] == 'violations']['full_phrase_score'],
	phrase_data.loc[phrase_data['condition'] == 'cont_b']['full_phrase_score'], equal_var=False)

scipy.stats.ttest_ind(phrase_data.loc[phrase_data['condition'] == 'cont_a']['full_phrase_score'],
	phrase_data.loc[phrase_data['condition'] == 'cont_b']['full_phrase_score'], equal_var=False)

scipy.stats.ttest_ind(phrase_data.loc[phrase_data['condition'] == 'violations']['full_phrase_score'],
	phrase_data[phrase_data['condition'].isin(['cont_a', 'cont_b'])]['full_phrase_score'], equal_var=False)


	#Violations vs control_a
	{'t-statistic': -1.3649784935027272,
	'pscore': 0.1768950132514035}

	#Violations vs control_b
	{'t-statistic': -2.3859794115479973,
	'pscore': 0.019910632865915996}

	#Control_a vs control_b
	{'t-statistic': 1.2156887313271687,
	'pscore': 0.22843408057477124}

	#Violations vs merged control a/b
	{'t-statistic': -1.060664781729557,
	'pscore': 0.29139731434947869}

#phrase scores
Violations vs control_a = (array(-1.3649784935027272), 0.1814944436837613)
Violations vs control_b = (array(-2.3859794115479973), 0.022923201379099176)
Control_a vs control_b = (array(1.2156887313271687), 0.23266462077400421)
Violations vs merged control a/b = (array(-1.5037489160401154), 0.13734539601346304)


#word frequency data
freq_data = pd.read_csv('freq_dataframe.csv', index_col=0)

scipy.stats.ttest_ind(freq_data.loc[freq_data['condition'] == 'violations']['LogFreqBNC(Zipf)'],
	freq_data.loc[freq_data['condition'] == 'controlA']['LogFreqBNC(Zipf)'], equal_var=False)

scipy.stats.ttest_ind(freq_data.loc[freq_data['condition'] == 'violations']['LogFreqBNC(Zipf)'],
	freq_data.loc[freq_data['condition'] == 'controlB']['LogFreqBNC(Zipf)'], equal_var=False)

scipy.stats.ttest_ind(freq_data.loc[freq_data['condition'] == 'controlB']['LogFreqBNC(Zipf)'],
	freq_data.loc[freq_data['condition'] == 'controlA']['LogFreqBNC(Zipf)'], equal_var=False)

scipy.stats.ttest_ind(freq_data.loc[freq_data['condition'] == 'controlB']['LogFreqBNC(Zipf)'],
	freq_data[freq_data['condition'].isin(['controlA', 'controlB'])]['LogFreqBNC(Zipf)'], equal_var=False)

	#Violations vs control_a
	{'t-statistic': -1.8011659503570043,
	'pscore': 0.073976997230005476}

	#Violations vs control_b
	{'t-statistic': -0.35028553350053626,
	'pscore': 0.72666239473575456}

	#Control_a vs control_b
	{'t-statistic': -1.389542510802542,
	'pscore': 0.16700716016325182}

	#Violations vs merged control a/b
	{'t-statistic': -0.7669189376965669,
	'pscore': 0.44402511306353087}

#word frequency data
Violations vs control_a = (array(-1.8011721069635254), 0.073989220924834231)
Violations vs control_b = (array(-0.3504829516997592), 0.72651674291049284)
Control_a vs control_b = (array(-1.394430727115454), 0.16553040628497923)
Violations vs merged control a/b = (array(-0.7591683176145174), 0.44906382880433748)



#ngram scores
Violations vs control_a = (array(-1.5462776917298013), 0.12273508902395282)
Violations vs control_b = (array(0.0432047696408912), 0.96554821701818838)
Control_a vs control_b = (array(1.5539850177523924), 0.12088544594587128)
Violations vs merged control a/b = (array(-1.4896874256744124), 0.13662556291518219)

#phrase scores
Violations vs control_a = (array(-1.3649784935027272), 0.1814944436837613)
Violations vs control_b = (array(-2.3859794115479973), 0.022923201379099176)
Control_a vs control_b = (array(1.2156887313271687), 0.23266462077400421)
Violations vs merged control a/b = (array(-1.5037489160401154), 0.13734539601346304)

#word frequency data
Violations vs control_a = (array(-1.8011721069635254), 0.073989220924834231)
Violations vs control_b = (array(-0.3504829516997592), 0.72651674291049284)
Control_a vs control_b = (array(-1.394430727115454), 0.16553040628497923)
Violations vs merged control a/b = (array(-0.7591683176145174), 0.44906382880433748)


