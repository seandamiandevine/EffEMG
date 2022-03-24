import pandas as pd
from random import seed

seed(2022)

def stimGen(stim, n, efflevs=[1,2,3,4]):
	if not (n/len(efflevs)).is_integer(): 
		print('Does not divide into four even groupings of effort levels. Choose another value.')
		return
	nn  = int(n/len(efflevs))
	out = []
	for e in efflevs:
		out.append(stim[stim['Effort Code']==e].sample(n=nn).reset_index(drop=True))

	out = pd.concat(out)
	out.to_csv('test.csv')

stim   = pd.read_csv('all_test.csv')
n      = int(input('How many trials do you want (all cued)? '))
stimGen(stim, n)