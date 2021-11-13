import pandas as pd
import numpy as np

# Desired constants
# Both values should be divisible by number of effort levels
def checkDivis(Ncued, Neff=4, availableSamples=220):
    """
    Check whether the proposed number of cued trials works
    """
    x = Ncued//3
    y = x/Neff
    z = Ncued/Neff
    if y%1==0 and z%1==0:
        if Ncued+x < availableSamples:
            return True
    return False

#for i in range(0, 220):
#    Ncued  = i
#    Nnocue = Ncued//3
#    print('Do {} cued trials and {} not cued trials work? {}'.format(Ncued, Nnocue, checkDivis(Ncued)))

Ncued  = 144
Nnocue = Ncued//3
print('Total number of trials = {}'.format(Ncued+Nnocue))

# Load all stim data
stim    = pd.read_csv('all_test.csv')
effLevs = pd.unique(stim['Effort Code'])

# Split not cued trials
nEach = Nnocue//len(effLevs)

dfs = []
for level in effLevs:
    tmp = stim[stim['Effort Code']==level]
    tmp = tmp.sample(n=nEach)
    dfs.append(tmp)
    stim = stim[~stim.index.isin(tmp.index)]
nocued = pd.concat(dfs).reset_index(drop=True)
nocued.to_csv('nocued_{}.csv'.format(Nnocue))

# Split cued trials
nEach = Ncued//len(effLevs)

dfs = []
for level in effLevs:
    tmp = stim[stim['Effort Code']==level]
    tmp = tmp.sample(n=nEach)
    dfs.append(tmp)
    stim = stim[~stim.index.isin(tmp.index)]
   
cued = pd.concat(dfs).reset_index(drop=True)
cued.to_csv('cued_{}.csv'.format(Ncued))
