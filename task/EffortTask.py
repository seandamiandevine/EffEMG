from psychopy import visual, core, event, monitors
from random import shuffle
import os
import datetime as dt
import pyfiglet
import pandas as pd
from fx.addOutput import addOutput, initCSV

def runTask(id, sex, age, _thisDir):
    """
    Calculation effort task based on Vasenna et al. (2021)
    fEMG triggers to be added for BIOPAC machine
    
    
    For question, contact Sean. 
    seandamiandevine@gmail.com
    """
    # Initialize datafile
    filename = _thisDir + os.sep + 'data' + os.sep + 'EMGEff_' + str(id)+'_'+str(dt.datetime.now()).replace(':','_')+'.csv'
    initCSV(filename, ['id', 'age', 'sex', 'cued', 'block', 'trial', 'efflev', 'rewlev', 'calc', 'corAns', 'option1', 'option2', 'option3', 'tstart', 
        'ITI1Time','probTime', 'respTime', 'selectTime', 'ITI2Time', 'fbTime', 'ITI3Time', 'key_press', 'chosenAns', 'RT', 'acc', 'feedback'])

    # Window setup
    winsize = [1920, 1080] if not id=='debug' else [1920//1.5, 1080//1.5]
    winfull = True if not id=='debug' else False
    winGUI  = False if not id=='debug' else True
    win     = visual.Window(size=winsize, fullscr=winfull,allowGUI=winGUI, allowStencil=False,monitor='testMonitor', color=[1,1,1], colorSpace='rgb',  blendMode='avg', useFBO=True, units='cm')

    # Set constants
    textCol      = [-1, -1, -1]                                              # font colour
    fontH        = 1                                                         # font height
    CueDf        = pd.read_csv('stim/cued.csv')                              # equations for non-cued condition 
    NoCueDf      = pd.read_csv('stim/nocued.csv')                            # sequations for cued condition
    pracCalc     = pd.read_csv('stim/train.csv')                             # practice stimulus list
    nBlocksNoCue = 3                                                         # number of blocks (breaks) in non-cued condition
    nBlocksCue   = 5                                                         # number of blocks (breaks) in cued condition
    nTrialsNoCue = 2 if id=='debug' else len(NoCueDf)//nBlocksNoCue          # number of trials per block in non-cued condition
    nTrialsCue   = 2 if id=='debug' else len(CueDf)//nBlocksCue              # number of trials per block in cued condition
    nPract       = 2 if id=='debug' else len(pracCalc)                       # number of practice trials 
    choiceKeys   = ['q', 'w', 'e']                                           # keys to make choices
    cueTime      = 2                                                         # amount of time effort cue stays up (in s. )
    ITI          = 1.5                                                       # amount of time fixation cross stays up (in s.)
    probTime     = 5                                                         # amount of time calculation stays up (in s.)
    respTime     = 5                                                         # amount of time response options stay up (in s.)
    fbTime       = 1                                                         # amount of time feedback stays up (in s. )
    stimDir      = 'stim/'                                                   # directory where stimuli are located
    instDir      = 'instructions/'                                           # directory where instructions are located 

    # initialize instructions
    insts      = os.listdir(instDir) 
    instPic    = visual.ImageStim(win, image="instructions/1.png")
    
    # initialize trial components
    trialClock = core.Clock()
    effCue     = visual.ImageStim(win, image="{}eff1.png".format(stimDir))
    txt        = visual.TextStim(win, text="", height=3*fontH, color=textCol, pos=[0, 0], wrapWidth=30)
    opt1       = visual.TextStim(win, text="", height=3*fontH, color=textCol, pos=[-8, 0])
    opt2       = visual.TextStim(win, text="", height=3*fontH, color=textCol, pos=[0, 0])
    opt3       = visual.TextStim(win, text="", height=3*fontH, color=textCol, pos=[8, 0])
    endScreen  = visual.TextStim(win, text="", height=fontH,   color=textCol, pos=[0, 0], wrapWidth=30)
    rating     = visual.Slider(win, ticks=[1,2,3], pos=[0,0], color='black')

    #--------------------------------------Start Task-----------------------------------------
    #win.mouseVisible=True
    # print(rating.getRating())

    # while not rating.rating:
    #     rating.draw()
    #     win.flip()

    # Instructions A
    instCounter = 1
    while instCounter<5:
        instPic.image = '{}{}.png'.format(instDir,instCounter)
        instPic.draw()
        win.flip()
        key_press  = event.waitKeys(keyList=['left', 'right'])
        if key_press[0]=='left':
            instCounter -= 1
            if instCounter<1: instCounter=1
        else:
            instCounter += 1


    # Practice phase
    win.flip()
    core.wait(ITI)
    for pt in range(nPract):
        startTime = trialClock.getTime()

        # 1. Calculation
        calcTime = trialClock.getTime()
        effLev   = pracCalc['Effort Code'].iloc[pt]
        nums     = pracCalc[['num1', 'num2', 'num3', 'num4', 'num5']].loc[pt,:]
        thisCalc = ''.join([str(n) for n in nums.values])
        txt.text = thisCalc
        txt.draw()
        win.flip()
        core.wait(probTime)
        win.flip()

        # 2. Response 
        choiceTime = trialClock.getTime() 
        corAns     = pracCalc.Correct.iloc[pt]
        options    = [corAns, pracCalc.Wrong_1.iloc[pt], pracCalc.Wrong_2.iloc[pt]]
        shuffle(options) 
        opt1.text, opt2.text, opt3.text = [str(o) for o in options]
        opt1.draw()
        opt2.draw()
        opt3.draw()
        win.flip()
        key_press  = event.waitKeys(keyList=choiceKeys)
        chosenTime = trialClock.getTime()
        if not key_press:
            response    = 'NA'
            givenAnswer = 'NA' 
            RT          = 'NA'
            acc         = 0
        else:
            response    = key_press[0]
            choiceIdx   = choiceKeys.index(response)
            givenAnswer = options[choiceIdx]
            acc         = 1 if givenAnswer==corAns else 0
            RT          = chosenTime - choiceTime

        # 5. ITI 2
        ITI2Time = trialClock.getTime()
        txt.text = '+'
        txt.draw()
        win.flip()
        core.wait(ITI)
        win.flip()

        # 6. Feedback
        feedbackTime = trialClock.getTime()
        fb       = 'Correct' if acc==1 else 'Incorrect'
        txt.text = fb
        txt.draw()
        win.flip()
        core.wait(fbTime)
        win.flip()

        # 7. ITI 3
        ITI3Time = trialClock.getTime()
        txt.text = '+'
        txt.draw()
        win.flip()
        core.wait(ITI)
        win.flip()

        # Save. 
        out = [id, age, sex, 0, 'Practice', pt, effLev, 0, thisCalc, corAns, options[0], options[1], options[2], startTime,
                'NA', calcTime, choiceTime, chosenTime, ITI2Time, feedbackTime, ITI3Time, response, givenAnswer, RT, 
                acc, fb]
        addOutput(filename, out)

    # Instructions B
    while instCounter<=5:
        instPic.image = '{}{}.png'.format(instDir, instCounter)
        instPic.draw()
        win.flip()
        key_press  = event.waitKeys(keyList=['left', 'right'])
        if key_press[0]=='left':
            instCounter -= 1
            if instCounter<1: instCounter=1
        else:
            instCounter += 1
            print(instCounter)

    # Test trials 
    # No cue phase
    effList = NoCueDf.sample(frac=1).reset_index(drop=True)
    for b in range(nBlocksNoCue):
        if b > 0:
            txt.height = fontH
            txt.text   = 'You can now take a small break.\n\nIf you have any questions, please notify the experimenter.\n\nWhen you are ready to continue with the experiment, press SPACE.'
            txt.draw()
            win.flip()
            key_press  = event.waitKeys(keyList=['space'])
            win.flip()
            txt.height = 3*fontH
            txt.text   = '+'
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()
        for t in range(nTrialsNoCue):
            startTime = trialClock.getTime()

            # 1. Calculation
            calcTime = trialClock.getTime()
            nums     = effList[['num1', 'num2', 'num3', 'num4', 'num5']].loc[t,:]
            thisCalc = ''.join([str(n) for n in nums.values])
            txt.text = thisCalc
            txt.draw()
            win.flip()
            core.wait(probTime)
            win.flip()

            # 2. Response 
            choiceTime = trialClock.getTime() 
            corAns     = effList.Correct.iloc[t]
            options    = [corAns, effList.Wrong_1.iloc[t], effList.Wrong_2.iloc[t]]
            shuffle(options) 
            opt1.text, opt2.text, opt3.text = [str(o) for o in options]
            opt1.draw()
            opt2.draw()
            opt3.draw()
            win.flip()
            key_press  = event.waitKeys(keyList = choiceKeys, maxWait=respTime)
            chosenTime = trialClock.getTime()
            if not key_press:
                response    = 'NA'
                givenAnswer = 'NA' 
                RT          = 'NA'
                acc         = 0
            else:
                response    = key_press[0]
                choiceIdx   = choiceKeys.index(response)
                givenAnswer = options[choiceIdx]
                acc         = 1 if givenAnswer==corAns else 0
                RT          = chosenTime - choiceTime

            # 3. ITI 2
            ITI2Time = trialClock.getTime()
            txt.text = '+'
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # 4. Feedback
            feedbackTime = trialClock.getTime()
            if response=='NA':
                fb = 'Too Slow!'
            elif acc==1:
                fb = 'Correct'
            elif acc==0:
                fb = 'Incorrect'
            txt.text = fb
            txt.draw()
            win.flip()
            core.wait(fbTime)
            win.flip()

            # 5. ITI 3
            ITI3Time = trialClock.getTime()
            txt.text = '+'
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # Save output
            out = [id, age, sex, 0, b, t, effLev, 0, thisCalc, corAns, options[0], options[1], options[2], startTime,
                'NA', calcTime, choiceTime, chosenTime, ITI2Time, feedbackTime, ITI3Time, response, givenAnswer, RT, 
                acc, fb]
            addOutput(filename, out)

    # Instructions C
    while instCounter<len(insts):
        instPic.image = '{}{}.png'.format(instDir,instCounter)
        instPic.draw()
        win.flip()
        key_press  = event.waitKeys(keyList=['left', 'right'])
        if key_press[0]=='left':
            instCounter -= 1
            if instCounter<5: instCounter=5
        else:
            instCounter += 1

    # Test trials
    # Cued phase
    effList = CueDf.sample(frac=1).reset_index(drop=True)
    for b in range(nBlocksCue):
        if b > 0:
            txt.height = fontH
            txt.text   = 'You can now take a small break.\n\nIf you have any questions, please notify the experimenter.\n\nWhen you are ready to continue with the experiment, press SPACE.'
            txt.draw()
            win.flip()
            key_press  = event.waitKeys(keyList=['space'])
            win.flip()
            txt.height = 3*fontH
            txt.text   = '+'
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()
        for t in range(nTrialsCue):
            startTime = trialClock.getTime()
            
            # 1. Display effort cue
            effCueTime   = trialClock.getTime()
            effLev       = effList['Effort Code'].iloc[t]
            effCue.image = '{}eff{}.png'.format(stimDir, effLev)
            effCue.draw()
            win.flip()
            core.wait(cueTime)
            win.flip()

            # 2. ITI 1
            ITI1Time = trialClock.getTime()
            txt.text = '+'
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # 3. Calculation
            calcTime = trialClock.getTime()
            nums     = effList[['num1', 'num2', 'num3', 'num4', 'num5']].loc[t,:]
            thisCalc = ''.join([str(n) for n in nums.values])
            txt.text = thisCalc
            txt.draw()
            win.flip()
            core.wait(probTime)
            win.flip()

            # 4. Response 
            choiceTime = trialClock.getTime() 
            corAns     = effList.Correct.iloc[t]
            options    = [corAns, effList.Wrong_1.iloc[t], effList.Wrong_2.iloc[t]]
            shuffle(options) 
            opt1.text, opt2.text, opt3.text = [str(o) for o in options]
            opt1.draw()
            opt2.draw()
            opt3.draw()
            win.flip()
            key_press  = event.waitKeys(keyList = choiceKeys, maxWait=respTime)
            chosenTime = trialClock.getTime()
            if not key_press:
                response    = 'NA'
                givenAnswer = 'NA' 
                RT          = 'NA'
                acc         = 0
            else:
                response    = key_press[0]
                choiceIdx   = choiceKeys.index(response)
                givenAnswer = options[choiceIdx]
                acc         = 1 if givenAnswer==corAns else 0
                RT          = chosenTime - choiceTime

            # 5. ITI 2
            ITI2Time = trialClock.getTime()
            txt.text = '+'
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # 6. Feedback
            feedbackTime = trialClock.getTime()
            if response=='NA':
                fb = 'Too Slow!'
            elif acc==1:
                fb = 'Correct'
            elif acc==0:
                fb = 'Incorrect'
            txt.text = fb
            txt.draw()
            win.flip()
            core.wait(fbTime)
            win.flip()

            # 7. ITI 3
            ITI3Time = trialClock.getTime()
            txt.text = '+'
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # Save output
            out = [id, age, sex, 1, b, t, effLev, 0, thisCalc, corAns, options[0], options[1], options[2], startTime,
                ITI1Time, calcTime, choiceTime, chosenTime, ITI2Time, feedbackTime, ITI3Time, response, givenAnswer, RT, 
                acc, fb]
            addOutput(filename, out)

    # Show end screen
    endScreen.text='Thank you for completing our study!\n\nSee the experimenter for further details.'
    endScreen.draw()
    win.flip()
    event.waitKeys(keyList = ['escape'])

# Run task
os.system('clear')
print(pyfiglet.figlet_format("EMG EFFORT STUDY"))
# id  = input('Please enter the SUBJECT ID NUMBER: ')
# age = input("Please enter the subject's AGE: ")
# sex = input("Please enter the subject's SEX: ")
id  = 'debug'
age = 25
sex = 'M'
print('Good luck!')

runTask(id, str(sex), str(age), os.getcwd())
