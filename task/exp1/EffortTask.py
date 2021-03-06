from psychopy import visual, core, event, monitors
from random import shuffle
import os
import datetime as dt
import pyfiglet
import pandas as pd
import serial
from fx.addOutput import addOutput, initCSV


def runTask(id, sex, age, _thisDir=os.getcwd(), behav=False):
    """
    Calculation effort task based on Vasenna et al. (2021)
    fEMG triggers to be added for BIOPAC machine

    @id:       subject idea
    @sex:      subject sex
    @age:      subject age
    @_thisDir: location of this script, data folder,  stimuli, and instructions

    For question, contact Sean.
    seandamiandevine@gmail.com
    """

    def sendTrigger(ser, i:int, delay=0.01):
        """
        Send a trigger of specified integer value.
        See this link for help: http://web.cecs.pdx.edu/~harry/compilers/ASCIIChart.pdf.

        @i: integer value for a trigger

        (ser has to be a global variable)
        """
        trigger = format(i, '02x').upper().encode()
        if behav:
            # don't send trigger if it is behavioural
            print(trigger)
        else:
            ser.write(trigger)
            ser.flush()
            core.wait(delay)
            ser.write('00'.encode())
            ser.flush()

    # Initialize datafile
    filename = _thisDir + os.sep + 'data' + os.sep + 'EMGEff_' + str(id)+'_'+str(dt.datetime.now()).replace(':','_')+'.csv'
    initCSV(filename, ['id', 'age', 'sex', 'cued', 'block', 'condtrial' ,'trialinblock', 'efflev', 'rewlev', 'calc', 'corAns', 'option1', 'option2', 'option3', 'tstart',
        'ITI1Time','probTime', 'respTime', 'selectTime', 'ITI2Time', 'fbTime', 'ITI3Time', 'key_press', 'chosenAns', 'RT', 'acc', 'feedback'])

    # Window setup
    winsize = [1920, 1080] if not id=='debug' else [1920//1.5, 1080//1.5]
    winfull = True if not id=='debug' else False
    winGUI  = False if not id=='debug' else True
    win     = visual.Window(size=winsize, fullscr=winfull,allowGUI=winGUI, allowStencil=False,monitor='testMonitor', color=[1,1,1], colorSpace='rgb',  blendMode='avg', useFBO=True, units='cm')

    # Set constants
    textCol      = [-1, -1, -1]                                              # font colour
    fontH        = 1                                                         # font height
    CueDf        = pd.read_csv('stim/cued_144.csv')                          # equations for cued condition
    NoCueDf      = pd.read_csv('stim/nocued_48.csv')                         # sequations for non-cued condition
    pracCalc     = pd.read_csv('stim/train.csv')                             # practice stimulus list
    nBlocksNoCue = 3                                                         # number of blocks (breaks) in non-cued condition
    nBlocksCue   = 6                                                         # number of blocks (breaks) in cued condition
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
    TLXcsv       = '{}TLX.csv'.format(stimDir)                               # csv file containing TLX questions and anchors
    TLXdims      = ['mental_demand', 'effort']                               # which TLX dimensions to ask
    trigList     = pd.read_csv('EffEMG_trigs.csv')

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
    TLXRating  = visual.RatingScale(win, low=0, high=10, markerStart=5, textColor='black', scale=None, lineColor='black', markerColor='DarkRed', labels=['Very Low', 'Very High'])

    #--------------------------------------Start Task-----------------------------------------
    win.mouseVisible=True if id=='debug' else False

    thisTrig = trigList.Code[trigList.TriggerName=='taskStart'].iloc[0]
    sendTrigger(ser, thisTrig)

    # Instructions A
    thisTrig = trigList.Code[trigList.TriggerName=='instructions'].iloc[0]
    instCounter = 1
    while instCounter<5:
        instPic.image = '{}{}.png'.format(instDir,instCounter)
        win.callOnFlip(sendTrigger, ser, thisTrig)
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
        thisTrig = trigList.Code[trigList.TriggerName=='pracProb{}'.format(effLev)].iloc[0]
        win.callOnFlip(sendTrigger, ser, thisTrig)
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
        thisTrig   = trigList.Code[trigList.TriggerName=='pracAns{}'.format(effLev)].iloc[0]
        win.callOnFlip(sendTrigger, ser, thisTrig)
        opt1.draw()
        opt2.draw()
        opt3.draw()
        win.flip()
        thisTrig   = trigList.Code[trigList.TriggerName=='pracResp{}'.format(effLev)].iloc[0]
        key_press  = event.waitKeys(keyList=choiceKeys)
        sendTrigger(ser, thisTrig)
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
        thisTrig = trigList.Code[trigList.TriggerName=='pracITI2'].iloc[0]
        win.callOnFlip(sendTrigger, ser, thisTrig)
        txt.draw()
        win.flip()
        core.wait(ITI)
        win.flip()

        # 6. Feedback
        feedbackTime = trialClock.getTime()
        if acc==1:
            fb       = 'Correct'
            thisTrig = trigList.Code[trigList.TriggerName == 'pracFbCor{}'.format(effLev)].iloc[0]
        else:
            fb       = 'Incorrect'
            thisTrig = trigList.Code[trigList.TriggerName == 'pracFbIncor{}'.format(effLev)].iloc[0]
        txt.text = fb
        win.callOnFlip(sendTrigger, ser, thisTrig)
        txt.draw()
        win.flip()
        core.wait(fbTime)
        win.flip()

        # 7. ITI 3
        ITI3Time = trialClock.getTime()
        txt.text = '+'
        thisTrig = trigList.Code[trigList.TriggerName=='pracITI3'].iloc[0]
        win.callOnFlip(sendTrigger, ser, thisTrig)
        txt.draw()
        win.flip()
        core.wait(ITI)
        win.flip()

        # Save.
        out = [id, age, sex, 0, 'Practice', 'NA', pt, effLev, 0, thisCalc, corAns, options[0], options[1], options[2], startTime,
                'NA', calcTime, choiceTime, chosenTime, ITI2Time, feedbackTime, ITI3Time, response, givenAnswer, RT,
                acc, fb]
        addOutput(filename, out)

    # Instructions B
    thisTrig = trigList.Code[trigList.TriggerName=='instructions'].iloc[0]
    while instCounter<=5:
        instPic.image = '{}{}.png'.format(instDir, instCounter)
        win.callOnFlip(sendTrigger, ser, thisTrig)
        instPic.draw()
        win.flip()
        key_press  = event.waitKeys(keyList=['left', 'right'])
        if key_press[0]=='left':
            instCounter -= 1
            if instCounter<1: instCounter=1
        else:
            instCounter += 1

    # Test trials
    # No cue phase
    effList = NoCueDf.sample(frac = 1).reset_index(drop = True)
    tCount  = 0 # total trial counter
    for b in range(nBlocksNoCue):
        if b > 0:
            thisTrig = trigList.Code[trigList.TriggerName=='blockScreen'].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
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
            effLev   = effList['Effort Code'].iloc[tCount]
            nums     = effList[['num1', 'num2', 'num3', 'num4', 'num5']].loc[tCount,:]
            thisCalc = ''.join([str(n) for n in nums.values])
            txt.text = thisCalc
            thisTrig = trigList.Code[trigList.TriggerName=='noCueProb{}'.format(effLev)].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            core.wait(probTime)
            win.flip()

            # 2. Response
            choiceTime = trialClock.getTime()
            corAns     = effList.Correct.iloc[tCount]
            options    = [corAns, effList.Wrong_1.iloc[tCount], effList.Wrong_2.iloc[tCount]]
            shuffle(options)
            opt1.text, opt2.text, opt3.text = [str(o) for o in options]
            thisTrig = trigList.Code[trigList.TriggerName=='noCueAns{}'.format(effLev)].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            opt1.draw()
            opt2.draw()
            opt3.draw()
            win.flip()
            thisTrig   = trigList.Code[trigList.TriggerName=='noCueResp{}'.format(effLev)].iloc[0]
            key_press  = event.waitKeys(keyList = choiceKeys, maxWait=respTime)
            sendTrigger(ser, thisTrig)
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
            thisTrig = trigList.Code[trigList.TriggerName=='noCueITI2'].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # 4. Feedback
            feedbackTime = trialClock.getTime()
            if response=='NA':
                fb = 'Too Slow!'
                thisTrig = trigList.Code[trigList.TriggerName=='noCueFbSlow'].iloc[0]
            elif acc==1:
                fb = 'Correct'
                thisTrig = trigList.Code[trigList.TriggerName=='noCueFbCor{}'.format(effLev)].iloc[0]
            elif acc==0:
                fb = 'Incorrect'
                thisTrig = trigList.Code[trigList.TriggerName=='noCueFbIncor{}'.format(effLev)].iloc[0]
            txt.text = fb
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            core.wait(fbTime)
            win.flip()

            # 5. ITI 3
            ITI3Time = trialClock.getTime()
            txt.text = '+'
            thisTrig = trigList.Code[trigList.TriggerName=='noCueITI3'].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # Save output
            out = [id, age, sex, 0, b, tCount, t, effLev, 0, thisCalc, corAns, options[0], options[1], options[2], startTime,
                'NA', calcTime, choiceTime, chosenTime, ITI2Time, feedbackTime, ITI3Time, response, givenAnswer, RT,
                acc, fb]
            addOutput(filename, out)
            tCount+=1

    # Instructions C
    thisTrig = trigList.Code[trigList.TriggerName=='instructions'].iloc[0]
    while instCounter<len(insts):
        instPic.image = '{}{}.png'.format(instDir,instCounter)
        win.callOnFlip(sendTrigger, ser, thisTrig)
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
    tCount  = 0
    for b in range(nBlocksCue):
        if b > 0:
            txt.height = fontH
            txt.text   = 'You can now take a small break.\n\nIf you have any questions, please notify the experimenter.\n\nWhen you are ready to continue with the experiment, press SPACE.'
            thisTrig = trigList.Code[trigList.TriggerName=='blockScreen'].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
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
            effLev       = effList['Effort Code'].iloc[tCount]
            effCue.image = '{}eff{}.png'.format(stimDir, effLev)
            thisTrig     = trigList.Code[trigList.TriggerName=='cue{}'.format(effLev)].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            effCue.draw()
            win.flip()
            core.wait(cueTime)
            win.flip()

            # 2. ITI 1
            ITI1Time = trialClock.getTime()
            txt.text = '+'
            thisTrig     = trigList.Code[trigList.TriggerName=='cueITI1'].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # 3. Calculation
            calcTime = trialClock.getTime()
            nums     = effList[['num1', 'num2', 'num3', 'num4', 'num5']].loc[tCount,:]
            thisCalc = ''.join([str(n) for n in nums.values])
            txt.text = thisCalc
            thisTrig = trigList.Code[trigList.TriggerName=='cueProb{}'.format(effLev)].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            core.wait(probTime)
            win.flip()

            # 4. Response
            choiceTime = trialClock.getTime()
            corAns     = effList.Correct.iloc[tCount]
            options    = [corAns, effList.Wrong_1.iloc[tCount], effList.Wrong_2.iloc[tCount]]
            shuffle(options)
            opt1.text, opt2.text, opt3.text = [str(o) for o in options]
            thisTrig   = trigList.Code[trigList.TriggerName=='cueAns{}'.format(effLev)].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
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
            thisTrig = trigList.Code[trigList.TriggerName=='cueITI2'].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # 6. Feedback
            feedbackTime = trialClock.getTime()
            if response=='NA':
                fb = 'Too Slow!'
                thisTrig = trigList.Code[trigList.TriggerName=='cueFbSlow'].iloc[0]
            elif acc==1:
                fb = 'Correct'
                thisTrig = trigList.Code[trigList.TriggerName=='cueFbCor{}'.format(effLev)].iloc[0]
            elif acc==0:
                fb = 'Incorrect'
                thisTrig = trigList.Code[trigList.TriggerName=='cueFbIncor{}'.format(effLev)].iloc[0]
            txt.text = fb
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            core.wait(fbTime)
            win.flip()

            # 7. ITI 3
            ITI3Time = trialClock.getTime()
            txt.text = '+'
            thisTrig = trigList.Code[trigList.TriggerName=='cueITI3'].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()

            # Save output
            out = [id, age, sex, 1, b, tCount, t, effLev, 0, thisCalc, corAns, options[0], options[1], options[2], startTime,
                ITI1Time, calcTime, choiceTime, chosenTime, ITI2Time, feedbackTime, ITI3Time, response, givenAnswer, RT,
                acc, fb]
            addOutput(filename, out)
            tCount+=1

    # Show TLX instructions screen
    instPic.image = '{}tlx_inst.png'.format(instDir)
    thisTrig      = trigList.Code[trigList.TriggerName=='instructions'].iloc[0]
    win.callOnFlip(sendTrigger, ser, thisTrig)
    instPic.draw()
    win.flip()
    event.waitKeys(keyList=['right'])

    # TLX
    effLevs     = pd.unique(CueDf['Effort Code'])
    effCue.pos  = [0, 6]
    effCue.size -= (3, 3)
    txt.height  = fontH
    TLX         = pd.read_csv(TLXcsv)
    TLX         = TLX[TLX.dimension.isin(TLXdims)]
    TLXresults  = {'id':id, 'EffortLevel':{}}
    for dim in range(len(TLX)):
        txt.text                 = TLX.itemText.iloc[dim]
        TLXresults[TLXdims[dim]] = {}
        for lev in effLevs:
            TLXresults['EffortLevel'][lev] = lev
            effCue.image                   = '{}eff{}.png'.format(stimDir, lev)
            TLXRating.reset()
            while TLXRating.noResponse:
                effCue.draw()
                txt.draw()
                TLXRating.draw()
                win.flip()
            TLXresults[TLXdims[dim]][lev] = TLXRating.getRating()
            thisTrig = trigList.Code[trigList.TriggerName=='TLX_{}_{}'.format(TLX.dimension.iloc[dim], lev)].iloc[0]
            sendTrigger(ser, thisTrig)
            win.flip()
            core.wait(0.5)
    pd.DataFrame.from_dict(TLXresults).to_csv('data/tlx/{}.csv'.format(id))

    # Show end screen
    endScreen.text='Thank you for completing our study!\n\nSee the experimenter for further details.'
    endScreen.draw()
    win.flip()
    event.waitKeys(keyList = ['escape'])
    thisTrig = trigList.Code[trigList.TriggerName=='taskEnd'].iloc[0]
    sendTrigger(ser, thisTrig)

# Run task
os.system('cls')
print(pyfiglet.figlet_format("EMG EFFORT STUDY"))
id     = input('Please enter the SUBJECT ID NUMBER: ')
age    = input("Please enter the subject's AGE: ")
sex    = input("Please enter the subject's SEX: ")
trigs_ = input("Send triggers? (y/n): ")

if trigs_=='y':
    behav = False
    ser = serial.Serial('COM3', 115200, timeout=0)
    if ser.isOpen() == False: ser.open() # open the serial port only if not open yet
    ser.write("RR".encode())
    ser.flush()
elif trigs_=='n':
    behav = True
    ser   = None
else:
    raise ValueError('Whether or not to send triggers must be "y" or "n"')

print('Good luck!')

runTask(id, str(sex), str(age), behav=behav)
