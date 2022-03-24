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
    initCSV(filename, ['id', 'age', 'sex', 'cued', 'block', 'trial' ,'trialinblock', 'efflev', 'rewlev', 'calc', 'corAns', 'option1', 'option2', 'option3', 'tstart',
        'ITI1Time','probTime', 'respTime', 'selectTime', 'ITI2Time', 'fbTime', 'ITI3Time', 'key_press', 'chosenAns', 'RT', 'acc', 'feedback', 'total_points'])

    # Window setup
    winsize = [1920, 1080] if not id=='debug' else [1920//1.5, 1080//1.5]
    winfull = True #if not id=='debug' else False
    winGUI  = False if not id=='debug' else True
    win     = visual.Window(size=winsize, fullscr=winfull,allowGUI=winGUI, allowStencil=False,monitor='testMonitor', color=[1,1,1], colorSpace='rgb',  blendMode='avg', useFBO=True, units='cm')

    # Set constants
    maxpay       = '5.00'                                       # total subjects are told they earned at the end (str: in dollars)
    textCol      = [-1, -1, -1]                                 # font colour
    fontH        = 1                                            # font height
    rewlevs      = ['low', 'high']                              # reward levels
    rewpoints    = [1, 10]                                      # points associated with reward levels (low,high)
    efflevs      = [1,2,3,4]                                    # effort levels
    teststim     = pd.read_csv('stim/test.csv')                 # test stimulus list
    pracstim     = pd.read_csv('stim/train.csv')                # practice stimulus list
    nblocks      = 5                                            # number of blocks
    ntrials      = 2 if id=='debug' else len(teststim)/nblocks  # number of trials per block in cued condition
    nPract       = 0 if id=='debug' else len(pracstim)          # number of practice trials
    choiceKeys   = ['q', 'w', 'e']                              # keys to make choices
    cueTime      = 2                                            # amount of time effort cue stays up (in s. )
    ITI          = 1.5                                          # amount of time fixation cross stays up (in s.)
    probTime     = 5                                            # amount of time calculation stays up (in s.)
    respTime     = 5                                            # amount of time response options stay up (in s.)
    fbTime       = 1                                            # amount of time feedback stays up (in s. )
    stimDir      = 'stim/'                                      # directory where stimuli are located
    instDir      = 'instructions/'                              # directory where instructions are located
    TLXcsv       = f'{stimDir}TLX.csv'                          # csv file containing TLX questions and anchors
    TLXdims      = ['mental_demand', 'effort']                  # which TLX dimensions to ask
    trigList     = pd.read_csv('EffEMG_trigs2.csv')             # triggers


    # initialize instructions
    insts      = os.listdir(instDir)
    instPic    = visual.ImageStim(win, image=f"{instDir}Slide1.png")

    # initialize trial components
    trialClock = core.Clock()
    effCue     = visual.ImageStim(win, image=f"{stimDir}cue1_low.png", size=(0.2,0.75), units='norm')
    txt        = visual.TextStim(win, text="", height=3*fontH, color=textCol, pos=[0, 0], wrapWidth=30)
    opt1       = visual.TextStim(win, text="", height=3*fontH, color=textCol, pos=[-8, 0])
    opt2       = visual.TextStim(win, text="", height=3*fontH, color=textCol, pos=[0, 0])
    opt3       = visual.TextStim(win, text="", height=3*fontH, color=textCol, pos=[8, 0])
    endScreen  = visual.TextStim(win, text="", height=fontH,   color=textCol, pos=[0, 0], wrapWidth=30)
    TLXRating  = visual.RatingScale(win, low=0, high=10, markerStart=5, textColor='black', scale=None, lineColor='black', markerColor='DarkRed', labels=['Very Low', 'Very High'])

    # initialize effort/reward pairings
    reweff     = [(r,e) for r in rewlevs for e in efflevs]          
    reweff     *= int((ntrials*nblocks)/len(reweff))
    shuffle(reweff)

    effdfs     = {}
    effCounter = [0]*4 
    for _,e in enumerate(efflevs):
        effdfs[e] = teststim[teststim['Effort Code']==e].sample(frac=1).reset_index(drop=True)


    #--------------------------------------Start Task-----------------------------------------
    win.mouseVisible=True if id=='debug' else False

    thisTrig = trigList.Code[trigList.TriggerName=='taskStart'].iloc[0]
    sendTrigger(ser, thisTrig)

    # Instructions A
    thisTrig = trigList.Code[trigList.TriggerName=='instructions'].iloc[0]
    instCounter = 1
    while instCounter<5:
        instPic.image = f'{instDir}Slide{instCounter}.png'
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
        effLev   = pracstim['Effort Code'].iloc[pt]
        nums     = pracstim[['num1', 'num2', 'num3', 'num4', 'num5']].loc[pt,:]
        thisCalc = ''.join([str(n) for n in nums.values])
        txt.text = thisCalc
        thisTrig = trigList.Code[trigList.TriggerName==f'pracProb{effLev}'].iloc[0]
        win.callOnFlip(sendTrigger, ser, thisTrig)
        txt.draw()
        win.flip()
        core.wait(probTime)
        win.flip()

        # 2. Response
        choiceTime = trialClock.getTime()
        corAns     = pracstim.Correct.iloc[pt]
        options    = [corAns, pracstim.Wrong_1.iloc[pt], pracstim.Wrong_2.iloc[pt]]
        shuffle(options)
        opt1.text, opt2.text, opt3.text = [str(o) for o in options]
        thisTrig   = trigList.Code[trigList.TriggerName==f'pracAns{effLev}'].iloc[0]
        win.callOnFlip(sendTrigger, ser, thisTrig)
        opt1.draw()
        opt2.draw()
        opt3.draw()
        win.flip()
        thisTrig   = trigList.Code[trigList.TriggerName==f'pracResp{effLev}'].iloc[0]
        key_press  = event.waitKeys(keyList=choiceKeys+['escape'])
        sendTrigger(ser, thisTrig)
        chosenTime = trialClock.getTime()
        if not key_press:
            response    = 'NA'
            givenAnswer = 'NA'
            RT          = 'NA'
            acc         = 0
        if key_press[0]=='escape':
            core.quit()
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
            thisTrig = trigList.Code[trigList.TriggerName == f'pracFbCor{effLev}'].iloc[0]
        else:
            fb       = 'Incorrect'
            thisTrig = trigList.Code[trigList.TriggerName == f'pracFbIncor{effLev}'].iloc[0]
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
                acc, fb, 0]
        addOutput(filename, out)

    # Instructions B
    thisTrig = trigList.Code[trigList.TriggerName=='instructions'].iloc[0]
    while instCounter<17:
        instPic.image = f'{instDir}Slide{instCounter}.png'
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
    b            = 0
    t_in_b       = 0
    total_points = 0
    for t,re in enumerate(reweff):
        if t>0 and t%ntrials==0:
            # Break
            t_in_b = 0
            b+=1
            txt.height = fontH
            txt.text   = 'You can now take a small break.\n\nIf you have any questions, please notify the experimenter.\n\nWhen you are ready to continue with the experiment, press SPACE.'
            thisTrig   = trigList.Code[trigList.TriggerName=='blockScreen'].iloc[0]
            win.callOnFlip(sendTrigger, ser, thisTrig)
            txt.draw()
            win.flip()
            key_press  = event.waitKeys(keyList=['space']+['escape'])
            if key_press[0]=='escape': core.quit()
            win.flip()
            txt.height = 3*fontH
            txt.text   = '+'
            txt.draw()
            win.flip()
            core.wait(ITI)
            win.flip()
    
        # Run trial
        startTime  = trialClock.getTime()
        reward     = re[0]
        effort     = re[1]
        thisrow    = effdfs[effort].loc[effCounter[effort-1]]
        effCounter[effort-1]+=1

        # 1. Display effort cue
        effCueTime   = trialClock.getTime()
        effCue.image = f'{stimDir}cue{effort}_{reward}.png'
        thisTrig     = trigList.Code[trigList.TriggerName==f'cue{effort}_{reward}'].iloc[0]
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
        nums     = thisrow.loc[['num1', 'num2', 'num3', 'num4', 'num5']].to_list()
        thisCalc = ''.join([str(n) for n in nums])
        txt.text = thisCalc
        thisTrig = trigList.Code[trigList.TriggerName==f'cueProb{effort}_{reward}'].iloc[0]
        win.callOnFlip(sendTrigger, ser, thisTrig)
        txt.draw()
        win.flip()
        core.wait(probTime)
        win.flip()

        # 4. Response
        choiceTime = trialClock.getTime()
        corAns     = thisrow.Correct
        options    = [corAns, thisrow.Wrong_1, thisrow.Wrong_2]
        shuffle(options)
        opt1.text, opt2.text, opt3.text = [str(o) for o in options]
        thisTrig   = trigList.Code[trigList.TriggerName==f'cueAns{effort}_{reward}'].iloc[0]
        win.callOnFlip(sendTrigger, ser, thisTrig)
        opt1.draw()
        opt2.draw()
        opt3.draw()
        win.flip()
        key_press  = event.waitKeys(keyList = choiceKeys+['escape'], maxWait=respTime)
        chosenTime = trialClock.getTime()
        if not key_press:
            response    = 'NA'
            givenAnswer = 'NA'
            RT          = 'NA'
            acc         = 0
        elif key_press[0]=='escape':
            core.quit()
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
            fb_color = 'black'
            fb = 'Too Slow!'
            thisTrig = trigList.Code[trigList.TriggerName=='cueFbSlow'].iloc[0]
        elif acc==1:
            fb_color = 'green'
            pts = rewpoints[0] if reward=='low' else rewpoints[1]
            fb = f'+{pts}'
            total_points+=pts
            thisTrig = trigList.Code[trigList.TriggerName==f'cueFbCor{effort}'].iloc[0]
        elif acc==0:
            fb_color = 'red'
            fb = '+0'
            thisTrig = trigList.Code[trigList.TriggerName==f'cueFbIncor{effort}'].iloc[0]
        txt.text = fb
        win.callOnFlip(sendTrigger, ser, thisTrig)
        txt.color = fb_color
        txt.draw()
        win.flip()
        core.wait(fbTime)
        win.flip()
        txt.color = textCol

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
        out = [id, age, sex, 1, b, t, t_in_b, effort, reward, thisCalc, corAns, options[0], options[1], options[2], startTime,
            ITI1Time, calcTime, choiceTime, chosenTime, ITI2Time, feedbackTime, ITI3Time, response, givenAnswer, RT,
            acc, fb, total_points]
        addOutput(filename, out)
        t_in_b+=1

    # Show TLX instructions screen
    instPic.image = f'{instDir}Slide17.png'
    thisTrig      = trigList.Code[trigList.TriggerName=='instructions'].iloc[0]
    win.callOnFlip(sendTrigger, ser, thisTrig)
    instPic.draw()
    win.flip()
    event.waitKeys(keyList=['right'])

    # TLX
    re          = [(r,e) for r in rewlevs for e in efflevs] 
    effCue.size = [i/1.5 for i in effCue.size]
    effCue.pos  = [0, 0.5]
    txt.height  = fontH
    TLX         = pd.read_csv(TLXcsv)
    TLX         = TLX[TLX.dimension.isin(TLXdims)]
    TLXresults  = {'id':id, 'RewardLevel':[], 'EffortLevel':[] }
    for dim in TLXdims: 
        TLXresults[dim] = []

    for i,dim in enumerate(TLXdims):
        txt.text  = TLX.itemText.iloc[i]
        for rew,eff in re:
            TLXresults['RewardLevel'].append(rew)
            TLXresults['EffortLevel'].append(eff)
            effCue.image = f'{stimDir}cue{eff}_{rew}.png'
            TLXRating.reset()
            while TLXRating.noResponse:
                effCue.draw()
                txt.draw()
                TLXRating.draw()
                win.flip()
            TLXresults[dim].append(TLXRating.getRating())
            thisTrig = trigList.Code[trigList.TriggerName==f'TLX_{TLX.dimension.iloc[i]}_{eff}'].iloc[0]
            sendTrigger(ser, thisTrig)
            win.flip()
            core.wait(0.5)

    TLXresults['RewardLevel'] = TLXresults['RewardLevel'][:len(re)]
    TLXresults['EffortLevel'] = TLXresults['EffortLevel'][:len(re)]
    pd.DataFrame.from_dict(TLXresults).to_csv(f'data/tlx/{id}.csv')

    # Show end screen
    endScreen.text=f'Thank you for completing our study!\n\nYou earned an extra ${maxpay}!\n\nSee the experimenter for further details.'
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
