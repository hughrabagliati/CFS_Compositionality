from __future__ import division  # so that 1/3=0.333 instead of 1/3=0

from psychopy import gui
from psychopy import visual
from psychopy import core
from psychopy import data
from psychopy import event
from psychopy import logging

from psychopy.constants import *  # things like STARTED, FINISHED
import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy.random import random, randint, normal, shuffle, choice
import os  # handy system and path functions
import csv

# Get subject number and list to use
dialogue_content = {
	'Subject number' : 0, 
	'Experiment List (Practice, 1)' : 'Practice',
	}

dialogue_order = [
	'Subject number', 
	'Experiment List (Practice, 1)'
	]

dialogue_box = gui.DlgFromDict(
	dictionary=dialogue_content, 
	order=dialogue_order, 
	title='Welcome to the experiment'
	)

if dialogue_box.OK:
	print(dialogue_content)
else:
	print('User Cancelled')
	core.quit()

# Ensure that relative paths start from the same directory as this script
# current_dir = os.path.dirname(os.path.abspath(__file__))
# os.chdir(current_dir)

# Store info about the experiment session
experiment_name = 'CFS popping time'  # from the Builder filename that created this script
experiment_info = {u'session': u'001', u'participant': u''}
experiment_info['date'] = data.getDateStr()  # add a simple timestamp
experiment_info['experiment_name'] = experiment_name

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = 'data/'+ str(dialogue_box.data[0])

# An ExperimentHandler isn't essential but helps with data saving
current_experiment = data.ExperimentHandler(
	name=experiment_name, 
	extraInfo=experiment_info, 
	runtimeInfo=None,
	#originPath=u'/Users/alex/Documents/Popping Time/',
	savePickle=True, 
	saveWideText=False,
	dataFileName=filename)

logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

# Set up the stimuli used in this trial.
stimuli = data.TrialHandler(
	nReps=1, 
	method='random', 
	extraInfo=experiment_info, 
	originPath=None,
	#trialList=data.importConditions(u'stimuli'+str(dialogue_box.data[1])+'.csv'),
	trialList=data.importConditions(u'stimuli/' + str(dialogue_content['Experiment List (Practice, 1)']) + u'_stims.csv'),
	seed=None, 
	name='stimuli')

#Need to make stimuliX.csv files for this.
#See Hugh's files for example but take the actual data from Sklar et al.

# current_experiment.addLoop(stimuli)
# current_trial = stimuli.trialList[0]
# print(current_trial['Prime'])
# Does the above do anything??


# Start Code - component code to be run before the window creation


# Set up the Window
win = visual.Window(
	size=[1280, 1024], 
	fullscr=True, 
	screen=0, 
	allowGUI=True, 
	allowStencil=False,
	monitor='testMonitor', 
	color=[0,0,0], 
	blendMode='avg', 
	useFBO=True,
	units='pix')

# Store frame rate of monitor if we can measure it successfully
experiment_info['frameRate'] = win.getActualFrameRate()

if experiment_info['frameRate'] != None:
	frameDur = 1.0/round(experiment_info['frameRate'])
else:
	frameDur = 1.0/60.0 # couldn't get a reliable measure so guess

# Initialize components for Routine "init_experiment"
init_experiment_clock = core.Clock()

# global sc_c # screen color
# global f_c # frame color
# global sq_c # square color
# global tx_c # text color
# global pri_tx_c # prime text color
# 1 = white
# -0.06 = grey 80
# -0.37 = grey 120
# -1 = black

# Colours for elements
screen_color = 1
frame_color = [0,0,0] #-0.06
square_color = -0.55
text_color = 0.7
prime_text_color = -1

# Sizes for elements
frame_size = 420 # frame size (square) (frame surrounding the stimulus)
square_size =  360 # square size (square where the stimulus is shown)
line_size = 30 # line size (the width of the line surrounding the stimulus)

# global fl_p # Where should the flash be drawn 1
# global tx_p # Where should the text stimulus be drawn
# global LeftAnchor
# global RightAnchor

# Positions for elements
flash_position = [-300, 0]
text_position = [300,0] # These were swapped in the original code but fixed by using wrong var names...
LeftAnchor = flash_position[0] - (frame_size/2) + 100
RightAnchor = text_position[0] - (frame_size/2) + 100

# global f_t # The duration (in frame) of a flash image presentation 1
# global resp_t # How long is the response time
# global flash_t # How long is the period of CFS
flash_frame_time = 1.5
response_time = 8
flash_total_time = 5.3 #This never gets used!!!!!!!!!!!

# [red, green, blue, yellow, white, cc1, cc3, cc4, cc5, purple]
# Set up array of colors to be used for random cycling later on

color_set = [
	[1, -1, -1], 
	[-1, 1, -1],
	[-1, -1, 1],
	[1, 0.97, -0.55],
	[1,1,1],
	[1.0,0,0],
	[-1,1,1],
	[1,0,-1],
	[1,-1,1],
	[0.43, 0.08, 0.70]
		]

# Parameters for the flashing area.
columns = 20
rows = 20
number_of_cells = columns * rows
cell_size = 20

#Divide the number of cells by the number of colors and round down.
cells_per_color = int(np.floor(float(number_of_cells)/len(color_set)))

#For each color, add that many cells of that color to a list.
colors_for_each_cell = []
	
for color in color_set:
	for i in range(cells_per_color):
		colors_for_each_cell.append(color)

# Shuffle the colors so that we get random ones to use to fill up the rest of
# the list, until it matches the number of cells. 
shuffle(color_set)

i = number_of_cells - len(colors_for_each_cell)
while i > 0:
	colors_for_each_cell.append(color_set[i])
	i -= 1

# Shuffle them all up again.
shuffle(colors_for_each_cell)

# Initialise the beginning coordinates for each cell in the grid.
cell_coordinates = []

# Get the uppermost left cell's coordinates
x_left = (1 - columns) * cell_size / 2
y_top = (1 - rows) * cell_size / 2

for r in range(rows):
	for c in range(columns):
		#cell_coordinates.append((x_left+float(np.random.normal(3,4,1)) + c * cell_size, y_top+float(np.random.normal(3,4,1)) + l * cell_size))
		cell_coordinates.append(
			(x_left + float(np.random.normal(3,4,1)) + c * cell_size,
			y_top + float(np.random.normal(3,4,1)) + r * cell_size)
			) 

left_side_cell_coordinates = [i for i in cell_coordinates]
right_side_cell_coordinates = [(i[0]+600, i[1]) for i in cell_coordinates]

# Choose initial random rotations for cells.
cell_orientations = np.random.uniform(0,360,len(cell_coordinates))
	
# Choose initial random sizes for cells.
cell_sizes = np.random.normal(10,5,len(cell_coordinates)) + cell_size

# Create the flash array object.
flash = visual.ElementArrayStim(
					win=win,
					fieldPos= flash_position,
					nElements=number_of_cells,
					sizes=cell_sizes,
					xys=cell_coordinates,
					colors=colors_for_each_cell,
					oris=cell_orientations,
					name='flash',
					autoLog=False,
					elementMask=None,
					elementTex=None)

# This function will be called during to change the flasher.
# No need to shuffle and then use .setX() method - can just shuffle.
def flash_change_old(flash):
	shuffle(flash.colors)
	shuffle(flash.oris)
	shuffle(flash.xys)
	flash.sizes = np.random.normal(15,10,columns*rows) + cell_size
	# shuffle(flash.sizes)
	# square_size_rand = np.random.normal(15,10,columns*rows) + cell_size*0.75
	# flash.setColors(flash.colors)
	# flash.setXYs(flash.xys)
	# flash.setOris(flash.oris)
	# flash.setSizes(square_size_rand)

def flash_change(flash, cell_size, columns, rows):
    shuffle(flash.colors)
    shuffle(flash.oris)
    shuffle(flash.xys)
    cell_size_rand = np.random.normal(15,10,columns*rows) + cell_size*0.75
    flash.setColors(flash.colors)
    flash.setXYs(flash.xys)
    flash.setOris(flash.oris)
    flash.setSizes(cell_size_rand)


# Initialize visual components for fixation_screen

fixate_whole_frame = visual.ImageStim(win=win,
	name='whole_frame',
	pos=flash_position,
	size=frame_size,
	color=frame_color, 
	interpolate=True, depth=0.0)
fixate_flash_subframe = visual.ImageStim(win=win,
	name='flash_subframe',
	pos=flash_position,
	size=square_size,
	color=frame_color,
	interpolate=True, depth=-1.0)
fixate_flash_bar_left = visual.ImageStim(win=win,
	name='flash_bar_left',
	image='texture_vertical.bmp',
	pos=[flash_position[0] - (frame_size/2), 0],
	size=[(16/120) * frame_size, frame_size],
	color=[1,1,1], 
	interpolate=True, depth=-2.0)
fixate_flash_bar_right = visual.ImageStim(win=win,
	name='flash_bar_right',
	image='texture_vertical.bmp',
	pos=[flash_position[0] + (frame_size/2), 0],
	size=[(16/120)*frame_size, frame_size],
	color=[1,1,1], 
	interpolate=True, depth=-3.0)
fixate_text_subframe = visual.ImageStim(win=win,
	name='text_subframe',
	pos=text_position,
	size=square_size,
	color=frame_color,
	interpolate=True, depth=-4.0)
fixate_text_bar_left = visual.ImageStim(win=win,
	name='text_bar_left',
	image='texture_vertical.bmp',
	pos=[text_position[0] - (frame_size/2), 0],
	size=[(16/120) * frame_size, frame_size],
	color=[1,1,1], 
	interpolate=True, depth=-5.0)
fixate_text_bar_right = visual.ImageStim(win=win,
	name='text_bar_right',
	image='texture_vertical.bmp',
	pos=[text_position[0] + (frame_size/2), 0],
	size=[(16/120) * frame_size, frame_size],
	color=[1,1,1], 
	interpolate=True, depth=-6.0)
fixate_flash_fixation_square = visual.ImageStim(win=win,
	name='flash_fixation_square',
	mask="cross",
	pos=flash_position,
	size=[30,30],
	color='black', 
	interpolate=True, depth=-4.0)
fixate_text_fixation_square  = visual.ImageStim(win=win,
	name='text_fixation_square',
	mask="cross",
	pos=text_position,
	size=[30,30],
	color='black', 
	interpolate=True, depth=-4.0)

# Initialize visual components for CFS phase

cfs_whole_frame = visual.ImageStim(win=win,
	name='whole_frame',
	pos=flash_position,
	size=frame_size,
	color=frame_color, 
	interpolate=True, depth=0.0)
cfs_flash_bar_left = visual.ImageStim(win=win,
	name='flash_bar_left',
	image='texture_vertical.bmp',
	pos=[flash_position[0] - (frame_size/2), 0],
	size=[(16/120) * frame_size, frame_size],
	color=[1,1,1], 
	interpolate=True, depth=-1.0)
cfs_flash_bar_right = visual.ImageStim(win=win,
	name='flash_bar_right',
	image='texture_vertical.bmp',
	pos=[flash_position[0] + (frame_size/2), 0],
	size=[(16/120)*frame_size, frame_size],
	color=[1,1,1], 
	interpolate=True, depth=-2.0)
cfs_text_subframe = visual.ImageStim(win=win,
	name='text_subframe',
	pos=text_position,
	size=square_size,
	color=frame_color,
	interpolate=True, depth=-3.0)
cfs_flash_subframe = visual.ImageStim(win=win,
	name='flash_subframe',
	pos=flash_position,
	size=square_size,
	color=frame_color,
	interpolate=True, depth=1.0)
cfs_text_bar_left = visual.ImageStim(win=win,
	name='text_bar_left',
	image='texture_vertical.bmp',
	pos=[text_position[0] - (frame_size/2), 0],
	size=[(16/120) * frame_size, frame_size],
	color=[1,1,1], 
	interpolate=True, depth=-4.0)
cfs_text_bar_right = visual.ImageStim(win=win,
	name='text_bar_right',
	image='texture_vertical.bmp',
	pos=[text_position[0] + (frame_size/2), 0],
	size=[(16/120) * frame_size, frame_size],
	color=[1,1,1], 
	interpolate=True, depth=-5.0)
cfs_text_display_stimuli_above = visual.TextStim(win=win,
	name='text_display_stimuli_above',
	text='stimulus sentence',
	font='Arial',
	pos=[text_position[0] , 50],
	height=20,
	color=prime_text_color,
	depth=-6.0,
	opacity=0.0)
cfs_text_display_stimuli_below = visual.TextStim(win=win,
	name='text_display_stimuli_below ',
	text='stimulus sentence',
	font='Arial',
	pos=[text_position[0] , -50],
	height=20,
	color=prime_text_color,
	depth=-6.0,
	opacity=0.0)

# Components for perceptual rating element
frame_sq_c = visual.ImageStim(win=win, name='frame_sq_c',
	image=None, mask=None,
	ori=0, pos=flash_position, size=frame_size,
	color=frame_color, colorSpace='rgb', opacity=1,
	flipHoriz=False, flipVert=False,
	texRes=128, interpolate=True, depth=0.0)
square_sq_c = visual.ImageStim(win=win, name='square_sq_c',
	image=None, mask=None,
	ori=0, pos=flash_position, size=square_size,
	color=1.0, colorSpace='rgb', opacity=1,
	flipHoriz=False, flipVert=False,
	texRes=128, interpolate=True, depth=-1.0)
LeftBar_sq_c = visual.ImageStim(win=win, name='LeftBar_sq_c',
	image='texture_vertical.bmp', mask=None,
	ori=0, pos=[flash_position[0]-(frame_size/2), 0], size=[(16/120)*frame_size,frame_size],
	color=[1,1,1], colorSpace='rgb', opacity=1,
	flipHoriz=False, flipVert=False,
	texRes=128, interpolate=True, depth=-2.0)
RightBar_sq_c = visual.ImageStim(win=win, name='RightBar_sq_c',
	image='texture_vertical.bmp', mask=None,
	ori=0, pos=[flash_position[0]+(frame_size/2), 0], size=[(16/120)*frame_size,frame_size],
	color=[1,1,1], colorSpace='rgb', opacity=1,
	flipHoriz=False, flipVert=False,
	texRes=128, interpolate=True, depth=-3.0)
TxSquare_sq_c = visual.ImageStim(win=win, name='TxSquare_sq_c',
	image=None, mask=None,
	ori=0, pos=text_position, size=square_size,
	color=square_color, colorSpace='rgb', opacity=1,
	flipHoriz=False, flipVert=False,
	texRes=128, interpolate=True, depth=-4.0)
TxLeftBar_sq_c = visual.ImageStim(win=win, name='TxLeftBar_sq_c',
	image='texture_vertical.bmp', mask=None,
	ori=0, pos=[text_position[0]-(frame_size/2), 0], size=[(16/120)*frame_size,frame_size],
	color=[1,1,1], colorSpace='rgb', opacity=1,
	flipHoriz=False, flipVert=False,
	texRes=128, interpolate=True, depth=-5.0)
TxRightBar_sq_c = visual.ImageStim(win=win, name='TxRightBar_sq_c',
	image='texture_vertical.bmp', mask=None,
	ori=0, pos=[text_position[0]+(frame_size/2), 0], size=[(16/120)*frame_size,frame_size],
	color=[1,1,1], colorSpace='rgb', opacity=1,
	flipHoriz=False, flipVert=False,
	texRes=128, interpolate=True, depth=-6.0)

myRatingScale_c = visual.RatingScale(win, choices=['English', 'Chinese'], noMouse = True, pos = (-320,0.6), stretch = 0.5)

marker_l = visual.ImageStim(win,name='marker_l', image=None, mask=None,
	ori=0, pos=(LeftAnchor,80), size=30,
	color=[1.0,0.6,0.6], colorSpace='rgb', opacity=1,
	flipHoriz=False, flipVert=False,
	texRes=128, interpolate=True, depth=-4.0)

marker_r = visual.ImageStim(win,name='marker_r', image=None, mask=None,
	ori=0, pos=(RightAnchor,80), size=30,
	color=[1.0,0.6,0.6], colorSpace='rgb', opacity=1,
	flipHoriz=False, flipVert=False,
	texRes=128, interpolate=True, depth=-4.0)

L_Opt1 = visual.TextStim(win=win, ori=0, name='L_Opt1',
	text='No Experience',	font='Arial',
	pos=(LeftAnchor+65,80), height=20, wrapWidth=None,
	color=text_color, colorSpace='rgb', opacity=1,
	depth=-6.0, alignHoriz = 'left')
L_Opt2 = visual.TextStim(win=win, ori=0, name='L_Opt2',
	text='Vague Experience',	font='Arial',
	pos=(LeftAnchor+65,30), height=20, wrapWidth=None,
	color=text_color, colorSpace='rgb', opacity=1,
	depth=-6.0, alignHoriz = 'left')
L_Opt3 = visual.TextStim(win=win, ori=0, name='L_Opt3',
	text='Almost Clear Experience',	font='Arial',
	pos=(LeftAnchor+65,-20), height=20, wrapWidth=None,
	color=text_color, colorSpace='rgb', opacity=1,
	depth=-6.0, alignHoriz = 'left')
L_Opt4 = visual.TextStim(win=win, ori=0, name='L_Opt4',
	text='Totally Clear Experience',	font='Arial',
	pos=(LeftAnchor+65,-70), height=20, wrapWidth=None,
	color=text_color, colorSpace='rgb', opacity=1,
	depth=-6.0, alignHoriz = 'left')

R_Opt1 = visual.TextStim(win=win, ori=0, name='R_Opt1',
	text='No Experience',	font='Arial',
	pos=(RightAnchor+65,80), height=20, wrapWidth=None,
	color=text_color, colorSpace='rgb', opacity=1,
	depth=-6.0, alignHoriz = 'left')
R_Opt2 = visual.TextStim(win=win, ori=0, name='R_Opt2',
	text='Vague Experience',	font='Arial',
	pos=(RightAnchor+65,30), height=20, wrapWidth=None,
	color=text_color, colorSpace='rgb', opacity=1,
	depth=-6.0, alignHoriz = 'left')
R_Opt3 = visual.TextStim(win=win, ori=0, name='R_Opt3',
	text='Almost Clear Experience',	font='Arial',
	pos=(RightAnchor+65,-20), height=20, wrapWidth=None,
	color=text_color, colorSpace='rgb', opacity=1,
	depth=-6.0, alignHoriz = 'left')
R_Opt4 = visual.TextStim(win=win, ori=0, name='R_Opt4',
	text='Totally Clear Experience',	font='Arial',
	pos=(RightAnchor+65,-70), height=20, wrapWidth=None,
	color=text_color, colorSpace='rgb', opacity=1,
	depth=-6.0, alignHoriz = 'left')


endExpNow = False # Flag for 'escape' or other condition => quit the exp

frameN = -1
#-------Stuff for taking a break-------

left_break_message = visual.TextStim(win=win, name='left break message',
	text='Take a break. Press a key to continue.',	font='Arial',
	pos=[-300,0], height=25,
	color='black',
	depth=-6.0)

right_break_message = visual.TextStim(win=win, name='right break message',
	text='Take a break. Press a key to continue.',	font='Arial',
	pos=[300,0], height=25,
	color='black',
	depth=-6.0)

left_starting_message = visual.TextStim(win=win, name='left starting message',
	text='Press a key to begin.',	font='Arial',
	pos=[-300,0], height=25,
	color='black',
	depth=-6.0)

right_starting_message = visual.TextStim(win=win, name='right starting message',
	text='Press a key to begin.',	font='Arial',
	pos=[300,0], height=25,
	color='black',
	depth=-6.0)

init_squareClock = core.Clock()
reaction_time = core.Clock()
routineTimer = core.CountdownTimer()
opacity_timer = core.Clock()

get_confClock = core.Clock()

key_sq = event.BuilderKeyResponse()  # create an object of type KeyResponse

for trial_number, trial in enumerate(stimuli):
	print(trial)

	if trial_number == 0:
		left_starting_message.draw()
		right_starting_message.draw()
		win.flip()
		event.waitKeys()


	if trial_number > 0 and trial_number % 75 == 0:
		left_break_message.draw()
		right_break_message.draw()
		win.flip()
	
		event.waitKeys()

	key_sq.status = NOT_STARTED

	toss = np.random.randint(0,2)
	if toss == 0:
		cfs_text_display_stimuli = cfs_text_display_stimuli_above
		top_or_bottom = 'up'
	else:
		cfs_text_display_stimuli = cfs_text_display_stimuli_below
		top_or_bottom = 'down'


	# Take out the jumbled stuff
	#
	# if trial['Shuffle'] == 'Yes':
	# 	mixed_up = list(trial['Prime'])
	# 	shuffle(mixed_up)
	# 	mixed_up = ''.join(mixed_up)
	# 	cfs_text_display_stimuli.setText(mixed_up)
	
	# if trial['Shuffle'] == 'No':
	# 	cfs_text_display_stimuli.setText(trial['Prime'])

	init_squareClock.reset()

	init_squareComponents = []
	init_squareComponents.append(fixate_whole_frame)
	init_squareComponents.append(fixate_flash_subframe)
	init_squareComponents.append(fixate_flash_bar_left)
	init_squareComponents.append(fixate_flash_bar_right)
	init_squareComponents.append(fixate_text_subframe)
	init_squareComponents.append(fixate_text_bar_left)
	init_squareComponents.append(fixate_text_bar_right)
	init_squareComponents.append(fixate_text_fixation_square)
	init_squareComponents.append(fixate_flash_fixation_square)


	for thisComponent in init_squareComponents:
		if hasattr(thisComponent, 'status'):
			thisComponent.status = NOT_STARTED

	continueRoutine = True
	while continueRoutine and init_squareClock.getTime() < 2.0:
		# get current time
		t = init_squareClock.getTime()
		frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
		# update/draw components on each frame

		# *fixate_whole_frame* updates
		if t >= 0.0 and fixate_whole_frame.status == NOT_STARTED:
			fixate_whole_frame.setAutoDraw(True)

		# *fixate_flash_subframe* updates
		if t >= 0.0 and fixate_flash_subframe.status == NOT_STARTED:
			fixate_flash_subframe.setAutoDraw(True)

		# *fixate_flash_bar_left* updates
		if t >= 0.0 and fixate_flash_bar_left.status == NOT_STARTED:
			fixate_flash_bar_left.setAutoDraw(True)

		# *fixate_flash_bar_right* updates
		if t >= 0.0 and fixate_flash_bar_right.status == NOT_STARTED:
			fixate_flash_bar_right.setAutoDraw(True)

		# *fixate_text_subframe* updates
		if t >= 0.0 and fixate_text_subframe.status == NOT_STARTED:
			fixate_text_subframe.setAutoDraw(True)

		# *fixate_text_bar_left* updates
		if t >= 0.0 and fixate_text_bar_left.status == NOT_STARTED:
			fixate_text_bar_left.setAutoDraw(True)

		# *fixate_text_bar_right* updates
		if t >= 0.0 and fixate_text_bar_right.status == NOT_STARTED:
			fixate_text_bar_right.setAutoDraw(True)

		if t >= 0.5 and fixate_text_fixation_square.status == NOT_STARTED:
			fixate_text_fixation_square.setAutoDraw(True)
			
		if t >= 0.5 and fixate_flash_fixation_square.status == NOT_STARTED:
			fixate_flash_fixation_square.setAutoDraw(True)

		continueRoutine = False  # will revert to True if at least one component still running
		for thisComponent in init_squareComponents:
			if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
				continueRoutine = True
				break  # at least one component has not yet finished

		# check for quit (the Esc key)
		if endExpNow or event.getKeys(keyList=["escape"]):
			core.quit()

		# refresh the screen
		if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
			win.flip()
		else:  # this Routine was not non-slip safe so reset non-slip timer
			routineTimer.reset()

	#-------Ending Routine "init_square"-------
	for thisComponent in init_squareComponents:
		if hasattr(thisComponent, "setAutoDraw"):
			thisComponent.setAutoDraw(False)

	# Store data for current_experiment (ExperimentHandler)
	current_experiment.addData('SubjNo', str(dialogue_box.data[0]))
	current_experiment.addData('List', str(dialogue_box.data[1]))
	current_experiment.addData('prime', trial['Prime'])
	current_experiment.addData('prime_semantics', trial['Semantics'])
	#current_experiment.addData('shuffled?', trial['Shuffle'])
	#current_experiment.addData('prime_semantics_full', trial['Semantics'] + "_" + trial['Shuffle'])
	current_experiment.addData('Contrast', trial['Contrast'])
	current_experiment.addData('Condition', trial['Condition'])
	current_experiment.addData('MeanAffectivity', trial['MeanAffectivity'])

	#------Prepare to start Routine "show_flash"-------

	t = 0
	
	# frameN = -1
	
	# update component parameters for each repeat
	# flash begin routine
	# f_change = 0
	#key_fl = event.BuilderKeyResponse()  # create an object of type KeyResponse
	#key_fl.status = NOT_STARTED
	# keep track of which components have finished
	show_flashComponents = []
	show_flashComponents.append(cfs_whole_frame)
	show_flashComponents.append(cfs_flash_bar_left)
	show_flashComponents.append(cfs_flash_bar_right)
	show_flashComponents.append(cfs_text_subframe)
	show_flashComponents.append(cfs_flash_subframe)
	show_flashComponents.append(cfs_text_bar_left)
	show_flashComponents.append(cfs_text_bar_right)
	show_flashComponents.append(cfs_text_display_stimuli)
	show_flashComponents.append(fixate_flash_fixation_square)
	show_flashComponents.append(fixate_text_fixation_square)
	#show_flashComponents.append(key_fl)
	
	toss = np.random.randint(0,2)
	if toss == 0:
		cfs_whole_frame.pos = text_position
		cfs_flash_bar_left.pos = [text_position[0] - (frame_size/2), 0]
		cfs_flash_bar_right.pos = [text_position[0] + (frame_size/2), 0]
		cfs_text_subframe.pos = flash_position
		cfs_flash_subframe.pos = text_position
		cfs_text_bar_left.pos = [flash_position[0] - (frame_size/2), 0]
		cfs_text_bar_right.pos = [flash_position[0] + (frame_size/2), 0]
		cfs_text_display_stimuli_above.pos = [flash_position[0] , 50]
		cfs_text_display_stimuli_below.pos = [flash_position[0] , -50]
		flash.xys = right_side_cell_coordinates


	if toss == 1:
		cfs_whole_frame.pos = flash_position
		cfs_flash_bar_left.pos = [flash_position[0] - (frame_size/2), 0]
		cfs_flash_bar_right.pos = [flash_position[0] + (frame_size/2), 0]
		cfs_text_subframe.pos = text_position
		cfs_flash_subframe.pos = flash_position
		cfs_text_bar_left.pos = [text_position[0] - (frame_size/2), 0]
		cfs_text_bar_right.pos = [text_position[0] + (frame_size/2), 0]
		cfs_text_display_stimuli_above.pos = [text_position[0] , 50]
		cfs_text_display_stimuli_below.pos = [text_position[0] , -50]
		flash.xys = left_side_cell_coordinates


	for thisComponent in show_flashComponents:
		if hasattr(thisComponent, 'status'):
			thisComponent.status = NOT_STARTED



	reaction_time.reset()  # clock
	continueRoutine = True
	routineTimer.reset()
	routineTimer.add(8.0)#This is how long CFS phase lasts

	opacity_timer.reset()
	
	event.clearEvents(eventType='keyboard')

	while continueRoutine:
		time_remaining = routineTimer.getTime()
		
		if time_remaining <= 0.0:
			continueRoutine = False	

		
		key_presses = event.getKeys(timeStamped=reaction_time)
			
		if key_presses:
			if key_presses[0][0] == 'escape':
				endExpNow = True
			if key_presses[0][0] == 'up':
				pressed = key_presses[0][0]
				rt = key_presses[0][1]
				continueRoutine = False
			if key_presses[0][0] == 'down':
				pressed = key_presses[0][0]
				rt = key_presses[0][1]
				continueRoutine = False

		#event.clearEvents(eventType='keyboard')


		# frameN = frameN + 1
		

		if time_remaining >= 0.0:
			for item in [cfs_flash_bar_left, 
				cfs_flash_bar_right, 
				cfs_text_subframe, 
				cfs_text_subframe, 
				cfs_text_bar_left,
				cfs_text_bar_right,
				cfs_text_display_stimuli,
				fixate_flash_fixation_square,
				fixate_text_fixation_square]:
				if item.status == NOT_STARTED:
					item.setAutoDraw(True)

		if time_remaining > 0.0 and cfs_flash_subframe.status == NOT_STARTED:
			cfs_flash_subframe.draw()

		o = opacity_timer.getTime()
		
		o = opacity_timer.getTime()
		if o <= 0.7 and trial['Contrast'] == 80:
			cfs_text_display_stimuli.setOpacity(o * 1.143)
			if trial['Semantics'] == "Hebrew":
				cfs_text_display_stimuli.setText(trial['Prime'][::-1])#Reverse the Hewbrew stim because of lack of RTL lang support
			else:
				cfs_text_display_stimuli.setText(trial['Prime'])
			# if trial['Shuffle'] == 'Yes':
			# 	cfs_text_display_stimuli.setText(mixed_up)
			# if trial['Shuffle'] == 'No':
			# 	cfs_text_display_stimuli.setText(trial['Prime'])

		else:
			if o <= 0.7:
				cfs_text_display_stimuli.setOpacity(o * 0.72)
				if trial['Semantics'] == "Hebrew":
					cfs_text_display_stimuli.setText(trial['Prime'][::-1])#Reverse the Hewbrew stim because of lack of RTL lang support
				else:
					cfs_text_display_stimuli.setText(trial['Prime'])
				# if trial['Shuffle'] == 'Yes':
				# 	cfs_text_display_stimuli.setText(mixed_up)
				# if trial['Shuffle'] == 'No':
				# 	cfs_text_display_stimuli.setText(trial['Prime'])

		# flash each frame
		# if time_remaining > 0:
		# 	if frameN >= f_change:
		# 		flash_change(flash)
		# 		f_change += flash_frame_time
		# 	flash.draw()
		
		if time_remaining > 0:
			flash_change(flash=flash, cell_size=cell_size, columns=columns, rows=rows)
			flash.draw()

		if time_remaining <= 0:
			pressed = None
			rt = None


		# # check if all components have finished
		# if not continueRoutine:  # a component has requested a forced-end of Routine
		# 	routineTimer.reset()  # if we abort early the non-slip timer needs reset
		# 	break
		
		#continueRoutine = False  # will revert to True if at least one component still running
		


		#check for quit (the Esc key)
		if endExpNow or event.getKeys(keyList=["escape"]):
		 	core.quit()

		# refresh the screen
		if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
			win.flip()

	
	#-------Ending Routine "show_flash"-------
	for thisComponent in show_flashComponents:
		if hasattr(thisComponent, "setAutoDraw"):
			thisComponent.setAutoDraw(False)

	if pressed and rt:
		current_experiment.addData('correct response', top_or_bottom)
		current_experiment.addData('given response', pressed)
		if pressed == top_or_bottom:
			current_experiment.addData('match?', 1)
		else:
			current_experiment.addData('match?', 0)
		current_experiment.addData('rt', rt)

	else:
		current_experiment.addData('correct response', top_or_bottom)
		current_experiment.addData('given response', None)
		current_experiment.addData('match?', 0)
		current_experiment.addData('rt', None)

	# check responses
	# if key_fl.keys in ['', [], None]:  # No response was made
	#	key_fl.keys=None
	# store data for current_experiment (ExperimentHandler)
	# current_experiment.addData('key response', theseKeys)
	# if theseKeys != None:  # we had a response
	# 	current_experiment.addData('response time', theseKeys)
	
	#current_experiment.nextEntry() 

	t = 0
	a = 0 # Where on the scale are they?
	LeftPos = [(LeftAnchor+50,80),(LeftAnchor+50,30),(LeftAnchor+50,-20),(LeftAnchor+50,-70)]
	RightPos = [(RightAnchor+50,80),(RightAnchor+50,30),(RightAnchor+50,-20),(RightAnchor+50 ,-70)]
	marker_l.setPos(LeftPos[0])
	marker_r.setPos(RightPos[0])
	get_confClock.reset()  # clock
	frameN = -1
	# update component parameters for each repeat
	key_sq_c = event.BuilderKeyResponse()  # create an object of type KeyResponse
	key_sq_c.status = NOT_STARTED
	# keep track of which components have finished
	get_confComponents = []
	get_confComponents.append(frame_sq_c)
	get_confComponents.append(square_sq_c)
	get_confComponents.append(LeftBar_sq_c)
	get_confComponents.append(RightBar_sq_c)
	get_confComponents.append(TxSquare_sq_c)
	get_confComponents.append(TxLeftBar_sq_c)
	get_confComponents.append(TxRightBar_sq_c)
	get_confComponents.append(key_sq_c)
	get_confComponents.append(myRatingScale_c)
	get_confComponents.append(marker_l)
	get_confComponents.append(marker_r)
	get_confComponents.append(L_Opt1)
	get_confComponents.append(L_Opt2)
	get_confComponents.append(L_Opt3)
	get_confComponents.append(L_Opt4)
	get_confComponents.append(R_Opt1)
	get_confComponents.append(R_Opt2)
	get_confComponents.append(R_Opt3)
	get_confComponents.append(R_Opt4)

	for thisComponent in get_confComponents:
		if hasattr(thisComponent, 'status'):
			thisComponent.status = NOT_STARTED

	#-------Start Routine "get_conf"-------
	continueRoutine = True
	while myRatingScale_c.noResponse:
		# get current time
		t = get_confClock.getTime()
		frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
		# update/draw components on each frame

		# *LeftBar_sq* updates
		if t >= 0.0 and LeftBar_sq_c.status == NOT_STARTED:
			# keep track of start time/frame for later
			LeftBar_sq_c.tStart = t  # underestimates by a little under one frame
			LeftBar_sq_c.frameNStart = frameN  # exact frame index
			LeftBar_sq_c.setAutoDraw(True)

		# *RightBar_sq* updates
		if t >= 0.0 and RightBar_sq_c.status == NOT_STARTED:
			# keep track of start time/frame for later
			RightBar_sq_c.tStart = t  # underestimates by a little under one frame
			RightBar_sq_c.frameNStart = frameN  # exact frame index
			RightBar_sq_c.setAutoDraw(True)

		# *TxLeftBar_sq* updates
		if t >= 0.0 and TxLeftBar_sq_c.status == NOT_STARTED:
			# keep track of start time/frame for later
			TxLeftBar_sq_c.tStart = t  # underestimates by a little under one frame
			TxLeftBar_sq_c.frameNStart = frameN  # exact frame index
			TxLeftBar_sq_c.setAutoDraw(True)

		# *TxRightBar_sq* updates
		if t >= 0.0 and TxRightBar_sq_c.status == NOT_STARTED:
			# keep track of start time/frame for later
			TxRightBar_sq_c.tStart = t  # underestimates by a little under one frame
			TxRightBar_sq_c.frameNStart = frameN  # exact frame index
			TxRightBar_sq_c.setAutoDraw(True)

		if t >= 0.5:
			L_Opt1.setAutoDraw(True)
			L_Opt2.setAutoDraw(True)
			L_Opt3.setAutoDraw(True)
			L_Opt4.setAutoDraw(True)
			R_Opt1.setAutoDraw(True)
			R_Opt2.setAutoDraw(True)
			R_Opt3.setAutoDraw(True)
			R_Opt4.setAutoDraw(True)
	
			marker_l.draw()
			marker_r.draw()

		# *key_sq* updates
		if t >= 0.5 and key_sq_c.status == NOT_STARTED:
			# keep track of start time/frame for later
			key_sq_c.tStart = t  # underestimates by a little under one frame
			key_sq_c.frameNStart = frameN  # exact frame index
			key_sq_c.status = STARTED
			# keyboard checking is just starting
			key_sq_c.clock.reset()  # now t=0
			event.clearEvents(eventType='keyboard')
		if key_sq_c.status == STARTED:
			theseKeys = event.getKeys()

			# check for quit:
			if "escape" in theseKeys:
				endExpNow = True
			if "right" in theseKeys:  # at least one key was pressed
				key_sq_c.keys = theseKeys[-1]  # just the last key pressed
				key_sq_c.rt = key_sq_c.clock.getTime()
				# a response ends the routine
				continueRoutine = False
			if "up" in theseKeys:
				a = max(a-1,0)
				marker_l.setPos(LeftPos[a])
				marker_r.setPos(RightPos[a])
			if "down" in theseKeys:
				a = min(a+1,3)
				marker_l.setPos(LeftPos[a])
				marker_r.setPos(RightPos[a])

		# check if all components have finished
		if not continueRoutine:  # a component has requested a forced-end of Routine
			routineTimer.reset()  # if we abort early the non-slip timer needs reset
			break
		continueRoutine = False  # will revert to True if at least one component still running
		for thisComponent in get_confComponents:
			if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
				continueRoutine = True
				break  # at least one component has not yet finished

		# check for quit (the Esc key)
		if endExpNow or event.getKeys(keyList=["escape"]):
			core.quit()

		# refresh the screen
		if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
			win.flip()
		else:  # this Routine was not non-slip safe so reset non-slip timer
			routineTimer.reset()

	#-------Ending Routine "get_conf"-------
	for thisComponent in get_confComponents:
		if hasattr(thisComponent, "setAutoDraw"):
			thisComponent.setAutoDraw(False)
	# check responses
	if key_sq_c.keys in ['', [], None]:  # No response was made
		key_sq_c.keys = None
	# store data for current_experiment (ExperimentHandler)
	current_experiment.addData('perceptual rating', a)
	if key_sq_c.keys == None:  # we had a response
		current_experiment.addData('perceptual rating.reactiontime', key_sq_c.rt)
	
	current_experiment.nextEntry()


current_experiment.saveAsWideText(fileName = './data/' + str(dialogue_box.data[0]) + '_Trials.csv')

win.close()
core.quit()