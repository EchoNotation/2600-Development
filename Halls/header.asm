	processor 6502
	include "vcs.h"

BUILD_DEBUG = 0

; --- Constants ---
;Kernel related
VBLANK_TIMER_DURATION=44
OVERSCAN_TIMER_DURATION=36

;Input related
UP_MASK = %00010000
DOWN_MASK = %00100000
LEFT_MASK = %01000000
RIGHT_MASK = %10000000 

;Rendering related
CHARACTER_HEIGHT=8
MAZE_HEIGHT=38
BATTLE_BOX_COLOR=$02
TEXT_COLOR=$06
TEXT_HIGHLIGHTED_COLOR=$0E
TEXT_INVALID_COLOR=$40
FRONTLINE_INDICATOR_COLOR=$2A
BACKLINE_INDICATOR_COLOR=$8A

SETUP_CURSOR_COLOR = $0E

CAMPFIRE_COLOR = $2A
FAR_FIRE_MAZE_HEIGHT = MAZE_HEIGHT - 24
NEAR_FIRE_MAZE_HEIGHT = MAZE_HEIGHT - 30
FAR_FIRE_GRAPHICS = RFarFire + 2 - FAR_FIRE_MAZE_HEIGHT
NEAR_FIRE_GRAPHICS1 = RNearFire + 4 -  NEAR_FIRE_MAZE_HEIGHT
NEAR_FIRE_GRAPHICS2 = RNearFire + 4 + 16 - NEAR_FIRE_MAZE_HEIGHT

EMPTY=0
A=1
B=2
C=3
D=4
E=5
F=6
G=7
H=8
I=9
J=10
K=11
L=12
M=13
N=14
O=15
P=16
Q=17
R=18
S=19
T=20
U=21
V=22
W=23
X=24
Y=25
Z=26
NUMBER0=27
NUMBER1=28
NUMBER2=29
NUMBER3=30
NUMBER4=31
NUMBER5=32
NUMBER6=33
NUMBER7=34
NUMBER8=35
NUMBER9=36

;Memory related --- This is actually a bad idea and should maybe be changed
MAZE_POINTER_PAGE_1 = $FE

;Flag related
CAMPFIRE_USED = $80
TRANSITIONING_TO_BATTLE = $40
TRANSITIONING_TO_CAMPFIRE = $20
TRANSITIONING_TO_MAZE = $10
NEED_NEW_MAZE = $08

;Maze related
NORTH = $08
SOUTH = $04
EAST = $02
WEST = $01
MAZE_WIDTH = 8
MAX_MAZE_LEVEL = 3
STEP_GRACE_PERIOD = 4
ENCOUNTER_RATE_MASK = $00

;Battle related
TIMER_MASK = $80
GUARDED_MASK = $40
SHIELDED_MASK = $20
ASLEEP_MASK = $18
PARRYING_MASK = $04
SHARPENED_MASK = $02
BLIGHTED_MASK = $01

LEGENDARY_RESIST_MASK = $80
PHYSICAL_RESIST_MASK = $40
FIRE_RESIST_MASK = $20
ICE_RESIST_MASK = $10
HOLY_RESIST_MASK = $08
ELECTRIC_RESIST_MASK = $04
POISON_RESIST_MASK = $02
RANGED_MASK = $01

FULL_MAGIC = 0
HALF_MAGIC = 1
THREE_HALVES_MAGIC = 2
ATTACK_AND_HALF_MAGIC = 3

	SEG.U Variables
	ORG $80

charIndex ds 1
char1 ds 1 ; -MMM-CCC : M - Mood, C - Class
char2 ds 1
char3 ds 1
char4 ds 1
name1 ds 4
name2 ds 4
name3 ds 4
name4 ds 4
name5 ds 4
battlerHP ds 0
hp1 ds 1
hp2 ds 1
hp3 ds 1
hp4 ds 1
enemyHP ds 4
mp1 ds 1
mp2 ds 1
mp3 ds 1
mp4 ds 1
partyBattlePos ds 1 ; ----4321 a 1 in that position indicates that that character is in the frontline ;Might be able to share
playerX ds 1 ;0000XXXX
playerY ds 1 ;0000YYYY
playerFacing ds 1 ; 000000XX 00 is east, 01 is south, 10 is west, 11 is north
exitLocation ds 1
campfireLocation ds 1
previousInput ds 1
currentInput ds 1
temp1 ds 1
temp2 ds 1
temp3 ds 1
temp4 ds 1
tempPointer4 ds 1
temp5 ds 1
tempPointer5 ds 1
temp6 ds 1
tempPointer6 ds 1
tempPointer1 ds 2
tempPointer2 ds 2
tempPointer3 ds 2
rand8 ds 1
experienceToNextLevel ds 1
currentMenu ds 1

cursorIndexAndMessageY ds 1
startingCursorIndexAndTargetID ds 1
aoeValueAndCampfireControl ds 1
aoeTargetID ds 1
aoeTargetsRemaining ds 1
menuSize ds 1
inBattle ds 1
currentBattler ds 1

vEdges ds 8 ;Vertical edges of the maze   0xxxxxxx
hEdges ds 7 ;Horizontal edges of the maze xxxxxxxx

battleActions ds 4
enemyID ds 4
battlerStatus ds 8 ;TGSSlPAB - T:Shield timer, G:Guard flag, S:Shield flag, Sl:Sleep timer, P:Parrying, A:Sharpened, B:Blighted
enemyAction ds 1
hasAction ds 1
currentMessage ds 1
menuLines ds 3
highlightedLineAndSteps ds 1
currentSound ds 1
soundOffset ds 1
soundFrequency ds 1
currentEffect ds 1
effectCounter ds 1
effectCountdown ds 1
mazeAndPartyLevel ds 1
flags ds 1
returnValue ds 1
fireMazeHeightAndMessageLine ds 1
mazeColor ds 1

	SEG CODE 

