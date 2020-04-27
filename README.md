# 3110-project

Group project for CS 3110 of Raahi Menon, Ritwick Bhargava, and Sean Yang

# Installation
For installation instructions, please see `INSTALL.txt`

# Compilation
To compile our game, run the command `make build`
To play our game, run the command `make play`
To clean build files, run `make clean`

# Gameplay
On running make play, the terminal will prompt you for a load file. On first
play, type `0.json` and hit enter. 

Use the `W` key to move upwards, `S` to move downwards, `A` to move left and 
`D` to move right. Please do not press these buttons at the same time; this 
could cause undefined behavior. 

To pick up an item, such as the **blue rupee** in this demo, press `E`.

To quit the game, press `Q`. This will create a new save file that stores your
last position in the game. The save file will be the json file in the saves 
whose number matches the number of files in the folder minus 1 (for example, 
when you first quit the game it will generate `1.json`, meaning two files will
be in the directory). **Item states are saved, but not loaded** - that is, the 
item on screen is a demo item, so even if you pick it up and save, it will still
be there when you load. This is not a bug. 

Please note that wall collisions are not yet implemented.