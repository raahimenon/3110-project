# 3110-project

Group project for CS 3110 of Raahi Menon, Ritwick Bhargava, and Sean Yang

# Installation
For installation instructions, please see `INSTALL.txt`

# Compilation
To compile our game, run the command `make build`
To play our game, run the command `make play`
To get our docs, run the command `make docs`
To run our test suite, run the command `make test`
To clean build files, run `make clean`

# Gameplay
On running make play, the terminal will prompt you for a load file. On first
play, type nothing and hit enter. 

Use the `W` key to move upwards, `S` to move downwards, `A` to move left and 
`D` to move right. 

Use `Right Click` to pick up objects and interact with exit stairs, use 
`left click` to use whatever item you are currently holding. To drop the item
in your current inventory slot, press `E`. Note that you cannot drop an item
where it would collid with another object, such as a **wall** or **enemy**.

When not holding an item, `left click` defaults to melee attack.

To quit the game, press `Q`. To quit and save, press `Esc`. 
This will create a new save file that stores your
last position in the game. The save file will be the json file in the saves 
whose number matches the number of files in the folder minus 1 (for example, 
when you first quit the game it will generate `1.json`, assuming you have already
run `make test` meaning two files will be in the directory). 