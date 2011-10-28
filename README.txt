Functioneel Programmeren - Turtle graphics

=== Starting ===

You can start the program bij running Graphics.hs in the haskell interpreter and running the command 'main'.

=== Using the program === 

Loading a previously made turtle graphics file can be done by pressing 'l' and typing the filename, followed by an enter.

=== Turtle graphics file ===

==== Commands =====
The language understands the following commands:

left [degrees]    - Turn left with the given amount of degrees
right [degrees]   - Turn right with the given amount of degrees
move [x] [y]      - Move to the specified x,y coordinates
forward [steps]   - Move forward with the given amount of pixels. This may be a float.
penup / pendown   - Put the pen up or down (every move is drawn or not)
color [R] [G] [B] - Change the color of the pen to the given RGB-value (0-255)
sleep [duration]  - Sleep the specified amount of time
clear             - Clear the screen
breakwhen [cond]  - Breaks out the current block when the value of cond is less or equal to 0

==== Blocks ====
Furthermore, there are two blocks:

do [name] :[arg1] :[arg2] ...
	[cmds]*
end

This adds a 'function' to the language. This allows subroutines to be created. The can be called with [name] [arg1] [arg2] after definition. Within the block, arguments can be referenced with :[arg1] or :[arg2], respectively.

repeat [times]
	[cmds]*
end

Repeats the given commands the specified amount of times.

==== Calculations ====
Arithmetic operations exist to do small calculations. These can be used at every place a number is expected. The following operations are supported: +, -, *, /, %, ^. Every operation must be placed between parenthesis, therefore, 3+3*3 is not correct, but (3+(3*3)) is.

==== Comments =====
Comments can be added with '#'. This is only recognized when the # is at the start of the line.

=== Examples ===
nativeEx.txt : This example shows the most primitive turtle functions such as repeat, forward and right.
modulesEx.txt : This example illustrates some basic functions the turtle can do. Do and repeat blocks, nesting and basic forward/right functions are used.
flowerEx.txt : Draws multiple squares while turning, creating something that looks like a flower.
basicRecurEx.txt : This shows a basic recursing algorithm going towards the left upper corner.
circlesEx.txt : This shows a recursing circle algorithm which doesn't fully accomplish a circle on each rotation, causing it to make a big green circle if left running long enough.
fracTreeEx.txt : This example shows recursion with a break when the size gets too small, together with calculations. The result is a realistic looking fractional tree.
snowflakeEx.txt : This example shows a Koch snowflake, recursively drawn. The level (detail) of the koch snowflake can be changed within the file. This is currently set to 5. Setting this higher than 7 is not recommended.
//An example showing a rainbow with a gradient of colors was under production, but sadly we didn't find time to finish it =(



