# ClipQueue

This is a terminal ui based clipboard manage which features several different operational modes for workflow automation.

Commands
clipqueue [mode [filename] ]
Modes:
n | normal - Default behaviour if no mode is specified. New clipboard actions triggered by keyboard shortcuts will automatically be recorded and added to the queue. New clipboard items will be set as the current clipboard item.
b | build - New cuts will not affect the current clipboard item, but will be stored for later use
s | static - Static mode does not monitor new clipboard activity, however you can set the current clipboard item
q | queue - Queue mode treats your stored clipboard as a queue, you will only paste from the top, and only cut or copy items to the bottom. When you paste, the topmost item will be removed and your clipboard selection will be moved to the next item
a | advance - Advance Mode does not remove selections, but is similar to Queue mode in that when you paste, your selection will advance automatically
the filename argument is used to choose an alternative filepath to use for the queue. the default is ~/queue.txt, you can customize your own queue file, use newlines to seperate the content, for multiline clips, you can use the omega (Ω) character

Controls:
Up/Down Arrow keys - change selection, setting the current clipboard item
j & k keys will also behave as up & down, similar to Vim
m - rotates through the various modes during runtime.
t & b keys will select the first and last items in the queue
q, ctrl-z, ctrl-c - quit
enter - resets the clipboard to the current item in case of some other clipboard interferenc
(outside of Clipqueue), use ctrl+c, ctrl+x, and ctrl+v as you would normally, Clipqueue will monitor these keys and pull from your clipboard automatically, saving to the queue file as new items are recovered

ClipQueue only supports text clipboard items

## development setup

this is fully hackable and you can get the source code ready to edit and run by following these steps

1. Install node + npm
2. Install Haskell Stack
3. in ./CQui run `stack build`
4. in ./CQKeyListener run `npm install`
5. to build binaries, run `sh ./build.hs`
