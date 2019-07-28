---test normal mode
new cuts add to the back AND are selected
new pastes do nothing

One
Two
Three
four
five
six

-- Build mode
new cuts add to the back only, selection does not change
new pastes do nothing


-- Test Static mode
new cuts do nothing
new pastes do nothing

-- Test Queue mode
new cuts to the back
new pastes delete the head and set clipboard to new head

-- Test Advance mode
new cuts to the back
new pastes advance the queue without changing it

One TwoThreeFourFiveFiveFive

--

test path behavior 

run executable from anywhere - default path should always be ~/queue.txt

run executable from anywhere with path, ensure that path is opened
