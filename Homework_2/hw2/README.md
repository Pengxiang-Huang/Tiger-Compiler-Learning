# README

The figure shows the finite automata of my design:

<img src="Screenshot 2023-02-13 at 3.36.16 PM.png" alt="Screenshot 2023-02-13 at 3.36.16 PM" style="zoom:50%;" />

### Comments Handling 

I create a state `COMMENT` and a reference `comment_level` for handling the comments. 

The first time encountered the `/*`, it enter the comment state and increment the comment level. Then every time the program encounters a `/*` it just increments the comment level. If encountering a `*/`, check if the current comment level is `1`, if yes then go back to the `INITIAL` state. 

## String handling

**Basic logic**: encounter a quote, then enter the `STRING` state, encounter another quote then go back to the `INITIAL` state. 

**Escape Handling:** There are a lot of escape characters. I handled \n, \t, \ddd, \^c , \\", \\\, \f ... f\\. 

The handling logic is shown in my code and my FSM figure shown above. 

## Error Handling

**Unclosed Comment**: I handled the unclosed comment by checking the comment level at the end of the file, if it is not 0, then we can judge that it is an unclosed comment. 

**Unclosed String end with a newline:** whenever encounter a newline a string, report an error. But if encounter a \ \n, it will ignore the \n and find another \ then continue the string. 

**Backslash 4 digits:** I did not treat this as an error. I will treat the first three character as a char, and fourth digits as a normal digit. My assumption is that: there is some scenario that use need to use the \ddd as a character, then follow a digit. We can not report this as an error. 

**Unknown escape character:** I use the "block list" method. Anything but the given one should be treated as an unknown escape character. Basically, we first match all the possible inputs, if no one match it, we report an error. 

## End-of-file handling

The function of `eof` already handles the end-of-file situation. It will print out the location of the end of the file. 

### PS

The control character is not output as the expected one (maybe it depends on computers?), but I think it is ok to print the control character in a plain text. 

Below are my output by running the `./runsml`

<img src="Screenshot 2023-02-15 at 4.37.18 PM.png" alt="Screenshot 2023-02-15 at 4.37.18 PM" style="zoom:50%;" />