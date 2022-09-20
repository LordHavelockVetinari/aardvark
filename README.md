# Aardvark
Aardvark is an esoteric programming language based on asynchronous functions.
Instead of traditional control flow, Aardvark uses the <i>async/await</i> syntax to emulate loops and conditions.
In addition, Aardvark has only one data type, asynchronous <i>tasks</i> (a.k.a. <i>futures</i> or <i>promises</i>), which it uses to emulate all the other data types.

## Using the interpreter
If you're using 64-bit Windows, download [aardvark_win64.exe](./aardvark_win64.exe).
Otherwise, you'll have to compile the interpreter yourself using Cargo.<br>
To run an Aardvark program, pass it to the interpreter as a command line argument.
