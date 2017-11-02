# Erlang-class
Repository with all the files and tasks from the Erlang class in Tec de Monterrey CCM (Fall 2016).
You can use them as you wish in any project.

## Compile
In order to compile and use them:
1. Install Erlang.
2. Navigate to the files containing folder.
3. Type "c(ModuleName)."
4. Use "ModuleName:functionName()" to use functions.
For example:  
c(calc).  
calc:start().

## Files
IMPORTANT: In the loop-designed systems it's essential to export the loop function (although not necessary outside). 
Otherwise it's not going to compile

### my_db.erl
Database implemented as a separate looping process with client interface. Made by Sergey Pavlov and Augusto Monge.

### calc.erl
Calculator implemented as a separate looping process with client interface.

### basicFunctions.erl
Standard library functions such as len, sum, reverse, reduce and so on. Implemented with different types of recursion: usual, tail-recursion and language-specific ones.

### emc.erl
Erlang master class made after University of Kent videos.

### subscription.erl
Processes subscription.

### my_supervisor.erl
Fault tolerant application architecture.

### ring.erl & myring.erl
Ring processes architecture.

### echo.erl
Process echo sample + Merge Sort implementation.

### helloworld.erl
Sample server with hello-world website page.

### tempo
Creates a process which waits for a message from the other process. If gets no message in given time reports that.
