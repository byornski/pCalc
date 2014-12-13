pCalc
=====

Fortran command line calculator which parses tokens into Reverse Polish Notation and evaluates it. Can take equations to be parsed from stdin or the command line:-

    [byornski ~/pCalc]$ ./pCalc
    Input : 3+4+5*17
    Result:  92
    Input : 5^200
    Result:  62230152778611417071440640537801242405902521687211671331011166147896988340353834411839448231257136169569665895551224821247160434722900390625
    Input : 2+4*5
    Result:  22
    Input : (2+4)*5
    Result:  30
    Input : 17 / 3
    Result:  5
    
    [byornski ~/pCalc]$ ./pCalc 4 + 5
    9
    [byornski ~/pCalc]$ ./pCalc "17^20"
    4064231406647572522401601


Uses a bigint library of my own creation. The values are unbounded (up to 10000000^(int64_huge)) and you will run out of memory before you hit this.



Operations
----------
Currently implemented operators are:

+ Add 	  	      +
+ Subtract              -
+ Multiply	      *
+ Integer Divide	      /
+ Mod                   %
+ Exponentiation	      ^

Unlimited parenthesis levels are allowed as the string are evaluated in reverse polish notation. 


Building
--------
Simplying running

make

should give a copy of pCalc in the root folder. The source files are stored in src/ and temporary build files in obj/. There is a shell script in src to generate the dependencies lists for make so these do not need to be specified manually beyond the use statement in the .f90 file. 



Requirements
------------
You need a reasonably up to date version of gfortran or ifort. Builds with GCC 4.9.1 but does not with GCC 4.4.7. Also builds properly with Intel 14.0 compiler (ifort). 

