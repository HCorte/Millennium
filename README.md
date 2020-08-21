## In here are some basic examples and explanations

When you're done, you can delete the content in this README and update the file with details for others getting started with your repository.

*We recommend that you open this README in another tab as you perform the tasks below. You can [watch our video](https://youtu.be/0ocf7u76WSo) for a full demo of all the steps in this tutorial. Open the video in a new tab to avoid leaving Bitbucket.*

---

## Compile and Link all files of a project

First step is to compile indevidual each source file (.f or .for).

1. The command as follow **FORTRAN SWAP** no need to explicit say SWAP.FOR or SWAP.F  thats because the **FORTRAN** command will aumoticaly seahc for file with the extension **F or FOR** .
2. Compile its dependecies **FORTRAN ISWAP** and **FORTRAN LOGGER**
2. Now to Link a file or files for starter link one file as example **LINK SWAP** thats
the same as **LINK/EXE=SWAP SWAP** or **LINK SWAP** first and in the first step COMPILE its **implicit that the extension is OBJ in LINK process** to note that the **flag EXE=SWAP** means the generated output file that a executable file will be name SWAP explicitly if its not defined as in the second command that is implicit gets the same name of the OBJ file.
3. In case its a Main file that haves multiples dependecies it will be **LINK/EXE=SWAP SWAP, ISWAP, LOGGER** the SWAP exe file generated will include its dependecies of ISWAP and LOGGER

---

## How to Debug a Fortran Project

The following steps are for a generic debug but also for a Millennium debug
**To debug a project the compilation and linkage will have a flag of debug**

1. To compile a file now is **FORTRAN/DEBUG/NOOPTIMIZE SWAP** or **FORTRAN/DEBUG/NOOPTIMIZE SWAP.FOR** 
2. To link all files of a project are now **LINK/DEBUG/EXE=SWAP SWAP,ISWAP,LOGGER** or **LINK/DEBUG/EXE=SWAP.EXE SWAP.OBJ,ISWAP.OBJ,LOGGER.OBJ**

## Now the steps needed for in Millenium Project to be Compiled and Linked

3. To compile a file in *Millenium Fortran* its needed to run symbol DFOR (DFOR == "@DSS$GTECH:DSS_FOR * ") 
    3.1 To get its value was use the dcl: *sh symb dfor* 
    3.2 The @ at start means it a **COMMAND PROCEDURE** thats DSS_FOR that its extension is .COM that is implicit when running a *@COMMAND PROCEDURE* 
    3.3 DSS$GTECH is a **Logical Name** that ist value is obtain by the following dcl: sh log DSS$GTECH
    Its value is: "DSS$GTECH" = "SYS$SYSDEVICE:[GTECHCOMMANDS.DEVELOPMENT.DSS]" (LNM$SYSTEM_TABLE)
4. To LINK a file in *Millenium Fortran* its needed to run symbol DLNK (DLNK == "@DSS$GTECH:DSS_LNK") 
    4.1 To get its value was use the dcl: *sh symb dlnk*
    3.2 The @ at start means it a **COMMAND PROCEDURE** thats DSS_LNK that its extension is .COM that is implicit when running a *@COMMAND PROCEDURE* 
    3.3 DSS$GTECH is a **Logical Name** that ist value is obtain by the following dcl: sh log DSS$GTECH
    Its value is: "DSS$GTECH" = "SYS$SYSDEVICE:[GTECHCOMMANDS.DEVELOPMENT.DSS]" (LNM$SYSTEM_TABLE)
5. *COMMAND PROCEDURE* is nothing more then a dcl script (command lines that run in sequence) at 

## Now the steps needed for in Millenium Project to be Compiled and Linked in Debug Mode

1. The same steps above the only diference will a v2 of its **COMMAND PROCEDURES** that will be
DSS_FORV2 and DSS_LNKV2 that have some changes where a extra parameter to indicate if its in debug mode or not

---

Now that you're more familiar with your Bitbucket repository, go ahead and add a new file locally. You can [push your change back to Bitbucket with SourceTree](https://confluence.atlassian.com/x/iqyBMg), or you can [add, commit,](https://confluence.atlassian.com/x/8QhODQ) and [push from the command line](https://confluence.atlassian.com/x/NQ0zDQ).
