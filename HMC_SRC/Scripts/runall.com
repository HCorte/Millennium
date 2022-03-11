$!Script to run after finishing stopsys and before runtst/runsys
$!runs taxmng, soupfm, soupwin, tmir, qliktran, millrecon
$ SAY :== WRITE SYS$OUTPUT
$ script_origin = 1
!$ DEFINE/USER_MODE SYS$INPUT SYS$COMMAND
$ SAY "Start Soupfm process"
$ RUN GXTSK:SOUPFM
7592
PRIM:MTMF01.FIL
WORK:UPURGE.FIL
T
E
$ SAY "End Soupfm process"
$ SAY "Start Millrecon process"
$ RUN GXTSK:MILLRECON
$ SAY "End Millrecon process"
$ SAY "Start Qliktran process"
$ RUN GXTSK:QLIKTRAN
$ SAY "End Qliktran process"