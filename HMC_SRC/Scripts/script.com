$!example
$!DEFINE/USER_MODE SYS$INPUT SYS$COMMAND
$ script_origin = 1
$ write sys$output script_origin
$ RUN PROGRAM_EXAMPLE
good
well
