$ @GXCOM:SETGXPROJD
$ exec = "ELOG"
$ full_name = GXPROJD + exec
$ ctx = ""
$ t = f$context("PROCESS", ctx, "PRCNAM", "''full_name'", "EQL")
$try_again:
$ running = (f$pid(ctx) .nes. "") 
$ if p1 .eqs. "STOP"
$ then
$    if running
$    then 
$       write sys$output "Stopping ELOG process ''full_name'" 
$       stop 'full_name'
$       wait 00:00:00.50
$       goto try_again
$    endif
$    exit
$ else
$    if .not.running
$    then
$       write sys$output "Starting ELOG process ''full_name'"
$       detrun 'exec'
$    else
$       write sys$output "ELOG process ''full_name' is already running!"
$    endif
$ endif
$ exit
