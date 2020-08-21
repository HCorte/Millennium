$ name = f$getjpi("","TERMINAL")
$ name = "ewatch_''name'"
$ ctx = ""
$ t = f$context("PROCESS", ctx, "PRCNAM", "''name'", "EQL")
$try_again:
$ running = (f$pid(ctx) .nes. "") 
$ if running 
$ then
$       write sys$output "Stopping EWATCH process ''name'" 
$       stop 'name'
$       wait 00:00:00.50
$       goto try_again
$ endif
$ if p1 .nes. "STOP"
$ then
$    write sys$output "Starting EWATCH process ''name'"
$    spawn/nowait/nolog/input=nl:/proc='name' RUN GXTSK:EWATCH
$ endif
$ exit
