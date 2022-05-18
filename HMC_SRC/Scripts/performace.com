$ verify = 'f$verify(0)
$
$! Get initial values for stats (this removes SPAWN overhead or the current
$! process values).
$
$ bio1 = f$getjpi (0, "BUFIO")
$ dio1 = f$getjpi (0, "DIRIO")
$ pgf1 = f$getjpi (0, "PAGEFLTS")
$ vip1 = f$getjpi (0, "VIRTPEAK")
$ wsp1 = f$getjpi (0, "WSPEAK")
$ dsk1 = f$getdvi ("sys$disk:","OPCNT")
$ tim1 = f$time ()
$
$ set noon
$ tik1 = f$getjpi (0, "CPUTIM")
$ set noverify
$
$! User command being timed:
$
$ 'p1' 'p2' 'p3' 'p4' 'p5' 'p6' 'p7' 'p8'
$
$ tik2 = f$getjpi (0, "CPUTIM")
$
$ bio2 = f$getjpi (0, "BUFIO")
$ dio2 = f$getjpi (0, "DIRIO")
$ pgf2 = f$getjpi (0, "PAGEFLTS")
$ vip2 = f$getjpi (0, "VIRTPEAK")
$ wsp2 = f$getjpi (0, "WSPEAK")
$ dsk2 = f$getdvi ("sys$disk:","OPCNT")
$ tim2 = f$time ()
$
$ tim = f$cvtime("''f$cvtime(tim2,,"TIME")'-''f$cvtime(tim1,,"TIME")'",,"TIME")
$ thun = 'f$cvtime(tim,,"HUNDREDTH")
$ tsec = (f$cvtime(tim,,"HOUR")*3600) + (f$cvtime(tim,,"MINUTE")*60) + -
    f$cvtime(tim,,"SECOND")
$
$ bio = bio2 - bio1
$ dio = dio2 - dio1
$ pgf = pgf2 - pgf1
$ dsk = dsk2 - dsk1
$ vip = ""
$ if vip2 .le. vip1 then vip = "*" ! Asterisk means didn't change (from parent)
$ wsp = ""
$ if wsp2 .le. wsp1 then wsp = "*"
$
$ tiks = tik2 - tik1
$ secs = tiks / 100
$ huns = tiks - (secs*100)
$ write sys$output ""
$!
$ time$line1 == -
f$fao("Execution (CPU) sec!5UL.!2ZL Direct I/O !7UL Peak working set!7UL!1AS", -
secs, huns, dio, wsp2, wsp)
$ write sys$output time$line1
$!
$ time$line2 == -
f$fao("Elapsed (clock) sec!5UL.!2ZL Buffered I/O!7UL Peak virtual !7UL!1AS", -
tsec, thun, bio, vip2, vip)
$ write sys$output time$line2
$!
$ time$line3 == -
f$fao("Process ID !AS SYS$DISK I/O!7UL Page faults !7UL", -
f$getjpi(0,"pid"), dsk, pgf)
$ write sys$output time$line3
$ if wsp+vip .nes. "" then write sys$output -
" (* peak from parent)"
$ write sys$output ""
$
$! Place these output lines in the job logical name table, so the parent
$! can access them (useful for batch jobs to automate the collection).
$
$ define /job/nolog time$line1 "''time$line1'"
$ define /job/nolog time$line2 "''time$line2'"
$ define /job/nolog time$line3 "''time$line3'"
$
$ verify = f$verify(verify)