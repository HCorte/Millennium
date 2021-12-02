$! test script
$SAY           :== WRITE SYS$OUTPUT
$SAY "Start"
$sh symbol exists
$exists :== "Henrique B."
$sh symbol exists
$SAY "End"