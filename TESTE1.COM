$SET VERIFY
$SET PREFIX "(!5%T) "
$SHOW SYMBOL dfor_v2
$WRITE SYS$OUTPUT "first parameter is ''P1' (parameter from symbol)
$WRITE SYS$OUTPUT "second parameter is ''P2' (parameter from the dc )"
$WRITE SYS$OUTPUT "third  parameter is ''P3' (parameter from the dc ) 
$@teste2 'P1'
