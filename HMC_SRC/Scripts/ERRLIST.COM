$! error messages list for integers values
$ write sys$output f$message(1409041)
$ write sys$output f$message(18292)
$ write sys$output f$message(99018)
$ write sys$output f$message(98962)
$ write sys$output f$message(65537)
$ write sys$output f$message(1148)
$! Some severe erros for testing the handler to avoid closing image aka killing the process
$ write sys$output "--------------------------------------------------------------------------------"
$ write sys$output f$message(9)
$ write sys$output f$message(8)
$ write sys$output f$message(2)
$ write sys$output f$message(1)
$ write sys$output f$message(10)
$!
$ write sys$output "-------------------Some severe erros for testing the handler to avoid closing image aka killing the process--------------------------"
$ write sys$output "FOR$_FILNOTFOU(1605868):",f$message(1605868)
$ write sys$output "FOR$_NO_SUCDEV(1605972):",f$message(1605972)
$ write sys$output "FOR$_FILNAMSPE(1605980):",f$message(1605980)
$ write sys$output "FOR$_OPEFAI(1605876):",f$message(1605876)
$ write sys$output "FOR$_NOTFORSPE(1605644):",f$message(1605644)
$! ------
$ write sys$output "test----:",f$message(2928)
