$ IF P1.EQS. "" THEN INQUIRE P1 "Enter file name"
$ IF P1.EQS. "" THEN EXIT
$ @GXCOM:RELEASE 'P1 "FNDEVB::"
