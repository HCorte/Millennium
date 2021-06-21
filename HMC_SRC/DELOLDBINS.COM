$! starts with removing the extra COMOLM in the current directory 
$ PROCESS = "OLM"
$ JUMPTO=1
$ OPEN/WRITE HFILE GXOLM:TESTAUX.DAT
$! read all the results from the search
$ NUMFILES = 0
$LOOPS:
$ FILE = F$SEARCH("COMOLM.EXE;*") 
$! WRITE SYS$OUTPUT FILE
$ WRITE/ERROR=WRITE_ERROR HFILE FILE
$ IF FILE .EQS. "" THEN GOTO SEARCHEND
$ NUMFILES = NUMFILES+1
$ GOTO LOOPS
$SEARCHEND:
$ CLOSE HFILE
$! WAIT 00:00:01
$ OPEN/READ HRFILE GXOLM:TESTAUX.DAT
$read_loop:
$ read/end_of_file=done_loop HRFILE opt1
$! WRITE SYS$OUTPUT "''opt1'"
$ goto read_loop
$! WAIT 00:00:01
$!dir COMOLM.EXE;0
$!file = f$search("COMOLM.EXE")
$!sh time
$!READ SYS$INPUT 
$done_loop:
$ close HRFILE
$ WRITE SYS$OUTPUT NUMFILES
$ IF NUMFILES .GT. 2 
$ THEN
$   INQUIRE OPTION "There are more then 2 ''PROCESS' remove the extra ones (Y/N)?" 
$   WRITE SYS$OUTPUT OPTION
$   IF OPTION .EQ. "Y" .OR. OPTION .EQ. "y" 
$   THEN
$     WRITE SYS$OUTPUT "so is it (OLM or IGS)???---> ''PROCESS'"
$     WRITE SYS$OUTPUT (PROCESS .EQ. "OLM") 
$     IF PROCESS .EQS. "OLM" 
$     THEN
$       PURGE COMOLM.EXE /log/keep=2
$       WRITE SYS$OUTPUT "******Removed OLM files with Success***"
$       PROCESS="IGS"
$       JUMPTO=1
$     ELSE
$       IF PROCESS .EQS. "IGS" 
$       THEN
$           WRITE SYS$OUTPUT "will purge IGS files"
$!           SET DEF GXIGS
$           PURGE GXIGS:INIGS.EXE /log/keep=2
$           PURGE GXIGS:OUTIGS.EXE /log/keep=2
$           PURGE GXIGS:COMIGS.EXE /log/keep=2
$           WRITE SYS$OUTPUT "******Removed IGS files with Success***"
$           JUMPTO=3
$           PROCESS="UTL"
$       ELSE
$           IF PROCESS .EQS. "UTL" 
$           THEN
$               WRITE SYS$OUTPUT "will purge UTL files"
$               PURGE GXUTL:AGTSUP.EXE /log/keep=2 
$               PURGE GXUTL:ASYNC_ASF.EXE /log/keep=2 
$               PURGE GXUTL:BSIM.EXE /log/keep=2 
$               PURGE GXUTL:BWTEST.EXE /log/keep=2 
$               PURGE GXUTL:CHECKLIST.EXE /log/keep=2 
$               PURGE GXUTL:COMMGT.EXE /log/keep=2 
$               PURGE GXUTL:CPYMTMF.EXE /log/keep=2 
$               PURGE GXUTL:DISP_SSC.EXE /log/keep=2 
$               PURGE GXUTL:DMPTOOL.EXE /log/keep=2 
$               PURGE GXUTL:EUROTMIR.EXE /log/keep=2 
$               PURGE GXUTL:FIXOPS.EXE /log/keep=2 
$               PURGE GXUTL:FIXTCF.EXE /log/keep=2 
$               PURGE GXUTL:FIXTMF.EXE /log/keep=2 
$               PURGE GXUTL:FIXVPF.EXE /log/keep=2 
$               PURGE GXUTL:GENAGTMIL.EXE /log/keep=2 
$               PURGE GXUTL:GENCODS.EXE /log/keep=2 
$               PURGE GXUTL:GNPASSAP.EXE /log/keep=2 
$               PURGE GXUTL:IGSSIM.EXE /log/keep=2 
$               PURGE GXUTL:IPSSIM.EXE /log/keep=2 
$               PURGE GXUTL:LOAD_TEXT.EXE /log/keep=2 
$               PURGE GXUTL:LODSPTSYS.EXE /log/keep=2 
$               PURGE GXUTL:LSIM.EXE /log/keep=2 
$               PURGE GXUTL:MAKPAS.EXE /log/keep=2 
$               PURGE GXUTL:MODOPS.EXE /log/keep=2 
$               PURGE GXUTL:PDATE.EXE /log/keep=2 
$               PURGE GXUTL:PROMORPT.EXE /log/keep=2 
$               PURGE GXUTL:RESULTREP.EXE /log/keep=2 
$               PURGE GXUTL:SCANDAYS.EXE /log/keep=2 
$               PURGE GXUTL:SCANVPF.EXE /log/keep=2 
$               PURGE GXUTL:SETDEBUG.EXE /log/keep=2 
$               PURGE GXUTL:SETSUPWEL.EXE /log/keep=2 
$               PURGE GXUTL:SETTEST.EXE /log/keep=2 
$               PURGE GXUTL:SETTKF.EXE /log/keep=2 
$               PURGE GXUTL:SETTRACE.EXE /log/keep=2 
$               PURGE GXUTL:SPY.EXE /log/keep=2 
$               PURGE GXUTL:STOPSIM.EXE /log/keep=2 
$               PURGE GXUTL:SWINS.EXE /log/keep=2 
$               PURGE GXUTL:SYSFILES.EXE /log/keep=2 
$               PURGE GXUTL:TERSIM.EXE /log/keep=2 
$               PURGE GXUTL:TESTLASTJCKPOTDRW.EXE /log/keep=2 
$               PURGE GXUTL:TSTGAM.EXE /log/keep=2 
$               PURGE GXUTL:TSTLOG.EXE /log/keep=2 
$               PURGE GXUTL:TSTVAL.EXE /log/keep=2 
$               PURGE GXUTL:TVERIFY.EXE /log/keep=2 
$               PURGE GXUTL:UPDASF_GSC.EXE /log/keep=2 
$               PURGE GXUTL:UPDPAS8_ASF.EXE /log/keep=2 
$               PURGE GXUTL:VALDIS.EXE /log/keep=2 
$               PURGE GXUTL:VALSTS.EXE /log/keep=2 
$               PURGE GXUTL:VSYSTAP.EXE /log/keep=2 
$               WRITE SYS$OUTPUT "******Removed UTL files with Success***"
$               JUMPTO=4
$               PROCESS="TSK"
$           ELSE
$               IF PROCESS .EQS. "TSK" 
$               THEN 
$                   WRITE SYS$OUTPUT "will purge TSK files"
$                   JUMPTO=2
$               ENDIF   
$           ENDIF
$       ENDIF
$     ENDIF
$   ELSE
$       WRITE SYS$OUTPUT "Files where not removed then exit"     
$       JUMPTO=2     
$   ENDIF
$ ELSE 
$   WRITE SYS$OUTPUT "will change to different directory: ''PROCESS'"
$   IF PROCESS .EQS. "OLM" THEN PROCESS="IGS"
$   IF PROCESS .EQS. "IGS" THEN PROCESS="UTL"
$   IF PROCESS .EQS. "UTL" THEN PROCESS="TSK"
$ ENDIF
$
$ WRITE SYS$OUTPUT "   There Will be a jumpto: ''JUMPTO'"
$ IF JUMPTO .EQ. 1 THEN GOTO remove_igs
$ IF JUMPTO .EQ. 2 THEN GOTO exit_step
$ IF JUMPTO .EQ. 3 THEN GOTO remove_lgs
$ IF JUMPTO .EQ. 4 THEN GOTO remove_tsk
$
$exit_step:
$ EXIT
$
$WRITE_ERROR:
$ CLOSE HFILE 
$ WRITE SYS$OUTPUT "******Finished with Error in Write***"
$ EXIT
$!
$!Remove now the extra INIGS, OUTIGS & COMIGS in GXIGS
$remove_igs:
$ OPEN/WRITE HFILE GXIGS:TESTAUX.DAT
$ NUMFILES = 0
$LOOPSEARCH:
$ FILE = F$SEARCH("GXIGS:INIGS.EXE;*") 
$ WRITE SYS$OUTPUT FILE
$ WRITE/ERROR=WRITE_ERROR HFILE FILE
$ IF FILE .EQS. "" THEN GOTO SEARCHENDIGS
$ NUMFILES = NUMFILES+1
$ GOTO LOOPSEARCH
$SEARCHENDIGS:
$ CLOSE HFILE
$ OPEN/READ HRFILE GXIGS:TESTAUX.DAT
$read_loopigs:
$ read/end_of_file=done_loop HRFILE opt1
$ goto read_loopigs
$
$
$remove_lgs:
$ OPEN/WRITE HFILE GXUTL:TESTAUX.DAT
$ NUMFILES = 0
$LOOPSEARCHUTL:
$ FILE = F$SEARCH("GXUTL:AGTSUP.EXE;*") 
$ WRITE SYS$OUTPUT FILE
$ WRITE/ERROR=WRITE_ERROR HFILE FILE
$ IF FILE .EQS. "" THEN GOTO SEARCHENDUTL
$ NUMFILES = NUMFILES+1
$ GOTO LOOPSEARCHUTL
$SEARCHENDUTL:
$ CLOSE HFILE
$ OPEN/READ HRFILE GXUTL:TESTAUX.DAT
$read_looputl:
$ read/end_of_file=done_loop HRFILE opt1
$ goto read_looputl
$
$
$remove_tsk:
$ OPEN/WRITE HFILE GXTSK:TESTAUX.DAT
$ NUMFILES = 0
$LOOPSEARCHTSK:
$ FILE = F$SEARCH("GXTSK:ABPAUDIT.EXE;*") 
$ WRITE SYS$OUTPUT FILE
$ WRITE/ERROR=WRITE_ERROR HFILE FILE
$ IF FILE .EQS. "" THEN GOTO SEARCHENDTSK
$ NUMFILES = NUMFILES+1
$ GOTO LOOPSEARCHTSK
$SEARCHENDTSK:
$ CLOSE HFILE
$ OPEN/READ HRFILE GXTSK:TESTAUX.DAT
$read_looptsk:
$ read/end_of_file=done_loop HRFILE opt1
$ goto read_looptsk



