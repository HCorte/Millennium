C
C V01 23-MAR-2011 RXK INCREASE X2X FILES 
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        PROGRAM CNVX2X
        IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
        INCLUDE 'INCLIB:X2XTER.DEF'
        INCLUDE 'INCLIB:X2XSTN.DEF'
        INCLUDE 'INCLIB:X2XSPC.DEF'
        INCLUDE 'INCLIB:X2XBLD.DEF'
        INCLUDE 'INCLIB:X2XGRP.DEF'
C
        INTEGER*4 ST,YESNO,REC 
        INTEGER*4 OLD_FDB(7),NEW_FDB(7)
        INTEGER*4 NEW_SIZE,OLD_SIZE
        INTEGER*4 I4NEW_FILE(5),I4OLD_FILE(5)
        INTEGER*4 ONUMAGT/6144/   !OLD NUMBER OF TERMINALS AND STATIONS
                         ! THIS EQUALS TO 
                         ! X2XSTN_RANGE,X2X_STATIONS,X2XBLD_RANGE,X2XGRP_RANGE
C
        CHARACTER*20 NEW_FILE,OLD_FILE
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
C
C X2XTER.FIL
C
        TYPE*,IAM()
        TYPE*,IAM(),'EXPANDING X2XTER.FIL'
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR X2XTER.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'SYSX:X2XTER.NEW'
        OLD_FILE = 'SYSX:X2XTER.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,OLD_SIZE)
        CLOSE(1)
C
        NEW_SIZE  = NUMAGT*X2XTER_SECT/2
        TYPE*,IAM()
        TYPE*,IAM(),'Old file size = ', ONUMAGT*X2XTER_SECT/2,'  /',OLD_SIZE
        TYPE*,IAM(),'New file size = ',  NUMAGT*X2XTER_SECT/2
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to copy >',ONUMAGT
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to expand X2XTER.FIL',YESNO)
        IF(YESNO.NE.1) GOTO 100
C
        CALL CRTFIL(I4NEW_FILE,NEW_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENX(3,NEW_FILE,0,0,0,ST)
        CALL IOINIT(NEW_FDB,3,X2XTER_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENX(4,OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,X2XTER_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        DO REC=1,ONUMAGT
           CALL READW(OLD_FDB,REC,X2XTER_REC,ST)
           IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)
           CALL WRITEW(NEW_FDB,REC,X2XTER_REC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
           IF(MOD(REC,1000).EQ.0) TYPE*,IAM(),REC,' records copied...'
        ENDDO         
C
        TYPE*,IAM(),REC-1,' records copied in total'
C
        CALL CLOSEFIL(NEW_FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming SYSX:X2XTER.FIL to SYSX:X2XTER.OLD'
        CALL LIB$RENAME_FILE('SYSX:X2XTER.FIL', 'SYSX:X2XTER.OLD')
C
        TYPE*,IAM(),'Renaming SYSX:X2XTER.NEW to SYSX:X2XTER.FIL'
        CALL LIB$RENAME_FILE('SYSX:X2XTER.NEW', 'SYSX:X2XTER.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after expansion' 
        ST = LIB$SPAWN('$ DIR X2XTER.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'X2XTER.FIL is successfully expanded!'
C
C X2XSTN.FIL
C
100     CONTINUE
        TYPE*,IAM()
        TYPE*,IAM(),'EXPANDING X2XSTN.FIL'
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR X2XSTN.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'SYSX:X2XSTN.NEW'
        OLD_FILE = 'SYSX:X2XSTN.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,OLD_SIZE)
        CLOSE(1)
C
        NEW_SIZE  = NUMAGT*X2XSTN_SECT/2
        TYPE*,IAM()
        TYPE*,IAM(),'Old file size = ', ONUMAGT*X2XSTN_SECT/2,'  /',OLD_SIZE
        TYPE*,IAM(),'New file size = ',  NUMAGT*X2XSTN_SECT/2
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to copy >',ONUMAGT
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to expand X2XSTN.FIL',YESNO)
        IF(YESNO.NE.1) GOTO 200
C
        CALL CRTFIL(I4NEW_FILE,NEW_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENX(3,NEW_FILE,0,0,0,ST)
        CALL IOINIT(NEW_FDB,3,X2XSTN_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENX(4,OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,X2XSTN_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        DO REC=1,ONUMAGT
           CALL READW(OLD_FDB,REC,X2XSTN_REC,ST)
           IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)
           CALL WRITEW(NEW_FDB,REC,X2XSTN_REC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
           IF(MOD(REC,1000).EQ.0) TYPE*,IAM(),REC,' records copied...'
        ENDDO         
C
        TYPE*,IAM(),REC-1,' records copied in total'
C
        CALL CLOSEFIL(NEW_FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming SYSX:X2XSTN.FIL to SYSX:X2XSTN.OLD'
        CALL LIB$RENAME_FILE('SYSX:X2XSTN.FIL', 'SYSX:X2XSTN.OLD')
C
        TYPE*,IAM(),'Renaming SYSX:X2XSTN.NEW to SYSX:X2XSTN.FIL'
        CALL LIB$RENAME_FILE('SYSX:X2XSTN.NEW', 'SYSX:X2XSTN.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after expansion' 
        ST = LIB$SPAWN('$ DIR X2XSTN.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'X2XSTN.FIL is successfully expanded!'
C
C X2XSPC.FIL
C
200     CONTINUE
        TYPE*,IAM()
        TYPE*,IAM(),'EXPANDING X2XSPC.FIL'
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR X2XSPC.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'SYSX:X2XSPC.NEW'
        OLD_FILE = 'SYSX:X2XSPC.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,OLD_SIZE)
        CLOSE(1)
C
        NEW_SIZE  = NUMAGT*X2XSPC_SECT/2
        TYPE*,IAM()
        TYPE*,IAM(),'Old file size = ', ONUMAGT*X2XSPC_SECT/2,'  /',OLD_SIZE
        TYPE*,IAM(),'New file size = ',  NUMAGT*X2XSPC_SECT/2
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to copy >',ONUMAGT
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to expand X2XSPC.FIL',YESNO)
        IF(YESNO.NE.1) GOTO 300
C
        CALL CRTFIL(I4NEW_FILE,NEW_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENX(3,NEW_FILE,0,0,0,ST)
        CALL IOINIT(NEW_FDB,3,X2XSPC_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENX(4,OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,X2XSPC_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        DO REC=1,ONUMAGT
           CALL READW(OLD_FDB,REC,X2XSPC_REC,ST)
           IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)
           CALL WRITEW(NEW_FDB,REC,X2XSPC_REC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
           IF(MOD(REC,1000).EQ.0) TYPE*,IAM(),REC,' records copied...'
        ENDDO         
C
        TYPE*,IAM(),REC-1,' records copied in total'
C
        CALL CLOSEFIL(NEW_FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming SYSX:X2XSPC.FIL to SYSX:X2XSPC.OLD'
        CALL LIB$RENAME_FILE('SYSX:X2XSPC.FIL', 'SYSX:X2XSPC.OLD')
C
        TYPE*,IAM(),'Renaming SYSX:X2XSPC.NEW to SYSX:X2XSPC.FIL'
        CALL LIB$RENAME_FILE('SYSX:X2XSPC.NEW', 'SYSX:X2XSPC.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after expansion' 
        ST = LIB$SPAWN('$ DIR X2XSPC.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'X2XSPC.FIL is successfully expanded!'
C
C X2XBLD.FIL
C 
300     CONTINUE
        TYPE*,IAM()
        TYPE*,IAM(),'EXPANDING X2XBLD.FIL'
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR X2XBLD.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'SYSX:X2XBLD.NEW'
        OLD_FILE = 'SYSX:X2XBLD.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,OLD_SIZE)
        CLOSE(1)
C
        NEW_SIZE  = NUMAGT*X2XBLD_SECT/2
        TYPE*,IAM()
        TYPE*,IAM(),'Old file size = ', ONUMAGT*X2XBLD_SECT/2,'  /',OLD_SIZE
        TYPE*,IAM(),'New file size = ',  NUMAGT*X2XBLD_SECT/2
        TYPE*,IAM()
        !TYPE*,IAM(),'Number of records to copy >',ONUMAGT
        !TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to expand X2XBLD.FIL',YESNO)
        IF(YESNO.NE.1) GOTO 400
C
        CALL CRTFIL(I4NEW_FILE,NEW_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENX(3,NEW_FILE,0,0,0,ST)
        CALL IOINIT(NEW_FDB,3,X2XBLD_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENX(4,OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,X2XBLD_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        DO REC=1,ONUMAGT
           CALL READW(OLD_FDB,REC,X2XBLD_REC,ST)
           IF(ST.EQ.144) GOTO 310 
           IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)
           CALL WRITEW(NEW_FDB,REC,X2XBLD_REC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
           IF(MOD(REC,1000).EQ.0) TYPE*,IAM(),REC,' records copied...'
        ENDDO         
C
310     CONTINUE
        TYPE*,IAM(),REC-1,' records copied in total'
C
        CALL CLOSEFIL(NEW_FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming SYSX:X2XBLD.FIL to SYSX:X2XBLD.OLD'
        CALL LIB$RENAME_FILE('SYSX:X2XBLD.FIL', 'SYSX:X2XBLD.OLD')
C
        TYPE*,IAM(),'Renaming SYSX:X2XBLD.NEW to SYSX:X2XBLD.FIL'
        CALL LIB$RENAME_FILE('SYSX:X2XBLD.NEW', 'SYSX:X2XBLD.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after expansion' 
        ST = LIB$SPAWN('$ DIR X2XBLD.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'X2XBLD.FIL is successfully expanded!'
C
C X2XGRP.FIL
C
400     CONTINUE
        TYPE*,IAM()
        TYPE*,IAM(),'EXPANDING X2XGRP.FIL'
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR X2XGRP.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'SYSX:X2XGRP.NEW'
        OLD_FILE = 'SYSX:X2XGRP.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,OLD_SIZE)
        CLOSE(1)
C
        NEW_SIZE  = NUMAGT*X2XGRP_SECT/2
        TYPE*,IAM()
        TYPE*,IAM(),'Old file size = ', ONUMAGT*X2XGRP_SECT/2,'  /',OLD_SIZE
        TYPE*,IAM(),'New file size = ',  NUMAGT*X2XGRP_SECT/2
        TYPE*,IAM()
        !TYPE*,IAM(),'Number of records to copy >',ONUMAGT
        !TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to expand X2XGRP.FIL',YESNO)
        IF(YESNO.NE.1) GOTO 500
C
        CALL CRTFIL(I4NEW_FILE,NEW_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENX(3,NEW_FILE,0,0,0,ST)
        CALL IOINIT(NEW_FDB,3,X2XGRP_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENX(4,OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,X2XGRP_SECT*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        DO REC=1,ONUMAGT
           CALL READW(OLD_FDB,REC,X2XGRP_REC,ST)
           IF(ST.EQ.144) GOTO 410
           IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)
           CALL WRITEW(NEW_FDB,REC,X2XGRP_REC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
           IF(MOD(REC,1000).EQ.0) TYPE*,IAM(),REC,' records copied...'
        ENDDO         
C
410     CONTINUE
        TYPE*,IAM(),REC-1,' records copied in total'
C
        CALL CLOSEFIL(NEW_FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming SYSX:X2XGRP.FIL to SYSX:X2XGRP.OLD'
        CALL LIB$RENAME_FILE('SYSX:X2XGRP.FIL', 'SYSX:X2XGRP.OLD')
C
        TYPE*,IAM(),'Renaming SYSX:X2XGRP.NEW to SYSX:X2XGRP.FIL'
        CALL LIB$RENAME_FILE('SYSX:X2XGRP.NEW', 'SYSX:X2XGRP.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after expansion' 
        ST = LIB$SPAWN('$ DIR X2XGRP.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'X2XGRP.FIL is successfully expanded!'
C
500     CONTINUE
        CALL GSTOP(GEXIT_SUCCESS)
        END

