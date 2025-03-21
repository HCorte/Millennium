C  GXSRC:CNVDKK.FOR
C
C V03 23-MAR-2011 RXK Added DIR command before and after conversion
C V02 19-FEB-2004 FRP EuroMillions: recalculate record according to MAXGAM=50.
C CONVERT DKKPOL FROM I4 TO I8
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
        PROGRAM CNVDKK
        IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DKKREC_OLD.DEF'
C
C
        INTEGER*4   DRAW
        INTEGER*4   ST, OLD_FDB(7), FDB(7)
        INTEGER*4   YESNO
	INTEGER*4   MAXDRW_CNV		! MAX NUMBER OF DRAWS IN JOKER FILE.
C
	INTEGER*4   FILE_SIZE		! NEW FILE SIZE
	CHARACTER*20 NEW_FILE,OLD_FILE
	INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
	EQUIVALENCE (NEW_FILE,I4NEW_FILE)
	EQUIVALENCE (OLD_FILE,I4OLD_FILE)
	INTEGER*4   SIZE
	INTEGER*4   MAXGAM_OLD
	PARAMETER(MAXGAM_OLD=10)
	REAL*8      UINT
	EXTERNAL    UINT
C
        CALL COPYRITE
C
C
        TYPE*,IAM()
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR J1F.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
	NEW_FILE = 'FILE:J1F.NEW'
	OLD_FILE = 'FILE:J1F.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
	MAXDRW_CNV = SIZE/(ODKKSEC/2) 
	FILE_SIZE  = MAXDRW_CNV*DKKSEC/2
	TYPE*,IAM(),'Old file size = ', MAXDRW_CNV*ODKKSEC/2
	TYPE*,IAM(),'New file size = ', MAXDRW_CNV*DKKSEC/2
	TYPE*,IAM()
	TYPE*,IAM(),'Number of draws to convert >',MAXDRW_CNV
	TYPE*,IAM()
	CALL PRMYESNO('Are you sure you want to convert JOKER file',YESNO)
	IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        CALL CRTFIL(I4NEW_FILE,FILE_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENW(3,I4NEW_FILE,0,0,0,ST)
        CALL IOINIT(FDB,3,DKKSEC*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENW(4,I4OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,ODKKSEC*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        TYPE*,IAM(),'STARTING...'
C
C       Read record from file
C
        DRAW=0
        DO WHILE(ST.EQ.0)
	   DRAW = DRAW + 1
           CALL READW(OLD_FDB,DRAW,ODKKREC,ST)
	   IF(ST.EQ.144) GOTO 100
           IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,DRAW)
C
	   CALL FASTSET(0,DKKREC,DKKRECLEN)
C
	   DKKSTS = ODKKSTS
	   DKKCTM = ODKKCTM
	   DKKTIM = ODKKTIM
	   DKKDRW = ODKKDRW
	   DKKBSD = ODKKBSD
	   DKKESD = ODKKESD
	   DKKPUP = ODKKPUP
	   DKKUPD = ODKKUPD
	   CALL FASTMOV(ODKKDAT,DKKDAT,DATLEN)
	   CALL FASTMOV(ODKKADV,DKKADV,NUMADV)
	   CALL FASTMOV(ODKKSAL,DKKSAL,KIGENT*MAXGAM_OLD)
	   CALL FASTMOV(ODKKSHR,DKKSHR,KIGDIV)
	   CALL FASTMOV(ODKKPAD,DKKPAD,KIGDIV)
	   CALL FASTMOV(ODKKPRG,DKKPRG,KIGDIV)
	   CALL FASTMOV(ODKKSHV,DKKSHV,KIGDIV)
	   CALL FASTMOV(ODKKPOL,DKKPOL,2*KIGDIV)
	   CALL FASTMOV(ODKKBRK,DKKBRK,KIGDIV)
	   CALL FASTMOV(ODKKFRZ,DKKFRZ,KIGDIV)
	   DKKTAX = ODKKTAX
	   CALL FASTMOV(ODKKRES,DKKRES,2)
	   DKKWIN = ODKKWIN
	   DKKHLD = ODKKHLD
	   DKKSER = ODKKSER
	   DKKPRC = ODKKPRC
	   DKKMAX = ODKKMAX
	   DKKDIV = ODKKDIV
	   DKKSPR = ODKKSPR
	   CALL FASTMOV(ODKKMAT,DKKMAT,3*KIGDIV)
	   CALL FASTMOV(ODKKPER,DKKPER,KIGDIV)
	   CALL FASTMOV(ODKKTSR,DKKTSR,KIGDIV)
	   DKKREV = ODKKREV
	   CALL FASTMOV(ODKKSED,DKKSED,2)
	   DKKOCT = ODKKOCT
	   DKKMLT = ODKKMLT
	   CALL FASTMOV(ODKKWRF,DKKWRF,10)
	   CALL FASTMOV(ODKKASH,DKKASH,KIGDIV)
	   DKKMIN = ODKKMIN
	   DKKDFF = ODKKDFF
           CALL MOVBYT(ODKKMDS,1,DKKMDS,1,MAXMLTD_AVL)
	   CALL FASTMOV(ODKKBAL,DKKBAL,MAXDRW)
	   DKKSPL = ODKKSPL
C
C          Write record to file
C
           CALL WRITEW(FDB,DRAW,DKKREC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,DRAW)

	   IF(MOD(DRAW,1000).EQ.0) TYPE*,IAM(),DRAW,' records converted...'
        ENDDO
100	CONTINUE
C
	TYPE*,IAM(),DRAW-1,' records converted in total.'
C
	CALL CLOSEFIL(FDB)
	CALL CLOSEFIL(OLD_FDB)
C
	TYPE*,IAM(),'Renaming FILE:J1F.FIL to FILE:J1F.OLD'
	CALL LIB$RENAME_FILE('FILE:J1F.FIL','FILE:J1F.OLD')

	TYPE*,IAM(),'Renaming FILE:J1F.NEW to FILE:J1F.FIL'
	CALL LIB$RENAME_FILE('FILE:J1F.NEW','FILE:J1F.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after conversion:'
        ST = LIB$SPAWN('$ DIR J1F.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'JOKER game file converted succesfully!'
	CALL GSTOP(GEXIT_SUCCESS)
        END

