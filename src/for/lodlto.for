C
C PROGRAM LODLTO
C $Log:   GXAFXT:[GOLS]LODLTO.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:53:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:54:08   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lodlto.for **
C
C LODLTO.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 04-MAY-90   LOU R.   INITIAL RELEASE FOR DENMARK.
C
C PROGRAM TO CONVERT ASCII DATA FILE CONTAINING DENMARK
C SYSTEM BETS. (LOTTO)
C
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM LODLTO
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LSYSCOM.DEF'
C
	INTEGER*4  MAXDEFS
	PARAMETER (MAXDEFS=39)                !NUMBER OF BETS DEFINED
	INTEGER*4 FDB(7), CMD, COUNT, XXX, XX, RECIDX, BETPROC
	INTEGER*4 TYPCNT, SYSNR, STYP, POINTER, ST, LASTPROC, SOFF
	LOGICAL MORE,COMPLETE
C
C
	CHARACTER*1 C1RECORD(80)
	CHARACTER*3 C3RECORD(20)
	INTEGER*4 TAB(20),TAB2(20,10)
	INTEGER*4 LTOFIL(4),SYSBET(MAXDEFS),SIMPBETS(MAXDEFS)
C
	DATA SYSBET/10,10,11,11,12,12,12,13,13,14,14,15,15,15,16,
     *	  16,16,17,17,18,18,19,19,20,20,20,16,17,18,18,19,19,19,
     *	  20,20,21,22,24,24/
C
	DATA SIMPBETS/8,30,20,34,12,24,48,18,66,48,132,24,69,
     *	   1155,32,109,240,272,688,860,1368,338,1368,450,1040,
     *	   2400,16,17,18,33,19,52,84,20,80,120,176,24,120/
C
	DATA SOFF/0/
	CHARACTER*9 SYSDEF(4)
	EQUIVALENCE (C1RECORD(1),TAB(1),C3RECORD(1))
	DATA SYSDEF/'UNDEFINED','FULL     ',
     *	            'REDUCED  ','CHANCE   '/
	DATA LTOFIL/'XXXX',':LTO','SYS.','FIL '/
C
	CALL COPYRITE
C
C
	TYPE *,'<<<<<<<<<< LODLTO 01.00 >>>>>>>>>>'
	LASTPROC=0
C
	CALL ROPEN('LODLTO.REP',7,ST)
	IF(ST.NE.0) THEN
	  TYPE*,' LODLTO.REP OPEN ERROR ',ST
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	OPEN(4,FILE='FILE2.TXT',IOSTAT=ST,STATUS='OLD',SHARED     )
	IF(ST.NE.0)THEN
	  TYPE *,'FILE2.TXT OPEN ERROR ',ST
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	CALL WIMG(5,'Enter volume name of LTOSYS.FIL ')
	READ(5,904) LTOFIL(1)
C
C READ LOTTO SYSTEM FILE INTO MEMORY
C
	CALL OPENW(2,LTOFIL,4,0,0,ST)
	IF(ST.NE.0) THEN
	  TYPE*,' LTOSYS.FIL OPEN ERROR ',ST
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	CALL IOINIT(FDB,2,1*256)
	CALL READIO(FDB,1,LSYS_ATR,LSYS_COMMON_LENGTH*4,ST)
	IF(ST.NE.0) THEN
	  TYPE*,' LTOSYS.FIL READ ERROR ',ST
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	POINTER=LSYS_FREEPTR+1     !SET POINTER
	STYP=NOSYS                 !SYSTEM TYPE
C
	CALL INPNUM('Enter System # to load from: ',SYSNR,1,LSYSMAX,ST)
	IF (ST.LT.0) GO TO 20000
C
C check if system number is already defined ...
C
	IF(LSYS_ATR(SYSNR).NE.NOSYS) THEN
	   TYPE*,' System number ',SYSNR,' already defined '
	   CALL GPAUSE
	ENDIF
C
C
	DO 10000 TYPCNT=1,MAXDEFS
C
C READ ASCII REPRENTATION OF THIS BET AND LOAD INTO TABLE
C
	BETPROC=0
	RECIDX=1
150	CONTINUE
	COMPLETE=.FALSE.
	MORE=.FALSE.
	READ(4,901) (C1RECORD(XX),XX=1,80)
	IF(C1RECORD(6).EQ.'R'.OR.
     *	   C1RECORD(6).EQ.'C') THEN
	  STYP=LSYS_REDUCED
	  SOFF=STYP
	  IF(C1RECORD(6).EQ.'C') SOFF=SOFF+1
	  WRITE(5,902) SYSDEF(SOFF+1),(C1RECORD(XXX),XXX=1,15)
	  GOTO 150
	ENDIF
	IF(C3RECORD(1).EQ.'***') THEN   !THIS BET COMPLETE LOAD IT
	  COMPLETE=.TRUE.
	  GOTO 250
	ENDIF
	IF(C3RECORD(1).EQ.'END') GOTO 10000
	IF(C3RECORD(1).EQ.'   ') THEN
	  MORE=.TRUE.     !PROCESS THIS CHUNCK FIRST
	  GOTO 250
	ENDIF
C
	DO 200 COUNT=1,20
	TAB2(COUNT,RECIDX)=TAB(COUNT)
200	CONTINUE
C
	RECIDX=RECIDX+1
	GOTO 150
C
250	CONTINUE
	CALL DECLTO(TAB2,RECIDX-1,SYSBET(TYPCNT),POINTER,BETPROC)
	IF(COMPLETE) THEN
	  LSYS_PTR(SYSNR)=LSYS_FREEPTR+1
	  LSYS_FREEPTR=POINTER-1
	  LSYS_NUMBET(SYSNR)=BETPROC
	  LSYS_NUMMRK(SYSNR)=SYSBET(TYPCNT)
	  LSYS_GAME(SYSNR)=1
	  LSYS_ATR(SYSNR)=STYP
	  LSYS_BOARD(SYSNR)=SIMPBETS(TYPCNT)
	  WRITE(5,903) SYSNR,SYSDEF(SOFF+1),SYSBET(TYPCNT),
     *	     SIMPBETS(TYPCNT),BETPROC
	  WRITE(7,903) SYSNR,SYSDEF(SOFF+1),SYSBET(TYPCNT),
     *	     SIMPBETS(TYPCNT),BETPROC
	  SYSNR=SYSNR+1
	  RECIDX=1
C
C MAKE SURE NEXT IS AVAILABLE
C
	  IF(LSYS_ATR(SYSNR).NE.NOSYS) THEN
	     TYPE*,' System number ',SYSNR,' already defined '
	     CALL GPAUSE
	  ENDIF
	  GOTO 10000
	ENDIF
	RECIDX=1
	GOTO 150
C
C
10000	CONTINUE
20000	CONTINUE
C
	CALL USRCLOS1(     7)
	CALL SPOOL('LODLTO.REP',1,ST)
	TYPE*,' LODLTO.REP HAS BEEN SPOOLED '
	TYPE *,' '
C
	TYPE*,' Please check report prior to saving image to file '
	TYPE*,' '
C
600	CONTINUE
	TYPE *,' 1 - save image in the file'
	TYPE *,' E - to abort, i.e., do not save image to file '
	CALL INPNUM('Enter command: ',CMD,1,1,ST)
	IF(ST.LT.0) THEN
	  CALL CLOSEFIL(FDB)
	  TYPE *,IAM(),'LODLTO aborted image not saved'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	IF(CMD.EQ.1) GOTO 700
	TYPE *,'Invalid'
	GOTO 600
C
C     UPDATE LTOSYS.FIL
C
700	CONTINUE
	CALL WRITEIO(FDB,1,LSYS_ATR,LSYS_COMMON_LENGTH*4,ST)
	IF (ST.NE.0) THEN
	   TYPE*,' LTOSYS.FIL WRITE ERROR ',ST
	   TYPE *,'LODLTO aborted, write error '
	   TYPE *,IAM(),'LTOSYS.FIL not updated'
	   CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	CALL CLOSEFIL(FDB)
	TYPE *,IAM(),'LODLTO complete, LTOSYS.FIL updated '
	CALL GSTOP(GEXIT_SUCCESS)
C
C THE FOLLOWING FORMATS ARE USED.
C
901	FORMAT(80A1)
902	FORMAT(1X,' PROCESSING ',A9,' BET ',15A1)
903	FORMAT(1X,' CENTRAL SYSTEM # ',I2,' CONVERTED IN COMMON ',/,
     *	   1X,' DEFINED AS ',A9,' SYSTEM  ',I2,'/',I5,/,
     *	   1X,' TRANSLATED FROM ',I5,' ASCII TABLES ',//)
904	FORMAT(A4)
	END
