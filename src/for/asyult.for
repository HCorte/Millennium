C
C PROGRAM ASYULT
C $Log:   GXAFXT:[GOLS]ASYULT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:14:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   06 Oct 1993 18:11:20   LMK
C  Initial revision.
C  
C
C ** Source - ASYULT.for;1 **
C
C ASYULT.FOR
C
C V02 08-JAN-92 DAS DISABLED RELAYS  (NOT PERTINENT TO REMOTE STATIONS)        
C V01 01-DEC-91 XXX RELEASED FOR VAX (NETHERLANDS)
C
C START RELAY BROADCASTING FUNCTIONS
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
C Copyright 1990 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM ASYULT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'
C
	INTEGER*4 PROBUF /0/,POLL_STS 
	INTEGER*4 ST, NOT_ALL, FORMAT
        INTEGER*4 PROCESS, MES_NUM, OUT_LEN, MES_DEST
        INTEGER*4 MAX_GROUPS, GROUP, TMP_OPTION
	INTEGER*4 MES_BUF(32)
	INTEGER*4 OPTION
	INTEGER*4 BROIDX, BROTYPE        ! all(-1), stn or grp idx/type
        INTEGER*4 GRPCNT, SAV_GROUP, RESET_TYPE
	LOGICAL   NXTSTN
C
	CALL COPYRITE
C
        TYPE *
        TYPE *,'<<<<< ASYULT X2X Broadcasting Facility >>>>>'
        TYPE *
C
10	CONTINUE
	GRPCNT=0
	BROTYPE=0
	SAV_GROUP=0
	RESET_TYPE=0
	IF (PROBUF.LE.0) THEN
	   CALL GETBUF(PROBUF)
	   IF (PROBUF.LE.0) THEN
	      TYPE *,'could not get a buffer ',PROBUF
	      CALL XWAIT(3,2,ST)
	      GOTO 10
	   ENDIF
	ENDIF
C

C START RELAY PROCESS.
C
100	CONTINUE
	NOT_ALL=0
	MAX_GROUPS=X2X_NUM_GROUPS
	FORMAT=X2XR_APPA_CHAIN
	NXTSTN=.FALSE.
C
        FORMAT=X2XR_APPA_ALL_NO_FORMAT
        MAX_GROUPS=X2X_STATIONS
C
        BROTYPE = 3
        GROUP=-1
        BROIDX=-1
        TMP_OPTION = 8
        OPTION=TMP_OPTION+1
        POLL_STS = 1
        RESET_TYPE=-HRESET
        PROCESS = 2
        MES_NUM=RESET_TYPE
        OUT_LEN=0
        MES_DEST=X2STMES_RELAYF_DS_TERM
C
C START THE RELAY MECHANISM.
C
	CALL X2RSTART(PRO(1,PROBUF),PROCESS,POLL_STS,GROUP,MES_NUM,
     *                OUT_LEN,MES_BUF,MES_DEST,FORMAT)
	CALL X2RADDBF(PROBUF)
	PROBUF=0
	END
