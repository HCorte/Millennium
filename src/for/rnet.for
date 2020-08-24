C
C SUBROUTINE RNET
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFIP:[GOLS]RNET.FOV                                     $
C  $Date::   11 Feb 1997 18:35:50                                         $
C  $Revision::   1.1                                                      $
C  $Author::   WPW                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_rnet.for;1 **
C
C RNET.FOR
C
C V05 06-MAR-2014 SCML Added support to PLACARD Project - IGS
C V04 09-OCT-1994 MP   Save simulator indication -  Integrate UK changes into 
C		               X2X Baseline
C V03 05-APR-1994 GPR  USE X2X_I4_STATION TO DETERMINE STATION AND TERNUM
C V02 03-APR-1992 TKO  REPLACE HARDWIRED LENGTH FOR 2ND PHASE
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C
C+
C
C NAME: RNET.FTN
C
C INPUT:
C        BUF -- BUFFER # CONTAINING THE DATA
C
C OUTPUT:
C         --
C
C SIDE EFFECTS:
C          NONE.
C
C
C
C-
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE RNET(BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'					! V04
        INCLUDE 'INCLIB:APUCOM.DEF'
C
	INTEGER*4 WLEN, LENGTH, NXTLNK, STATUS, PROBUF, SER
	INTEGER*4 RCOUNT, NRECS, LINK, LAST, BUF
C
	INTEGER*4 TEMP
	INTEGER*2 HTEMP(2)
	CHARACTER*1 CTEMP(4)
	EQUIVALENCE (TEMP,CTEMP(1),HTEMP(1))
C
C UNSTUFF THE INFORMATION IN THE BUFFER AND PLACE
C IN THE DISPATCHERS INPUT QUEUE.
C
	IF(NETBUF(MODE,BUF).NE.TRNMD)THEN
          GOTO1000
	ENDIF
C
C     CALCULATE CRC - JUST FOR CHECKING
C
	LAST=NETBUF(NEXT,BUF)
C***  CRC=0
C***  IF (LAST.GE.HDRSIZ+1.AND.LAST.LE.NETLEN-1) THEN
C***    CALL GETCRC(NETBUF(HDRSIZ+1,BUF),NETBUF(LAST,BUF),CRC)
C***  ELSE
C***    CRC=-1
C***  ENDIF
C***  IF (CRC.NE.NETBUF(CRCOFF,BUF)) THEN
C***    TYPE *,'invalid CRC ',CRC
C***    TYPE *,' buffer usage ',NETBUF(NEXT,BUF)
C***    CALL LDUMP(BUF)
C***    GOTO 1000
C***  ENDIF
C
	IF(NETBUF(NUMREC,BUF).EQ.0) GOTO 1000
	IF(NETBUF(PSER,BUF).EQ.0)   GOTO 1000
	LINK=HDRSIZ+1
	NRECS=NETBUF(NUMREC,BUF)
	LAST=NETBUF(NEXT,BUF)
	IF(LAST.LT.LINK.OR.LAST.GT.NETLEN-1)THEN
	  TYPE *,IAM(),CHAR(7),'BAD END OF LIST POINTER ',LAST
	  TYPE *,IAM(),CHAR(7),'NUMBER RECORDS = ',NRECS
	  TYPE *,IAM(),CHAR(7),' buffer usage ',NETBUF(NEXT,BUF)
	  CALL LDUMP(BUF)
	  GO TO 1000
	ENDIF
	IF(NETBUF(LAST,BUF).NE.0)THEN         !04/21/83 DBUG NBRPRO
	  TYPE *,IAM(),CHAR(7),'BAD LINK LIST IN BUFFER ',BUF
	  TYPE *,IAM(),CHAR(7),'LAST LINK SHOULD BE 0 BUT IS ',NETBUF(LAST,BUF)
	  TYPE *,IAM(),CHAR(7),' buffer usage ',NETBUF(NEXT,BUF)
	  TYPE *,IAM(),CHAR(7),'HSER=',NETBUF(PSER,BUF),' LINK=',LINK
	  TYPE *,IAM(),CHAR(7),'NRECS=',NRECS
	  TYPE *,IAM(),CHAR(7),'DUMP OF BUFFERS FOLLOW:'
	  CALL LDUMP(BUF)
	  GO TO 1000
	ENDIF
	RCOUNT=0                              !04/21/83 RECORD COUNT
C
C UPDATE HIGH SERIAL # (IF NOT ZERO)
C
	SER=NETBUF(PSER,BUF)
C
C LOOP TO REMOVE EACH TRANSACTION FROM BUFFER
C
100	IF(LINK.GE.LAST.OR.LINK.EQ.0)THEN
	  IF(RCOUNT.NE.NRECS)THEN
	    TYPE*,IAM(),CHAR(7),'RECORD COUNT MIS-MATCH '
	    TYPE*,IAM(),CHAR(7),'SUPPOSED TO BE ',NRECS,' RECORDS.'
	    TYPE*,IAM(),CHAR(7),'THERE WERE     ',RCOUNT,' RECORDS.'
	    CALL LDUMP(BUF)
	  ENDIF
	  GOTO 1000
	ENDIF
102	CALL GETBUF(PROBUF)
	IF(PROBUF.LT.1)THEN
	  CALL XWAIT(100,1,STATUS)             !GOOD @50-BAD@5-TRY@100
	  GOTO102
	ENDIF
C
C GET LINK TO NEXT RECORD FROM THIS RECORD
C
	NXTLNK=NETBUF(LINK,BUF)
	LINK=LINK+1
C
C GET TRCODE & INPLEN FROM NEXT FULLWORD
C
	TEMP=NETBUF(LINK,BUF)
	HPRO(TRCODE,PROBUF)=HTEMP(1)
C
C GET LINENO & TERNUM FROM NEXT 2 FULLWORDS
C
C       ***** Start V03 changes *****

        IF (X2X_I4_STATION) THEN
	   HPRO(INPLEN,PROBUF)=HTEMP(2)
	   LINK=LINK+1
	   PRO(LINENO,PROBUF)=NETBUF(LINK,BUF)
	   PRO(TERNUM,PROBUF)=NETBUF(LINK+1,BUF)
	   LINK=LINK+2
C
C Oct 9, 94 MP: save simulator indication - V04
C
	   IF(PRO(LINENO,PROBUF).EQ.-999) 			!V04
     *	      HPRO(SIMMOD,PROBUF) = PRO(LINENO,PROBUF)		!V04
C
	ELSE
           HPRO(TERNUM,PROBUF)=HTEMP(2)
           LINK=LINK+1
           TEMP=NETBUF(LINK,BUF)
           HPRO(LINENO,PROBUF)=HTEMP(1)
           HPRO(INPLEN,PROBUF)=HTEMP(2)
           LINK=LINK+1
C
C Oct 9, 94 MP: save simulator indication - V04
C
	   IF(HPRO(LINENO,PROBUF).EQ.-999) 			!V04
     *	      HPRO(SIMMOD,PROBUF) = HPRO(LINENO,PROBUF)		!V04
C
	ENDIF

C       ***** End V03 changes *****

C
C GET SERIAL & TIMESTAMP FROM NEXT 2 FULLWORDS
C
	PRO(SERIAL,PROBUF)=NETBUF(LINK,BUF)
	SER=PRO(SERIAL,PROBUF)
	PRO(TSTAMP,PROBUF)=NETBUF(LINK+1,BUF)
	LINK=LINK+2
C
C COMPUTE LENGTH OF INPUT TABLE IN FULLWORDS
C ROUNDING UP TO FULLWORD BOUNDARY, IF NECESSARY'
C
	LENGTH=HPRO(INPLEN,PROBUF)
	WLEN=(LENGTH+3)/4
	IF (WLEN.LE.0.OR.WLEN.GT.OUTLEN_MAX/4) THEN
	   TYPE *,' RECORD LENGTH INVALID ',WLEN,' OFFSET ',LINK
	   CALL LDUMP(BUF)
	   CALL RELBUF(PROBUF)
	   GOTO 1000
	ENDIF
C
C COPY OVER INPTAB IN 4-BYTE BLOCKS
C
C***  DO 106 K=0,WLEN-1
C***    PRO(INPTAB+K,PROBUF)=NETBUF(LINK+K,BUF)
C***106   CONTINUE
C
C EURO MIL PROJECT - IF THIS IS ONE TRANSACTION OF EURO MIL THEN PUT NETBUF INTO WRKTAB
C
        IF(HPRO(TRCODE,PROBUF) .EQ. TYPEUR) THEN
           CALL MOVTAB(NETBUF(LINK,BUF),PRO(INPTAB,PROBUF),4)
           LINK=LINK + 4
           CALL MOVTAB(NETBUF(LINK,BUF),PRO(WRKTAB,PROBUF),WLEN)	
C----+------------------------------------------------------------------
C V05| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        ELSEIF(HPRO(TRCODE,PROBUF) .EQ. TYPIGS) THEN
           CALL MOVTAB(NETBUF(LINK,BUF),PRO(INPTAB,PROBUF),4)
           LINK=LINK + 4
           CALL MOVTAB(NETBUF(LINK,BUF),PRO(WRKTAB,PROBUF),WLEN)        
C----+------------------------------------------------------------------
C V05| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        ELSE 
	   CALL MOVTAB(NETBUF(LINK,BUF),PRO(INPTAB,PROBUF),WLEN)
        ENDIF
	LINK=LINK+WLEN

        IF(HPRO(TRCODE,PROBUF).EQ.TYPCRS) THEN
          CALL MOVTAB(NETBUF(LINK,BUF),APUBUF(1,PROBUF),APULEN)
          LINK=LINK+APULEN
        ENDIF
C
C EURO MIL PROJECT - READ FROM OTHER SYSTEMS EURO MIL TRANSACTIONS
C
        IF(HPRO(TRCODE,PROBUF).EQ.TYPEUR) THEN
          CALL MOVTAB(NETBUF(LINK,BUF),APUBUF(1,PROBUF),APULEN)
          LINK=LINK+APULEN
        ENDIF
C----+------------------------------------------------------------------
C V05| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(HPRO(TRCODE,PROBUF) .EQ. TYPIGS) THEN
          CALL MOVTAB(NETBUF(LINK,BUF),APUBUF(1,PROBUF),APULEN)
          LINK=LINK+APULEN
        ENDIF
C----+------------------------------------------------------------------
C V05| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

	IF(LINK.NE.NXTLNK.AND.NXTLNK.GT.0)THEN
	  TYPE*,'CORRUPT NETBUF LINKS'
	  TYPE*,'PROCOM BUF#',PROBUF,' NETWORK BUF#',BUF
	  TYPE*,'LINK = ',LINK,' SHOULD BE ',NXTLNK
	  CALL LDUMP(BUF)
	  CALL RELBUF(PROBUF)
	  GOTO 1000
	ENDIF
	LINK=NXTLNK
	CALL ABL(PROBUF,QUETAB(1,DIS),STATUS)
	RCOUNT=RCOUNT+1                       !04/21/83 RECORD COUNT
	GOTO100
C
C NON TRANSACTION RECEIVED
C
1000	CONTINUE
C
C COMMON EXIT
C
	CALL RELSE(TSKNAM(DIS),STATUS) ! KICK DISPATCHER
	RETURN
	END
