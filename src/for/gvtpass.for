C  GXSRC:GVTPASS.FOR
C  
C  $Log:   GXAFIP:[GOLS]GVTPASS.FOV  $
C  
C     Rev 1.1   28 Jan 1997 19:28:54   RXK
C  Message numbers for Errlog changed
C  
C     Rev 1.0   05 Dec 1996 20:48:00   HXK
C  Initial revision.
C  
C     Rev 1.6   11 Oct 1994 10:01:52   MXP
C  If Instant system suppressed, response with the correct error message.
C  
C     Rev 1.5   09 Oct 1994  3:11:30   MXP
C  If Instant suppressed, reject with SUPR code.
C  
C     Rev 1.4   25 Sep 1994 15:12:48   MCM
C  CHECK FOR DELETE AS DATA SLOT 3
C  
C     Rev 1.3   30 Aug 1994 12:03:18   MCM
C  PASSWORD MANAGEMENT IS PROCESSED THRU SPESRVF
C  
C     Rev 1.2   27 Jul 1994 11:44:42   MCM
C  CORRECTED ERROR NUMBER TO BE USED BY ERRLOG
C  
C     Rev 1.1   20 Jun 1994 18:45:08   MCM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    20 Jun 1994 18:44:26   MCM
C  Initial revision.
C  
C PASS.FOR
C
C CALLING SEQUENCE:
C      CALL PASS(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
C
C
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GVTPASS(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C
	INTEGER*4 MYCHKSUM, CHKLEN, OPTIONS, CLASS
        INTEGER*4 CHKDIG, OFF, I, IND
        INTEGER*4 PASSNUM, NEWPASS, TER, STATUS
C       INTEGER*4 BUF(CDLEN), ST, MESS(EDLEN)
        INTEGER*4 ADD/0/, CHANGE/1/, DELETE/2/
	INTEGER*2 OUTLEN
	BYTE	  MESTAB(*)
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
        INTEGER*4   TEMP, TEMP1, TEMP2
C
        STATUS=0
C
C GET SEQUENCE NUMBER
C
        TEMP = ZEXT(MESTAB(1))
        TRABUF(TTRN)=IAND(TEMP,15)
C
C GET CHECKSUM
C
        TEMP1 = ZEXT(MESTAB(3))
        TEMP2 = ZEXT(MESTAB(4))
        TRABUF(TCHK) = ISHFT(TEMP1,8) + TEMP2
C
C GET STATISTICS
C
        TRABUF(TTSTCS)=ZEXT(MESTAB(5))
C
C GET OPTION FLAGS
C
        TEMP1 = ZEXT(MESTAB(6))
        TEMP2 = ZEXT(MESTAB(7))
        OPTIONS = ISHFT(TEMP1,8) + TEMP2
        IND=8
C
C CHECK FOR NODE NUMBER (NOT USED)
C
        IF(IAND(OPTIONS,'0001'X).NE.0) THEN
           IND=IND+4
        ENDIF
C
C CHECK FOR RETAILER NUMBER (NOT USED)
C
        IF(IAND(OPTIONS,'0002'X).NE.0) THEN
           IND=IND+4
        ENDIF
C
C CHECK FOR PASSWORD (NOT USED)
C
        IF(IAND(OPTIONS,'0004'X).NE.0) THEN
           IND=IND+2
        ENDIF
C
C CHECK FOR ORIGINATOR   (NOT USED)
C
        IF(IAND(OPTIONS,'0008'X).NE.0) THEN
           IND=IND+2
        ENDIF
C
C CHECK FOR LOCATION NUMBER (NOT USED)
C
        IF(IAND(OPTIONS,'0010'X).NE.0) THEN
           IND=IND+4
        ENDIF
C
C CHECK FOR USER ID (NOT USED)
C
        IF(IAND(OPTIONS,'0020'X).NE.0) THEN
           IND=IND+4
        ENDIF
C
C CHECK FOR OPERATOR ID (NOT USED)
C
        IF(IAND(OPTIONS,'0040'X).NE.0) THEN
           IND=IND+1
        ENDIF
C
C CHECK FOR PAYMENT TYPE (NOT USED HERE)
C
        IF(IAND(OPTIONS,'0080'X).NE.0) THEN
           IND=IND+2
        ENDIF
C
C CHECK FOR VALIDATION MODE (NOT USED HERE)
C
        IF(IAND(OPTIONS,'0100'X).NE.0) THEN
           IND=IND+2
        ENDIF
C
C CHECK FOR BANK (NOT USED HERE)
C
        IF(IAND(OPTIONS,'0200'X).NE.0) THEN
           IND=IND+8
        ENDIF
C
C GET CLASS
C
        CLASS=ZEXT(MESTAB(IND))
        TRABUF(TSDT3)=CLASS
        IF(CLASS.LT.ADD.OR.CLASS.GT.DELETE) THEN
          TRABUF(TERR)=SYNT
        ENDIF
        IND=IND+1
C
C GET PASSWORD
C 
	I4TEMP=0
	I1TEMP(1) = MESTAB(IND+0)
	I1TEMP(2) = MESTAB(IND+1)
	PASSNUM      = I4TEMP
	IND=IND+2
C
C GET NEW PASSWORD
C
        IF(CLASS.EQ.CHANGE) THEN
          I4TEMP=0
	  I1TEMP(1) = MESTAB(IND+0)
	  I1TEMP(2) = MESTAB(IND+1)
	  NEWPASS   = I4TEMP
          IF(NEWPASS.EQ.0) THEN
            TRABUF(TERR)=SYNT
            GOTO 6000
          ENDIF
	  IND=IND+2
        ENDIF
C
        IF(PASSNUM.EQ.0) THEN
          TRABUF(TERR)=SYNT
          GOTO 6000
        ENDIF
C
C If Instant (Agent Management) System is suppressed, do not allow the function.
C
	IF(P(SUPINS).EQ.1) THEN
C
C Reject with general type 9 and general error code.
C
	   TRABUF(TERR)=SUPR
           TRABUF(TSTAT)=REJT
	   TRABUF(TSDT1)=0
	   TRABUF(TSDT2)=0
C
C CONTROL AND SEQUENCE NUMBER
C
	   MESTAB(1) = '20'X+TRABUF(TTRN)
C
C TYPE AND SUBTYPE
C
	   MESTAB(2) = '90'X
C
	   IND=5
	   MESTAB(IND) = TRABUF(TERR)
	   IND=IND+1
C
           GOTO 7000
        ENDIF
C
        TER=TRABUF(TTER)
C
        GOTO(1000,2000,3000) CLASS+1
        TRABUF(TERR)=SYNT
        GOTO 6000
C
C       PASSWORD ADDITION
C
1000    CONTINUE
        TRABUF(TSNEW)=PASSNUM
        DO 1100 I=0,7
           IF(AGTTAB(APSNUM+I,TER).EQ.PASSNUM) THEN
             STATUS=1                               !DUPLICATE PASSWORD
             GOTO 6000
           ENDIF
1100    CONTINUE
        DO 1200 I=0,7
           IF(AGTTAB(APSNUM+I,TER).EQ.0) THEN
             AGTTAB(APSNUM+I,TER)=PASSNUM
             TRABUF(TSDT1)=I+1
             GOTO 6000
           ENDIF
1200    CONTINUE
        STATUS=2                                    !NO SLOT AVAILABLE
        GOTO 6000
C
C       PASSWORD MODIFICATION
C
2000    CONTINUE
        TRABUF(TSOLD)=PASSNUM
        TRABUF(TSNEW)=NEWPASS
        DO 2100 I=0,7
           IF(AGTTAB(APSNUM+I,TER).EQ.NEWPASS) THEN
             STATUS=1                                !DUPLICATE PASSWORD
             GOTO 6000
           ENDIF
2100    CONTINUE
        DO 2200 I=2,7        
           IF(AGTTAB(APSNUM+I,TER).EQ.PASSNUM) THEN
             AGTTAB(APSNUM+I,TER)=NEWPASS
             TRABUF(TSDT1)=I+1
             GOTO 6000
           ENDIF
2200    CONTINUE
        STATUS=3                                     !PASSWORD NOT FOUND
        GOTO 6000
C
C       PASSWORD DELETION
C
3000    CONTINUE
        TRABUF(TSNEW)=PASSNUM
        DO 3100 I=2,7        
           IF(AGTTAB(APSNUM+I,TER).EQ.PASSNUM) THEN
             TRABUF(TSDT1)=I+1
             DO 3200 OFF=I+1,8
                AGTTAB(APSNUM+OFF-1,TER)=AGTTAB(APSNUM+OFF,TER)
3200         CONTINUE
             GOTO 6000
           ENDIF
3100    CONTINUE
        STATUS=3                                     !PASSWORD NOT FOUND
        GOTO 6000
C
C       BUILD OUTPUT MESSAGE
C
6000    CONTINUE
C
C       CONTROL AND SEQUENCE NUMBER
C
        MESTAB(1) = '20'X+TRABUF(TTRN)
C
C       TYPE AND SUBTYPE
C
        MESTAB(2) = 'C4'X
C
C       TIME
C
        IND=5
        I4TEMP=TRABUF(TTIM)
        MESTAB(IND+0) = I1TEMP(1)
        MESTAB(IND+1) = I1TEMP(2)
        MESTAB(IND+2) = I1TEMP(3)
        IND=IND+3
C
C       JULIAN DATE
C
        I4TEMP=DAYJUL
        MESTAB(IND+0) = I1TEMP(1)
        MESTAB(IND+1) = I1TEMP(2)
        IND=IND+2
C
C       SERIAL NUMBER AND CHECK DIGITS
C
        CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),I4TEMP,CHKDIG)
        MESTAB(IND+0) = I1TEMP(1)
        MESTAB(IND+1) = I1TEMP(2)
        MESTAB(IND+2) = I1TEMP(3)
        MESTAB(IND+3) = CHKDIG
        IND=IND+4
C
C       SEND UPDATE TO THE LMS SYSTEM FOR GOOD MODIFICATIONS
C
C
C REMOVED AGENT GVT ID UPDATE / AGENT SATUS THAT TIMO ASKED FOR
C
        IF(TRABUF(TERR).EQ.NOER.AND.STATUS.EQ.0) THEN
C          IF(P(SYSTYP).EQ.LIVSYS) THEN
C            BUF(1)=2
C            BUF(2)=TRABUF(TSNEW)
C            BUF(3)=TCAGT
C            BUF(4)=TRABUF(TSDT1)
C            BUF(5)=TER
C            CALL ISBLDCMD(BUF,ST)
C            IF(ST.NE.0) THEN
C              MESS(1)=SPEF
C              MESS(2)=TEGEN
C              MESS(3)=35
C              MESS(4)=TRABUF(TSER)
C              MESS(5)=TRABUF(TTER)
C              CALL QUEMES(MESS)
C            ENDIF
C          ENDIF
          AGTHTB(AGTPASCDC,TER)=DAYCDC
        ELSEIF(STATUS.NE.0) THEN
          TRABUF(TERR)=INVL
          TRABUF(TSTAT)=REJT
        ELSE
          TRABUF(TSTAT)=REJT
        ENDIF
        TRABUF(TSDT2)=STATUS
C
C       RESULT CODE ONE
C
        MESTAB(IND+0) = TRABUF(TERR)
        IND=IND+1
C
C       RESULT CODE 2
C
        MESTAB(IND+0) = TRABUF(TSDT2)
        IND=IND+1
C
7000	CONTINUE
C
	OUTLEN=IND
	I4CCITT = TRABUF(TCHK)       
	MESTAB(3) = I1CCITT(1)
	MESTAB(4) = I1CCITT(2)
	CHKLEN=OUTLEN-1
	CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT=MYCHKSUM
	MESTAB(3)=I1CCITT(1)
	MESTAB(4)=I1CCITT(2)
	RETURN
	END
