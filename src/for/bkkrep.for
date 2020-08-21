C
C PROGRAM BKKREP
C
C BKKREP.FOR
C
C V09 13-MAY-1999 UXN SUPER TRIPLE ADDED.
C V08 10-JAN-1999 GPW STOPSYS OPTIMISATION
C V07 13-NOV-1997 UXN Automated BKKREP added.
C V06 02-OCT-1997 UXN INPUT LAYOUT CHANGED.
C V05 23-NOV-1995 PXB Couple and Double games added
C V04 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V03 29-APR-1994 JXP COPY=0
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 03-APR-92 GCAN INITIAL RELEASE FOR THE NETHERLANDS.
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
	PROGRAM BKKREP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4   REV			    !Program Revision.
	PARAMETER   (REV=01)
	INTEGER*4   CONLU		    !Logical Unit assigned to Console.
	PARAMETER   (CONLU=06)
C
	BYTE	    BELL/07/		    !Bell (Beep)  Character.
C
	INTEGER*4   GNUM		    !Game Number.
	INTEGER*4   GTYP		    !Game Type.
	INTEGER*4   GIND		    !Game Index.
	INTEGER*4   DRAW		    !Draw Number Eneterd.
	INTEGER*4   COPY		    !Number of Report Copies.
	INTEGER*4   ST			    !Status Returned by Subroutines.
	INTEGER*4   K,EXT,INP		    !Loop variable.
	INTEGER*4   MAX_REP		    !NUMBER OF GAME TYPES FOR THIS REP.
	PARAMETER   (MAX_REP=8)
	INTEGER*4   GTYP_REP(MAX_REP)	    !Allowed game types for this report
	DATA	    GTYP_REP/TDBL,TSCR,TWIT,TTSL,TCPL,TSSC,TTRP,TSTR/
	LOGICAL*4   SETFLG,AUTOPRMPT
	INTEGER*4   I,TSKST
	
C
C
	CALL COPYRITE
C
C DISPLAY PROGRAM REVISION.
C
	WRITE(CONLU,900) IAM(),IAM(),REV,IAM()
C
C GET SYSTEM CONFIGURATION INFO.
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) THEN
	   WRITE(CONLU,910) IAM(),ST,BELL
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C IF WINREP IS SET IN STOPCOM THEN USE THE VALUES FROM STOPCOM. 
C
	SETFLG = .FALSE.
	DO I=1,MAXGAM
	    IF(BKKREP_AUTO(I).NE.0) SETFLG = .TRUE.
	ENDDO
C
        IF(STOPMOD.EQ.WINMULTI) THEN                          !V06...
             IF(SETFLG) THEN 
                AUTOPRMPT=.TRUE.
                GO TO 99
             ELSE
  	        CALL GSTOP(GEXIT_SUCCESS)
             ENDIF
        ENDIF                                                 !...V06
C
	CALL STTSK(8HSTSYSTEM,TSKST,ST)
	AUTOPRMPT = SETFLG .AND. ISSUBPROC() .AND. ST.NE.4
	COPY=0
C
   99   CONTINUE
C
100	CONTINUE
C
C DISPLAY ALL GAMES 
C
	IF(.NOT.AUTOPRMPT) THEN
	  WRITE(CONLU,920) (K,GTNAMES(GTYP_REP(K)),K=1,MAX_REP)
 	  CALL PRMNUM('Enter game type  ',INP,1,MAX_REP,EXT)
	  IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	  GTYP = GTYP_REP(INP)
	  CALL PRMNUM('Enter game index ',GIND,1,MAXIND,EXT)
	  IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)

C
C PROMPT OP FOR DRAW NUMBER.
C
	GNUM = SCFGTN(GTYP,GIND)
C
	CALL PRMNUM('Enter Draw Number',DRAW,1,9999,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
C
C	CALL PRMNUM('Enter Number of Report Copies..: ',COPY,1,20,ST)
C	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
C
	ELSE
	  DO I=1,MAXGAM	
	    IF(BKKREP_AUTO(I).NE.0) THEN
	      DRAW = BKKREP_AUTO(I)
	      GTYP = GNTTAB(GAMTYP,I)
	      GIND = GNTTAB(GAMIDX,I)
	      GNUM = I
	      BKKREP_AUTO(I) = 0
	      GOTO 110
	    ENDIF
	  ENDDO
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
110	CONTINUE
C
C GENERATE REPORT.
C
	CALL ODBKKREP(GNUM,DRAW,COPY)
	GOTO 100
	   
900	FORMAT(1X,A,/,1X,A,'<<<<< BKKREP  V',I2.2,
     *	            'Book Keeping Report Generation >>>>>',/,1X,A)
910	FORMAT(1X,A,'Unable to get System Control Information,',
     *         '  Status: ',I4,A1)
920	FORMAT(/,' Book Keeping Report  ',//,<MAX_REP>(1X,I2,' - ',A8,/))
930	FORMAT(1X,A,I2,' - ',4A4)
940	FORMAT(1X,A)
950	FORMAT(1X,A,'Sorry, ',A8,
     *              ' is not available on this report! ',A1)
951	FORMAT(1X,A,A,A8,A,I2,1X,A,I6)
	END
