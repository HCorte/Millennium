C SUBROUTINE SCLOSNP
C
C V07 27-NOV-1997 UXN Changes for PITKA (singles,doubles etc.)
C V06 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V05 22-SEP-1993 GXA Enabeld to display all 40 (MAX#) of rows.
C V04 14-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V03 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C V02 21-OCT-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C TOTO SELECT CLOSE SNAPSHOT
C
C  VISION SUBROUTINE (TOTO SELECT CLOSE SNAPSHOT) SHOWING: SELECT ROW NUMBERS,
C  GAME NAMES, CLOSE TIMES (TIME THEY SHOULD CLOSE, ACT. TIME WHEN CLOSED),
C  CDC DATE AND JULIAN DATE, AND THE STATUS OF THE GAME.
C
C  CALLING SEQUENCE:
C     CALL SCLOSNP(NUM,GIND,ROW)
C
C  INPUT(S):
C     ARGUMENT LIST  - NUM       DRAW NUMBER
C                      GIND      GAME INDEX (FUTURE EXPANSION)
C                      ROW       ROW NUMBER (SELECTABLE 1-28)
C
C     SCREEN OUTPUTS - ODDSET OMSATTNING SNAPSHOT FROM
C                      MENU2 OF VPAGE.FTN
C
C  GLOSSARY:         ACTIM   => ACT. TIME WHEN CLOSED
C                    CLOSTIM => TIME WHEN GAME SHOULD CLOSE
C                    DBUF    => DATE BUFFER
C                    RDBUF   => ROW DATE BUFFER
C                    BEGSAL  => BEGINNING SALES DATE FOR SELECTED DRAW
C                    ENDSAL  => ENDING SALES DATE FOR SELECTED DRAW
C                    DISTIM  => ROUTINE WITHIN INPMOD TO DISPLAY TIME
C                    POLSTS  => NAMELIST FOR GAME STATUS
C                    RWSTS   => NAMELIST FOR ROW STATUS
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
	SUBROUTINE SCLOSNP(NUM,GIND,ROW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:DTSREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
	INTEGER*2 DBUF(LDATE_LEN),RDBUF(LDATE_LEN)
	INTEGER*2 BEGSAL(LDATE_LEN),ENDSAL(LDATE_LEN)
	INTEGER*4 FDB(7),ACTIM(MAXSRW),CLOSTIM(MAXSRW)
	INTEGER*4 NUM,GIND,GNUM,DRAW,ROW
	INTEGER*4 ST,LNS,I,K,R
	INTEGER*4 ENDROW				!Ending row to display.
	INTEGER*4 DSPROW/17/				!# of rows to display.
	CHARACTER    RTYPE(3)
	CHARACTER*17 POLSTS(11)
	CHARACTER*3 RWSTS(11)
	DATA POLSTS/'Not initialized  ','No drawing       ',
     *	            'Info entered     ','Game open        ',
     *	            'End of game      ','Results entered  ',
     *	            'Results verified ','Drawing completed',
     *	            'Results are final','Refund/cancelled ',
     *	            'Refunds enabled  '/
	DATA RWSTS/'   ','cls','ent','opn','cls','rin','ver',
     *	           'drw','fin','can','ref'/
	DATA RTYPE/'1','2',' '/
C
	DRAW=NUM
	IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
	  WRITE(CLIN23,3000) GTNAMES(TTSL)
	  RETURN
	ENDIF
C
	GNUM=GTNTAB(TTSL,GIND)
	IF(GNUM.LT.1) THEN
	  WRITE(CLIN23,3010) GTNAMES(TTSL),GIND
	  RETURN
	ENDIF
	IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
	IF(ROW.LT.1.OR.ROW.GT.MAXSRW) ROW = 1
	ENDROW = ROW + DSPROW
C
	IF(ENDROW.GT.MAXSRW) THEN
	   ENDROW = MAXSRW
	   ROW = ENDROW - DSPROW
	ENDIF
C
C GET DATA FROM COMMON OR DISK
C
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	  CALL GAMLOG(TTSL,GIND,DTSREC,TSLSTS)
	  GOTO 100
	ENDIF
C
C SET STATIC MODE
C
	SMODE=.TRUE.
C
C OPEN GAME FILE
C
	CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,1,DTSSEC*256)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	  CALL USRCLOS1(     1)
	  RETURN
	ENDIF
	CALL READW(FDB,DRAW,DTSREC,ST)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	  CALL USRCLOS1(     1)
	  RETURN
	ENDIF
C
C CHECK TO SEE IF THE GAME IS INITIALIZED
C
	IF(DTSSTS.EQ.0) THEN
	  WRITE(CLIN23,3040) GTNAMES(TTSL),GIND,DRAW
	  CALL USRCLOS1(     1)
	  RETURN
	ENDIF
	CALL USRCLOS1(     1)
C
100	CONTINUE
C
C INITIALIZE ARRAYS CONTAINING CLOSE TIMES
C
	CALL FASTSET(0,ACTIM,MAXSRW)
	CALL FASTSET(0,CLOSTIM,MAXSRW)
C
	DBUF(5)=DAYCDC
	BEGSAL(5)=DTSBSD
	ENDSAL(5)=DTSESD
	CALL LCDATE(DBUF)
	CALL LCDATE(BEGSAL)
	CALL LCDATE(ENDSAL)
C
C DETERMINE THE CLOSE TIMES FOR ALL TOTOSELECT ROWS
C
	DO 200 I=1,MAXSRW
	   ACTIM(I)  =DTSCTM(I)
	   CLOSTIM(I)=DTSTIM(I)
200	CONTINUE
C
C ENCODE TOTOSELECT CLOSE SNAPSHOT HEADING
C
	WRITE(CLIN1,1000) GTNAMES(TTSL),GIND,
     *			  (BEGSAL(I),I=9,13),(ENDSAL(I),I=9,13)
	WRITE(CLIN2,1001) DTSDRW,POLSTS(DTSSTS+1)
	WRITE(CLIN3,1002)
	LNS=4
	DO 210 R=ROW,ENDROW
C
C DETERMINE IF THE ROW IS NOT INITIALIZED.  IF ROW IS NOT ENCODE ROW
C NUMBER ONLY; HOWEVER, IF IT IS ENCODE SNAPSHOT
C
	   IF(DTSSTA(R).EQ.0) THEN
	      WRITE(XNEW(  LNS),999) R
	   ELSE
	      RDBUF(5)=DTSDAT(R)
	      CALL LCDATE(RDBUF)
	      WRITE(XNEW(  LNS),1003) R,(DTSNMS(I,1,R),I=1,3),
     *	                             (DTSNMS(I,2,R),I=1,3),
     *				      RTYPE(DTSROWTYP(R)),
     *	                              DISTIM(CLOSTIM(R)),
     *	                              DISTIM(ACTIM(R)),DTSDAT(R),
     *	                             (RDBUF(I),I=7,13),
     *	                              RWSTS(DTSSTA(R)+1)
	   ENDIF
	   LNS=LNS+1
210	CONTINUE
	RETURN
C
C     FORMAT STATEMENTS
C
999	FORMAT(I2.2,78(' '))
1000	FORMAT('* ',A8,1X,I1,2X,5A2,' - ',5A2,' *')
1001	FORMAT('  Event code- ',I4,20(' '),'* ',A17,' *')
1002	FORMAT('Row',1X,'Team names',18X,'Typ',1X,
     *	       'Close Time',1X,'Act.Time',1X,'Cdc',
     *	        3X,'Date',9X,'Sts')
1003	FORMAT(I2.2,2X,3A4,' - ',3A4,3X,A1,1X,
     *	       A8,3X,A8,1X,I4,1X,7A2,1X,A3)
3000	FORMAT('Enter !',A8,' game index ')
3010	FORMAT(A8,1X,I1,' game not active')
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
3040	FORMAT(A8,1X,I1,' game not initialized event > ',I4)
	END
