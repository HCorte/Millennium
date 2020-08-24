C SUBROUTINE EZEDIT
C
C $Log:   GXAFIP:[GOLS]EZEDIT.FOV  $
C  
C     Rev 1.2   05 Feb 1997 17:27:12   HXK
C  Added GVT news message
C  
C     Rev 1.1   13 Aug 1996 10:18:36   RXK
C  Bug fixed (shift from display of message removed) 
C  
C     Rev 1.0   17 Apr 1996 13:07:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:14:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ezedit.for **
C
C EZEDIT.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Do not include GLOBAL.DEF if including MSGCOM.DEF.
C
C V03 28-JAN-92 GCAN INCREASED MAX LINES FROM 4 TO 6.
C V02 27-NOV-91 GCAN ADDED MESSAGE NUMBER PARAMETER TO CALL IN ORDER TO
C 		     BE ABLE TO DISPLAY DIFFERENT MESSAGE NUMBERS(HEADINGS)
C V01 01-AUG-90 XXX  RELEASED FOR VAX
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
C Copyright 1991,1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE EZEDIT(MSGBUF,MSGNUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'
C
	INTEGER*4 INDSTR(9),INDEND(9)
	INTEGER*4 GNDSTR(9),GNDEND(9)
	INTEGER*4 SWAP, ST, LNUM, MN, LNE, ASK
	INTEGER*4 MSGNUM		!Message # we are Currently Editing. 
C
	CHARACTER ALINE*26,COMD*4,MSGBUF*1000,WRKMES*1000
	CHARACTER*28 ASTR
	CHARACTER*22 GSTR
	CHARACTER*28 DISPMSG(TXTBEG-1)
	DATA ASTR/'****************************'/
	DATA GSTR/'**********************'/
	DATA INDSTR/1,27,53,79,105,131,3*0/
	DATA INDEND/26,52,78,104,130,156,3*0/
	DATA GNDSTR/1,21,41,61,81,101,3*0/
	DATA GNDEND/20,40,60,80,100,120,3*0/
	DATA LNUM/0/
	DATA DISPMSG/'*??????????????????????????*',
     *		     '*  ONLINE TER (SP3) NEWS   *',
     *               '*??????????????????????????*',
     *               '*??????????????????????????*',
     *               '*??????????????????????????*',
     *               '*??????????????????????????*',
     *               '* GVT NEWS MESSAGE   *      ',
     *               '*??????????????????????????*',
     *               '*      TV NEWS MESSAGE     *'/
C
C
	IF(MSGNUM.EQ.MNEWS .OR. MSGNUM.EQ.TVNEWS) THEN
	   WRKMES(1:158)=MSGBUF(1:158)
	   ASK=0
100	   CONTINUE
	   CALL CLRSCR(5)
	   WRITE(5,10001) ASTR		!TOP BORDER OF TICKET IMAGE
	   WRITE(5,10002) DISPMSG(MSGNUM)	!NEWS MESSAGE LINE
	   WRITE(5,10003)			!BLANK LINE
C
C PRINT OUT THE MESSAGE BUFFER.
C
	   DO LNE=1,6
	      WRITE(5,10004)LNE,WRKMES(INDSTR(LNE):INDEND(LNE))
	   ENDDO

	   DO MN=1,3
	      WRITE(5,10003)
	   ENDDO

	   WRITE(5,10001)ASTR        !BOTTOM BORDER OF TICKET IMAGE

	   IF(ASK.EQ.0)THEN
	      WRITE(5,10005)
	      WRITE(5,10007)
	      WRITE(5,10010)
	      CALL WIMG(5,'        ENTER OPTION: ')
	      READ(5,10011)COMD,LNUM

	      IF(COMD.EQ.'DONE') THEN            !EDITING COMPLETED.
	         MSGBUF(1:158)=WRKMES(1:158)
	         RETURN
	      ENDIF

	      IF(COMD.NE.'DONE'.AND.COMD.NE.'ALT ') THEN
	         TYPE *,' INVALID COMMAND ENTERED    '
	         CALL XWAIT(2,2,ST)
	         GOTO 100
	      ENDIF

	      IF(LNUM.LT.1.OR.LNUM.GT.6)THEN
	         TYPE *,' LINE NUMBER VALUE ERROR    '
	         TYPE *,' PLEASE RE-ENTER'
	         CALL XWAIT(2,2,ST)
	         GOTO 100
	      ENDIF
C
C GO UP AND ALTER A LINE NUMBER THAT WAS REQUESTED
C
	      IF(COMD.EQ.'ALT ')THEN
	         ASK=1
	         GOTO 100
	      ENDIF
	   ELSE
	      ASK=0
	      IF(LNUM.EQ.6)THEN
	         WRITE(5,10012)WRKMES(INDSTR(LNUM):INDEND(LNUM))
	      ELSE
	         WRITE(5,10009)WRKMES(INDSTR(LNUM):INDEND(LNUM))
	      ENDIF
	      CALL WIMG(5,'NEW LINE')
	      READ(5,9000)ALINE
	   ENDIF
	   IF(ALINE(1:1).EQ.'@')GOTO 100   !SKIP REPLACEMENT
C
C REPLACE THE MESSAGE IN THE BUFFER WITH THE MESSAGE
C JUST ENTERED AND RE-DISPLAY FOR THE OPERATOR.
C
	   SWAP=26
	   WRKMES(INDSTR(LNUM):INDEND(LNUM))=ALINE(1:SWAP)
	   GOTO 100
C
C EDIT THE GVT MESSAGE HERE ...
C
	ELSEIF(MSGNUM.EQ.GVTNEWS) THEN   !edit GVT news message

	   WRKMES(1:120)=MSGBUF(1:120)

	   ASK=0
200	   CONTINUE
	   CALL CLRSCR(5)
	   WRITE(5,20001) GSTR		!TOP BORDER OF TICKET IMAGE
	   WRITE(5,10002) DISPMSG(MSGNUM)	!NEWS MESSAGE LINE
	   WRITE(5,20003)			!BLANK LINE
C
C PRINT OUT THE MESSAGE BUFFER.
C
	   DO LNE=1,6
	      WRITE(5,20004)LNE,WRKMES(GNDSTR(LNE):GNDEND(LNE))
	   ENDDO
	   DO MN=1,3
	      WRITE(5,20003)
	   ENDDO
	   WRITE(5,20001) GSTR        !BOTTOM BORDER OF TICKET IMAGE

	   IF(ASK.EQ.0)THEN
	      WRITE(5,10005)
	      WRITE(5,10007)
	      WRITE(5,10010)
	      CALL WIMG(5,'        ENTER OPTION: ')
	      READ(5,10011)COMD,LNUM

	      IF(COMD.EQ.'DONE') THEN            !EDITING COMPLETED.
	         MSGBUF(1:120)=WRKMES(1:120)
	         RETURN
	      ENDIF

	      IF(COMD.NE.'DONE'.AND.COMD.NE.'ALT ') THEN
	         TYPE *,' INVALID COMMAND ENTERED    '
	         CALL XWAIT(2,2,ST)
	         GOTO 200
	      ENDIF

	      IF(LNUM.LT.1.OR.LNUM.GT.6)THEN
	         TYPE *,' LINE NUMBER VALUE ERROR    '
	         TYPE *,' PLEASE RE-ENTER'
	         CALL XWAIT(2,2,ST)
	         GOTO 200
	      ENDIF
C
C GO UP AND ALTER A LINE NUMBER THAT WAS REQUESTED
C
	      IF(COMD.EQ.'ALT ')THEN
	         ASK=1
	         GOTO 200
	      ENDIF
	   ELSE
	      ASK=0
	      IF(LNUM.EQ.6)THEN
	         WRITE(5,10012)WRKMES(GNDSTR(LNUM):GNDEND(LNUM))
	      ELSE
	         WRITE(5,20009)WRKMES(GNDSTR(LNUM):GNDEND(LNUM))
	      ENDIF
	      CALL WIMG(5,'NEW LINE')
	      READ(5,9000)ALINE
	   ENDIF
	   IF(ALINE(1:1).EQ.'@')GOTO 200   !SKIP REPLACEMENT
C
C REPLACE THE MESSAGE IN THE BUFFER WITH THE MESSAGE
C JUST ENTERED AND RE-DISPLAY FOR THE OPERATOR.
C
	   SWAP=20
	   WRKMES(GNDSTR(LNUM):GNDEND(LNUM))=ALINE(1:SWAP)
	   GOTO 200
	ENDIF

9000	FORMAT(A)
10001	FORMAT(9X,A28)
10002	FORMAT(9X,A28)
10003	FORMAT(9X,'*',26X,'*')
10004	FORMAT(1X,'LINE',I2,2X,'*',A26,'*')
10005	FORMAT(//,10X,'****** EDITING COMMANDS ******',/)
10007	FORMAT(9X,'ALT #  - ALTER LINE (LINE NUMBER)')
10009	FORMAT(//,1X,'OLD LINE<',A26,'>','26 CHARACTERS',3X,
     *	      'NO CHANGE = "@" IN COLUMN 1',/)
10010	FORMAT(9X,'DONE   - EDITING SESSION COMPLETE',/)
10011	FORMAT(A4,BN,I3)
10012	FORMAT(//,1X,'OLD LINE<',A16,'>','16 CHARACTERS',9X,
     *	      'NO CHANGE = "@" IN COLUMN 1',/)
20001	FORMAT(9X,A22)
20002	FORMAT(9X,A22)
20003	FORMAT(9X,'*',20X,'*')
20004	FORMAT(1X,'LINE',I2,2X,'*',A20,'*')
20009	FORMAT(//,1X,'OLD LINE<',A20,'>','20 CHARACTERS',3X,
     *	      'NO CHANGE = "@" IN COLUMN 1',/)
	END
