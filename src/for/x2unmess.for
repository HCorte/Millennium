C
C SUBROUTINE X2UNMESS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2UNMESS.FOV                                 $
C  $Date::   17 Apr 1996 16:39:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2unmess.for;1 **
C
C X2UNMESS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SENDS AN UNSOLICITED NEWS MESSAGE TO A SPECIFIED STATION
C MAX 4 LINES OF MESSAGE CAN BE TRANSFERED.
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
	SUBROUTINE X2UNMESS(OPT,OPT2,OPT3,BUFFER,STATION,MES_NUM,
     *	                    OUT_LEN,DEST,FORMAT,NXTSTN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
C
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
C
	INTEGER*2 BUFFER(*)
	INTEGER*4 OPT,OPT2,OPT3
        INTEGER*4 LINMAX, DEST, MES_NUM, LINE, LINCNT
        INTEGER*4 TMPLEN, OUT_LEN, J, I, FLAG, STATION
        INTEGER*4 FORMAT
	LOGICAL   NXTSTN
	PARAMETER(LINMAX=4)
C
	CHARACTER*25 PROMPT
	CHARACTER*26 MESS(LINMAX),TMESS
	INTEGER*2    DMESS(LINMAX*26),CONTRL
	EQUIVALENCE (DMESS(1),MESS(1))
	DATA PROMPT/'Enter message for line X:'/
C.....	DATA CONTRL/Z20B1/      ! CONCURRENT
	DATA CONTRL/ZB120/      ! DEC
C
	DEST=X2STMES_RELAYF_DS_TERM  !MESSAGE TO TERMINALS
	MES_NUM=0
10	CONTINUE
	LINE=0
	LINCNT=0
C
C IF SENDING TO THE NEXT STATION IS A RELAY GROUP, DO NOT
C PROMPT FOR ANOTHER MESSAGE, SIMPLY USE THE LAST INPUT DATA.
C
	IF(NXTSTN) THEN
	  BUFFER(1)=CONTRL
	  TMPLEN=OUT_LEN/2+1
	  DO 12 J=1,TMPLEN
	    BUFFER(J+1)=DMESS(J)
12	  CONTINUE
	  RETURN
	ENDIF
C
	OUT_LEN=0
	CALL FASTSET(0,DMESS,54)
	TMESS='                           '
C
C GET MESSAGE
C
	TYPE*,'Enter message, max 4 lines of 26 characters',
     *	      ' terminated with a ^ '
C
C READ NEXT LINE OF TEXT.
C
20	CONTINUE
	LINCNT=LINCNT+1
	WRITE(PROMPT(24:24),903) LINCNT
	CALL WIMG(5,PROMPT)
	READ(5,900) TMESS
C
C SEARCH FOR TERMINATION.
C
	DO 30 I=1,26
	  IF(TMESS(I:I).EQ.'^') GOTO 40
30	CONTINUE
	IF(LINCNT.LT.LINMAX) THEN
	   MESS(LINCNT)=TMESS
	   GOTO 20
	ENDIF
	WRITE(5,901)
	GOTO 10
C
C MESSAGE COMPLETE.
C
40	CONTINUE
	TMESS(I:I)=' '
	MESS(LINCNT)=TMESS
	CALL WIMG(5,'Are you sure you want to send this message [Y/N]')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 10
	OUT_LEN=LINCNT*26
C
C STORE CONTROL INFORMATION OF MESSAGE.
C
	BUFFER(1)=CONTRL
C
C TRANSFER MESSAGE TO BUFFER
C
	TMPLEN=OUT_LEN/2+1
	DO 50 J=1,TMPLEN
	  BUFFER(J+1)=DMESS(J)
50	CONTINUE
	RETURN
C
C
900	FORMAT(A26)
901	FORMAT(' Message terminator not found, try again     ')
903	FORMAT(I1)
	END
