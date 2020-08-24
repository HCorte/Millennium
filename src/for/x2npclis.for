C
C SUBROUTINE X2NPCLIS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2NPCLIS.FOV                                 $
C  $Date::   17 Apr 1996 16:25:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2npclis.for **
C
C X2NPCLIS.FOR
C
C V02 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will display defined ports in the
C network port configuration file to the screen.  The user
C will have the option of displaying a specific port,
C or to page forward or back.
C
C Calling sequence:
C
C     CALL X2NPCLIS
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
	SUBROUTINE X2NPCLIS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
C
	INTEGER*4   NPC,NPCCNT          !Rec #/station count
	INTEGER*4   LASTNPC             !Last station displayed
	INTEGER*4   EOFCNT              !End of file count
	INTEGER*4   I,ST,K,ERR          !Work variables
	INTEGER*4   J, PREV, LAST_PREV
	CHARACTER   PROMPT*65           !Output prompt
	CHARACTER   OPT*4               !Input option
	CHARACTER   CHRSTR(20)          !ASCII for BCD address
	CHARACTER   CHRSTR2(20)         !ASCII for BCD address
	CHARACTER   X2FILNAM*20         !File name function
	CHARACTER   STATE(0:3)*10       !Port state
	CHARACTER   YESNO(0:1)*3        !Yes/no
	LOGICAL     MORE                !More data flag
C
	DATA        YESNO /'yes',' no'/
	DATA        STATE /' undefined','      idle',
     *	                   '    online','      down'/
C
C DISPLAY THE NEXT SCREEN OF PORTS.
C
	NPC=0
	LASTNPC=0
	LAST_PREV=0
	PREV=0
100	CONTINUE
	IF(NPC.LT.0) NPC=0
	EOFCNT=0
	NPCCNT=0
	CALL CLRSCR(5)
	WRITE(5,9080)
	WRITE(5,9090)
C
C READ A STATION FROM THE FILE AND DISPLAY
C IT TO THE SCREEN.
C
200	CONTINUE
CV02	IF(NPCCNT.LT.10 .AND. EOFCNT.LE.100) THEN
CV02250	  NPC=NPC+1
	IF(NPCCNT.LT.10 .AND. NPC.LE.X2X_NETWORK_PORTS) THEN	!V02
	  NPC=NPC+1						!V02
	  CALL READW(X2XNPC_FDB,NPC,X2XNPC_REC,ST)
	  IF(ST.EQ.144) GOTO 300
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XNPC),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XNPC_REC(1).LE.0) THEN
CV02	    GOTO 250
	    GOTO 200						!V02
	  ELSE
	    EOFCNT=0
	  ENDIF
C
C DISPLAY THE PORT TO THE SCREEN.
C
	  NPCCNT=NPCCNT+1
	  IF(NPCCNT.EQ.1) THEN
	    LAST_PREV=PREV
	    PREV=NPC
	  ENDIF
	  LASTNPC=NPC
	  CALL HTOA(CHRSTR,1,X2XNPC_ADDLEN,X2XNPC_ADDRES,ERR)
	  CALL HTOA(CHRSTR2,1,X2XNPC_ADDLEN,X2XNPC_HUNTADR,ERR)
	  DO 252 J=X2XNPC_ADDLEN+1,20
	    CHRSTR(J)=' '
	    CHRSTR2(J)=' '
252	  CONTINUE
	  WRITE(5,9100) X2XNPC_PORT,(CHRSTR(I),I=1,16),X2XNPC_CAPACITY,
     *	                STATE(X2XNPC_STATE), YESNO(X2XNPC_ASSIGN),
     *	               (CHRSTR2(I),I=1,16)
	  GOTO 200
	ENDIF
C
C IF NO PORTS EXIST, DISPLAY AN APPROPRIATE MESSAGE.
C
300	CONTINUE
	IF(NPCCNT.EQ.0) THEN
	  WRITE(5,9050)
	ENDIF
C
C BUILD THE APPROPRIATE OUTPUT PROMPT.
C
	IF(EOFCNT.LE.100) THEN
	  WRITE (PROMPT,9000)
	  MORE=.TRUE.
	ELSE
	  WRITE (PROMPT,9010)
	  MORE=.FALSE.
	ENDIF
C
C ASK USER WHAT THEY WANT TO DO.
C
	WRITE(5,*)
	WRITE(5,*)
350	CONTINUE
	CALL WIMG(5,PROMPT)
	READ(5,9020) OPT
	IF(OPT(1:1).EQ.'E'.OR.OPT(1:1).EQ.'e') THEN
	  GOTO 8000
	ELSE IF(MORE.AND.(OPT(1:1).EQ.'N'.OR.OPT(1:1).EQ.'n')) THEN
	  NPC=LASTNPC
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'P'.OR.OPT(1:1).EQ.'p') THEN
	  NPC=LAST_PREV-1
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'X'.OR.OPT(1:1).EQ.'x') THEN
	  NPC=CTOI(OPT(2:4),K)-1
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'H'.OR.OPT(1:1).EQ.'h') THEN
	  CALL X2XHLP('X2NPCLIS.HLP')
	  NPC=PREV-1
	  GOTO 100
	ELSE
	  WRITE(5,9040)
	  GOTO 350
	ENDIF
C
C PROGRAM EXIT
C
8000	CONTINUE
	RETURN
C
C     ==================== Format Statements ===================
C
9000	FORMAT(10(' '),'Enter N for next, P for Previous, ',
     *	               'or X# for port')
9010	FORMAT(10(' '),'Enter P for previous or X# for port',18(' '))
9020	FORMAT(A4)
9040	FORMAT(15(' '),'Invalid option input ')
9050	FORMAT(15(' '),'Sorry, no more ports exist ')
9080	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T28,'Network Port Listing',//)
9090	FORMAT(T12,'Port',T23,'Address',T33,'Capacity',
     *       T45,'State',T52,'Assign',T61,'Hunt Address',/,11X,61('='))
9100	FORMAT(T12,I4,T18,16A,T35,I3,T42,A10,T54,A3,T59,16A)
	END
