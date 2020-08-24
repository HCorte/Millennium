C
C SUBROUTINE X2MSGEDT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2MSGEDT.FOV                                 $
C  $Date::   17 Apr 1996 16:24:14                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C  V02  26-DEC-1995 DAS INCREASED ALLOWABLE NUMBER OF SEGMENTS (I2 -> I4)
C
C ** Source - x2sndbuf.for;2 **
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2MSGEDT(MESSAGE,BUFFER,MES_NUM,MES_LEN,ADD_LEN,COM) EDIT MSG
C
C     IN:
C     MESSAGE  - BUFFER OF DATA WITH MESSAGE
C     MES_NUM  - MESSAGE # USED
C     MES_LEN  - MESSAGE LENGTH
C     COM      - 0 IF NEW COMM TASK, .NON.0 IF OLD
C     OUT:
C     BUFFER   - BUFFER WITH DATA
C     ADD_LEN  - OUTPUT LENGTH
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
	SUBROUTINE X2MSGEDT(MESSAGE,BUFFER,MES_NUM,MES_LEN,ADD_LEN,COM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
	INTEGER*2 MESSAGE(*)               !MESSAGE WITH DATA
	INTEGER*4 BUFFER(*)                !BUFFER TO BE FILLED
	INTEGER*4 MES_NUM                  !MESSAGE NUMBER TO EDIT
	INTEGER*4 MES_LEN                  !LENGTH OF MESSAGE IN MESS
	INTEGER*4 ADD_LEN                  !LENGTH OF MESSAGE IN BUFF
	INTEGER*4 AT_SIGN /'00000040'X/
	INTEGER*4 TO_ADD, DATA, POINTER, MSG_PTR, LAST, TO_MOVE
	INTEGER*4 LENGTH, NEW_MESSAGE, COM
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	   TYPE *,'MSGEDT ',MES_NUM,MES_LEN,COM
	IF (COM.EQ.0) THEN
	   IF (MES_LEN.NE.0)
     *	        MES_LEN=MES_LEN/OUTLEN_MAX+MOD(MES_LEN,OUTLEN_MAX)
	   IF (MES_LEN.GT.0) CALL MOVBYT(MESSAGE,1,BUFFER,1,MES_LEN)
	   ADD_LEN=MES_LEN
C
	   NEW_MESSAGE=MES_NUM
C
10	   CONTINUE
	   IF (NEW_MESSAGE.LE.0) GOTO 9000
	   IF (NEW_MESSAGE.GE.MSGCTL) GOTO 9000
	   LENGTH=I4MSGTAB(SEGLEN,NEW_MESSAGE)
	   IF (LENGTH.LE.0) GOTO 9000
	   IF (LENGTH.GT.MSGLST) GOTO 9000
	   CALL MOVBYT(MSGTAB(1,NEW_MESSAGE),1,BUFFER,ADD_LEN+1,
     *	               LENGTH)
	   ADD_LEN=ADD_LEN+LENGTH
	   NEW_MESSAGE=I4MSGTAB(MSGLNK,NEW_MESSAGE)
	   GOTO 10
	ELSE
	   TO_MOVE=0
	   IF (MES_LEN.NE.0) THEN
	      TO_MOVE=MES_LEN/OUTLEN_MAX
	      MES_LEN=MOD(MES_LEN,OUTLEN_MAX)
	      IF (TO_MOVE.GT.0)
     *	      CALL MOVBYT(MESSAGE,1,BUFFER,1,TO_MOVE)
	   ENDIF
	   ADD_LEN=TO_MOVE
	   NEW_MESSAGE=MES_NUM
	   LAST=0
	   MSG_PTR=TO_MOVE+1
C
20	   CONTINUE
	   IF (NEW_MESSAGE.LE.0) GOTO 9000
	   IF (NEW_MESSAGE.GE.MSGCTL) GOTO 9000
	   LENGTH=I4MSGTAB(SEGLEN,NEW_MESSAGE)
           IF (LENGTH.LE.0) GOTO 9000
	   IF (LENGTH.GT.MSGLST) GOTO 9000
	   DO 50, POINTER=1,LENGTH
	      CALL ILBYTE(DATA,MSGTAB(1,NEW_MESSAGE),POINTER-1)
	      IF (LAST.NE.AT_SIGN) THEN        !OLD ESCAPE SEQUENCE
	         LAST=DATA
	         IF (DATA.EQ.AT_SIGN) GOTO 50  !NEW ESCAPE SEQ
	         CALL ISBYTE(DATA,BUFFER,ADD_LEN)
	         ADD_LEN=ADD_LEN+1
	         GOTO 50
	      ELSE           !CONTINUATION OF ESCAPE SEQUENCE
	         LAST=0
	         IF (DATA.EQ.AT_SIGN) THEN   !DUBLE ESCAPE
	            CALL ISBYTE(DATA,BUFFER,ADD_LEN)
	            ADD_LEN=ADD_LEN+1
	            GOTO 50
	         ELSE
C
C     SHOULD BE 1:9 ASCII
C
	          IF (DATA.LT.'00000031'X.OR.DATA.GT.'00000039'X)
     *	          GOTO 9000
	          TO_ADD=DATA-'00000030'X
	          IF (MES_LEN.GT.0) THEN
	             IF (MES_LEN-TO_ADD.LT.0) THEN
	                TO_ADD=MES_LEN
	                MES_LEN=-1
	             ENDIF
	             MES_LEN=MES_LEN-TO_ADD
	           CALL MOVBYT(MESSAGE,MSG_PTR,BUFFER,ADD_LEN+1,TO_ADD)
	            ADD_LEN=ADD_LEN+TO_ADD
	            MSG_PTR=MSG_PTR+TO_ADD
	          ENDIF
	         ENDIF
	       ENDIF
50	   CONTINUE
	   NEW_MESSAGE=I4MSGTAB(MSGLNK,NEW_MESSAGE)
	   GOTO 20
	ENDIF
C
9000	CONTINUE
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	    TYPE *,'RET X2MSGEDT ',ADD_LEN
	RETURN
	END
