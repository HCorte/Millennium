C SUBROUTINE WRITMSG
C
C
C V01 04-MAR-96 wsm Extracted from ALLDLL for Finland.
C
C ** Source - alldll.for **
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C WRITE MSGBUF INTO MSGCOM OR DISCIMG:
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE WRITMSG(IMGCHR, MSGNUM, MSGBUF)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:MSGCOM.DEF'
C
	INTEGER*4	CURSOR,
     *			FREESEG,
     *			LENGTH,
     *			LOCMSG,
     *			MSGNUM,
     *                  DUMMY,
     *			NEXTSEG
C
        CHARACTER*(MSGSZH*2) IMGCHR(*)
C
	CHARACTER*(*)	MSGBUF
C
	LOCMSG = MSGNUM					! LOCAL COPY
C
	IF (I4MSGTAB(SEGLEN, LOCMSG) .GT. 0) CALL FLXDELETE(LOCMSG)
	IMGCHR(LOCMSG)(:MIN(LEN(MSGBUF), MSGLST)) = MSGBUF
	IF (LEN(MSGBUF) .LE. MSGLST) GOTO 20		! SET NULL LINK & EXIT.
C
	DO 10 CURSOR = MSGLST + 1, LEN(MSGBUF), MSGLST
	  NEXTSEG = FREESEG(DUMMY)
	  IF (NEXTSEG .LE. 0) THEN
	    WRITE(6, *) 'ERROR: OUT OF SPACE. MESSAGE PARTIALLY WRITTEN.'
	    GOTO 20 					! SET NULL LINK & EXIT.
	  ENDIF
	  I4MSGTAB(MSGLNK, LOCMSG) = NEXTSEG		! SET LINK.
	  LOCMSG = NEXTSEG
	  LENGTH = LEN(MSGBUF) - CURSOR + 1
	  IMGCHR(LOCMSG)(:MIN(MSGLST, LENGTH)) =
     *    MSGBUF(CURSOR: CURSOR + MIN(MSGLST, LENGTH) - 1)
	  I4MSGTAB(SEGLEN, LOCMSG) = MIN(LENGTH, MSGLST)
10	CONTINUE
C
C WAIT UNTIL NOW TO SET LENGTH OF FIRST SEGMENT IN ORDER TO LOCK
C MESSAGE UNTIL IT IS COMPLETELY WRITTEN, TO MAINTAIN INTEGRITY:
C
20	I4MSGTAB(SEGLEN, MSGNUM) = MIN(MSGLST, LEN(MSGBUF))
	I4MSGTAB(MSGLNK, LOCMSG) = 0			! LAST / ONLY SEGMENT.
C
	RETURN
	END
C
