C SUBROUTINE STUFFIT
C
C V01 04-MAR-96 wsm Extracted from ALLDLL for Finland.
C
C ** Source - alldll.for **
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE STUFFIT (CHAR)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
C     THIS SUBROUTINE LOADS A CHARACTER ARGUMENT INTO THE /MSGCOM/ BUFFER.
C
	INTEGER*4	PTR
C
	CHARACTER*1000	MSGBUF
	CHARACTER*1	CHAR
C
	COMMON /STUFFC/ PTR, MSGBUF
C
	PTR = PTR + 1
	MSGBUF(PTR: PTR) = CHAR
C
	RETURN
	END
C
