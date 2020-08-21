C
C $Log:   GXAFIP:[GOLS]SETMES.FOV  
C  
C     Rev 1.2   01 Feb 1997 17:40:14   RXK
C  Changes for CDU.
C  
C     Rev 1.1   13 Jan 1997 17:08:52   RXK
C  CDU, phase 2 
C  
C     Rev 1.0   17 Apr 1996 15:01:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   12 Jul 1993 20:08:22   GXA
C  Initial revision.
C                                                                               
C SUBROUTINE TO SET GAME TICKET MARKETING MESSAGES ONLINE
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SETMES(MSG,LINES,MTAB)
	IMPLICIT NONE 
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C                                                           
	INTEGER*4 CBUF(CDLEN),MTAB(TICKET_LENGTH,TICKET_ROWS)
	INTEGER*4 MSG, LINES, I                                           
C
C
	IF(MSG.LT.1.OR.MSG.GT.MAXGAM+PRM_NUMOPN+NUMCDU) THEN
	   TYPE*,IAM(),'Invalid message number ',MSG,' message not set'
	   RETURN 
	ENDIF 
C                                                                               
C DISABLE CURRENT MESSAGE                                                       
C                                                                               
	CALL FASTSET(0,CBUF,CDLEN) 
	CBUF(1) = 7
	CBUF(3) = TCGEN
	CBUF(5) = MSG
	CBUF(6) = 'SYS '
	CALL COMMAND(CBUF) 
        IF(LINES.EQ.0) GOTO 100
CRXK	IF(LINES.EQ.0) RETURN
C                                                                               
C SET NEW MESSAGE                                                               
C                                                                               
	DO I=1,LINES 
	   CALL FASTSET(0,CBUF,CDLEN)
	   CBUF(1) = 5 
	   CBUF(2) = 1
	   CBUF(3) = TCGEN
	   CBUF(4) = I
	   CBUF(5) = MSG
	   CBUF(6) = 'SYS '
	   CALL FASTMOV(MTAB(1,I),CBUF(8),4) 
	   CALL COMMAND(CBUF)
	   CBUF(2) = 5
	   CALL FASTMOV(MTAB(5,I),CBUF(8),4)
	   CALL COMMAND(CBUF)
	END DO
C                                                                               
C ENABLE MESSAGE                                                                
C                                                                               
	CALL FASTSET(0,CBUF,CDLEN)
	CBUF(1) = 6
	CBUF(2) = LINES
	CBUF(3) = TCGEN
	CBUF(5) = MSG
	CBUF(6) = 'SYS '
	CALL COMMAND(CBUF)
C
C CHANGE REVISION #
C
100     CONTINUE
	CBUF(1) = 8
	IF(MSG.LE.MAXGAM+PRM_NUMOPN) THEN
	   CBUF(2) = TKTMRV(MSG)
        ELSE
	   CBUF(2) = TKTCDU
        ENDIF
	CBUF(3) = TCGEN
	CBUF(5) = MSG
	CBUF(6) = 'SYS '
	CALL COMMAND(CBUF)
C
	RETURN
	END
