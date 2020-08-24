C
C SUBROUTINE ENCACT
C $Log:   GXAFXT:[GOLS]ENCACT.FOV  $
C  
C     Rev 1.1   17 May 1996 11:42:46   HXK
C  Update from Wojtek, Siew Mun
C  
C     Rev 1.0   21 Jan 1993 16:12:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_actagt.for **
C
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ENCACT(LINE,ASTAT,I)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 ASTAT(4,54),LINE(20),I, J
C
	INTEGER*4    XLINE(20)
	CHARACTER*80 CLINE
	EQUIVALENCE (CLINE,XLINE)
C
	CHARACTER*3 OPS(2)
	CHARACTER*6 STAT(2)
	DATA OPS/'off','on '/
	DATA STAT/'active','*idle*'/
C
C
	WRITE(CLINE,900) ASTAT(2,I),ASTAT(1,I),
     *	                 OPS(ASTAT(3,I)),STAT(ASTAT(4,I)),
     *	                 ASTAT(2,I+1),ASTAT(1,I+1),
     *	                 OPS(ASTAT(3,I+1)),STAT(ASTAT(4,I+1)),
     *	                 ASTAT(2,I+2),ASTAT(1,I+2),
     *	                 OPS(ASTAT(3,I+2)),STAT(ASTAT(4,I+2))
900	FORMAT(3(I9.9,' ',I5,' ',A3,' ',A6,1X))
	DO 910 J = 1,20
	  LINE(J) = XLINE(J)
910	CONTINUE
	RETURN
	END
