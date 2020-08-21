C
C SUBROUTINE PAD0
C $Log:   GXAFXT:[GOLS]PAD0.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:21:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:15:04   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_altrlvl.for **
C
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PAD0(CNUM,SNUM,N,C)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 SNUM, N
	CHARACTER CNUM*5, TEMP*5, C*1
	INTEGER*4 J, K, M
C
C
	TEMP = '     '
	CNUM=ITOC(SNUM,K)
	IF(K.NE.N) THEN
	  J=K
	  DO 100 M=K,1,-1
	    TEMP(M:M)=CNUM(M:M)
	    CNUM(J+(N-M):J+(N-M))=TEMP(M:M)
	    J=J-2
100	  CONTINUE
	  DO 200 M=1,N-K
	    CNUM(M:M)=C
200	  CONTINUE
	ENDIF
	RETURN
	END
