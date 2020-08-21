C
C SUBROUTINE NTBET
C $Log:   GXAFXT:[GOLS]NTBET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:14:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:10:04   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - printra.for **
C
C
C BUILD BET IMAGE FOR NUMBERS TRANSACTIONS
C ==============================================================
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NTBET(TRABUF,CBETS,LINES)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
C
        INTEGER*4 I, POOL, LINES
        INTEGER*4 NNUM(NMAXBT),NPOL(NMAXBT),NAMT(NMAXBT)
        CHARACTER*56 CBETS(12)
        DATA POOL/0/
C
        DO 20 I=1,TRABUF(TWNBET)
          NAMT(I)=TRABUF(TWNAMT1+(I-1)*3)
          NNUM(I)=TRABUF(TWNNUM1+(I-1)*3)
          NPOL(I)=TRABUF(TWNPOL1+(I-1)*3)
20      CONTINUE
C
        DO 30 I=1,TRABUF(TWNBET)
          IF(TRABUF(TWNTYP).EQ.NB3TYP) THEN
            POOL=P3NAMES(NPOL(I))
	    IF(NPOL(I).EQ.TNB3F2) THEN
              WRITE (CBETS(I),902) POOL,CMONY(NAMT(I),6,BETUNIT),
     *		                   NNUM(I)
            ELSE IF(NPOL(I).EQ.TNB3B2) THEN
              WRITE (CBETS(I),903) POOL,CMONY(NAMT(I),6,BETUNIT),
     *		                   NNUM(I)
	    ELSE
              WRITE (CBETS(I),900) POOL,CMONY(NAMT(I),6,BETUNIT),
     *		                   NNUM(I)
	    ENDIF
	  ENDIF
          IF(TRABUF(TWNTYP).EQ.NB4TYP) THEN
            POOL=P4NAMES(NPOL(I))
            WRITE (CBETS(I),901) POOL,CMONY(NAMT(I),6,BETUNIT),
     *	                         NNUM(I)
  	  ENDIF
30      CONTINUE
	LINES=TRABUF(TWNBET)+1
        RETURN
C
900     FORMAT(1X,A4,1X,A6,1X,I3.3)
901     FORMAT(1X,A4,1X,A6,1X,I4.4)
902     FORMAT(1X,A4,1X,A6,1X,I2.2,'X')
903     FORMAT(1X,A4,1X,A6,1X,'X',I2.2)
	END
