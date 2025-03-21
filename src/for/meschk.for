C
C SUBROUTINE MESCHK
C $Log:   GXAFXT:[GOLS]MESCHK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:01:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:59:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lodtxt.for **
C
C
C
C SUBROUTINE TO CALCULATE MESSAGE CHECKSUMS AND
C TO CHECK MESSAGES FOR COM CONTROL CHARACTERS
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MESCHK(I4MESS,CMESS,LENGTH)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	BYTE CMESS(512),NEWMES(512)
	INTEGER*4 I4MESS(128)
	INTEGER*4 IND,I,CHKSM,LENGTH
C
C FROM BYTE 3 TO LENGTH
C
	CMESS(3)=0
	CALL CHECKSUM(I4MESS,1,LENGTH,CHKSM)
	CMESS(3)=CHKSM
C
C SCAN BUFFER FOR ANY '@' AND SUBSTITUTE IT WITH '@@'
C
	IND=3
	DO 200 I=3,LENGTH
	NEWMES(IND)=CMESS(I)
	IF(CMESS(I).EQ.'@') THEN
	  IND=IND+1
	  NEWMES(IND)='@'
	ENDIF
	IND=IND+1
200	CONTINUE
	LENGTH=IND-1
C
C
	DO 210 I=3,LENGTH
	CMESS(I)=NEWMES(I)
210	CONTINUE
	RETURN
	END
