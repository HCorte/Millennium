C
C SUBROUTINE EBCASC
C $Log:   GXAFXT:[GOLS]EBCASC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:03:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:11:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_ebcasc.for **
C
C EBCASC.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C       CALL EBCASC(STRING,ERR)
C		 STRING   = CHARACTER STRING
C                   ERR   = 0 IF ALL GOOD EBC CHARS, -1 IF NOT ALL GOOD
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	  SUBROUTINE EBCASC(STRING,ERR)
	  IMPLICIT NONE
C
	  CHARACTER STRING*(*)
	  INTEGER*4 ERR
C
	  INTEGER*4 I, K
C
	  CHARACTER ASC(0:255)
C
	DATA (ASC(K),K=0,99)/
     1	       60*'~',
     6	       4*'~',' ',5*'~',
     7	       05*'~','.','<','(','+','~',
     8	          '&',09*'~',
     9	       '!','$','*',')','!','~','-','/',2*'~'/
	DATA (ASC(K),K=100,255)/
     1	       7*'~',',','%','_',
     1	       '>','?',8*'~',
     2	       2*'~',':','#','@',' ','=','"','~','A',
     3	       'B','C','D','E','F','G','H','I','~','~',
     4	       '~','~','~','~','~','J','K','L','M','N',
     5	       'O','P','Q','R',6*'~',
     6	       '~','~','S','T','U','V','W','X','Y','Z',
     7	       10*'~',
     8	       10*'~',
     9	       '~','~','~','A','B','C','D','E','F','G',
     1	       'H','I','~','~','~','~','~','~','~','J',
     1	       'K','L','M','N','O','P','Q','R','~','~',
     2	       4*'~','\','~','S','T','U','V',
     3	       'W','X','Y','Z',6*'~',
     4	       '0','1','2','3','4','5','6','7','8','9',
     5	       6*'~'/
C
C
	  ERR=0
	  DO 1010 I=1,LEN(STRING)
	    STRING(I:I)=ASC(ICHAR(STRING(I:I)))
	    IF(STRING(I:I).EQ.'~') THEN
	      STRING(I:I)=' '
	      ERR=-1
	    ENDIF
1010	  CONTINUE
C
C
8000	  RETURN
	  END
