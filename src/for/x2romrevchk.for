C
C X2ROMREVCHK.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2ROMREVCHK.FOV                              $
C  $Date::   17 Apr 1996 16:32:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C X2ROMREVCHK.FOR
C
C V02 15-SEP-96 das changes for bckground load
C V01 13-SEP-95 SCD RELEASED FOR VAX
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
C
	PROGRAM X2ROMREVCHK
C	This test program uses subroutine READ_ROM_REV to verify that the
C	contents of the X2X ROM Revision file are correct.

	IMPLICIT NONE

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'

	INTEGER*4	STATUS,AUX_STATUS

C	Error status values
	INTEGER*4	OPEN_ERROR		!file open error 
	PARAMETER      (OPEN_ERROR = 1)
	INTEGER*4	INV_BLOCK_TYPE		!invalid block type error 
	PARAMETER      (INV_BLOCK_TYPE = 2)
	INTEGER*4	MAX_MCP_REC_EXCEEDED	!number of records exceeds 
	PARAMETER      (MAX_MCP_REC_EXCEEDED = 3)!MAXMCP or MAXFCL error 
	INTEGER*4	MAX_SFT_REC_EXCEEDED	!number of records exceeds 
	PARAMETER      (MAX_SFT_REC_EXCEEDED = 4)!MAXSFT or MAXGVT error 

	LOGICAL*4	ALL_ZERO
	INTEGER*2 	I

	CALL READ_ROM_REV (STATUS,AUX_STATUS)
	IF (STATUS .NE. 0) THEN
	    IF (STATUS .EQ. OPEN_ERROR) THEN
     		TYPE *,' ROM REV file error - VMS error code ',AUX_STATUS
	    ELSEIF (STATUS .EQ. INV_BLOCK_TYPE) THEN
     		TYPE *,' ROM REV file contains an invalid block type'
	    ELSEIF (STATUS .EQ. MAX_MCP_REC_EXCEEDED) THEN
     		TYPE *,' Record count in ROM REV file exceeds MCP or FCL max ',
     *		       AUX_STATUS
	    ELSEIF (STATUS .EQ. MAX_SFT_REC_EXCEEDED) THEN
     		TYPE *,' Record count in ROM REV file exceeds SFT or GVT max ',
     *		       AUX_STATUS
	    ENDIF				!on error status type check
	ENDIF					!on error status check

C	Check to see if there are any non-zero terminal ROM revs
	
	ALL_ZERO = .TRUE.
	I = 1
	DO WHILE (ALL_ZERO .AND. I .LE. MAXMCP)
	   IF (MCPROM(I) .NE. 0) THEN
	       ALL_ZERO = .FALSE.
	   ELSE
	       I = I+1
	   ENDIF
	END DO

	IF (.NOT. ALL_ZERO) THEN
	    WRITE (6,10)
	    DO I = 1,MAXMCP
	       IF (MCPROM(I) .NE. 0) WRITE (6,15) MCPROM(I),MCPNAM(I)
	    END DO
	    WRITE (6,5)
	ELSE
	    WRITE (6,50) 'MCP'
	ENDIF

C	Check to see if there are any non-zero terminal ID numbers
	
	ALL_ZERO = .TRUE.
	I = 1
	DO WHILE (ALL_ZERO .AND. I .LE. MAXFCL)
	   IF (FCLTIN(I) .NE. 0) THEN
	       ALL_ZERO = .FALSE.
	   ELSE
	       I = I+1
	   ENDIF
	END DO

	IF (.NOT. ALL_ZERO) THEN
	    WRITE (6,20)
	    DO I = 1,MAXFCL
	       IF (FCLTIN(I) .NE. 0) WRITE (6,25) FCLTIN(I),FCLNAM(I)
	    END DO
	    WRITE (6,5)
	ELSE
	    WRITE (6,50) 'FCL'
	ENDIF

C	Check to see if there are any non-zero GVT SOFTLOADER ROM revs
	
	ALL_ZERO = .TRUE.
	I = 1
	DO WHILE (ALL_ZERO .AND. I .LE. MAXSFT)
	   IF (SFTROM(I) .NE. 0) THEN
	       ALL_ZERO = .FALSE.
	   ELSE
	       I = I+1
	   ENDIF
	END DO

	IF (.NOT. ALL_ZERO) THEN
	     WRITE (6,30)
	     DO I = 1,MAXSFT
	        IF (SFTROM(I) .NE. 0) WRITE (6,35) SFTROM(I),
     *						   SFTNAM(SFT_LIST(I))
     *					 	   
	     END DO
	     WRITE (6,5)
	ELSE
	    WRITE (6,50) 'SFT'
	ENDIF

C	Check to see if there are any non-zero GVT APPLICATION revs
	
	ALL_ZERO = .TRUE.
	I = 1
	DO WHILE (ALL_ZERO .AND. I .LE. MAXGVT)
	   IF (GVTROM(I) .NE. 0) THEN
	       ALL_ZERO = .FALSE.
	   ELSE
	       I = I+1
	   ENDIF
	END DO

	IF (.NOT. ALL_ZERO) THEN
	     WRITE (6,40)
	     DO I = 1,MAXGVT
	        IF (GVTROM(I) .NE. 0) WRITE (6,45) GVTROM(I),
     *						   GVTNAM(GVT_LIST(I))
	     END DO
	     WRITE (6,5)
	ELSE
	    WRITE (6,50) 'GVT'
	ENDIF

5	FORMAT (/)
10	FORMAT (' MCP information',/,'  ROM ID (hex)',14x,'MCP NAME',/)
15	FORMAT (3X,Z8,19X,A4)
20	FORMAT (' FCL information',/,'  FCL TIN (hex)',13x,'FCL NAME',/)
25	FORMAT (7X,Z2,21X,A4)
30	FORMAT (' SFT information',/,
     *		'  GVT ROM ID (hex)',10X,'SOFTLOADER NAME',/)
35	FORMAT (3X,Z4,23X,A4)
40	FORMAT (' GAP information',/,
     *		'  GVT APPLICATION ID (hex)',2X,'GVT APPLICATION NAME',/)
45	FORMAT (3X,Z4,23X,A4)
50	FORMAT (' *** NO ',A3,' records found in ROM Revision file ***',/)

	STOP
	END
