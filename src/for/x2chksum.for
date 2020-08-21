C
C SUBROUTINE X2CHKSUM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKSUM.FOV                                 $
C  $Date::   17 Apr 1996 16:13:14                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2chksum.for;1 **
C
C X2CHKSUM.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will calculate two checksums:  the
C first checksum uses the standard GTECH algorithmn, and
C the second by XORing each byte of the input buffer.
C
C Calling sequence:
C
C     CALL X2CHKSUM(TABLE,BEGOFF,LENGTH,RESULT1,RESULT2)
C
C Input parameters:
C
C        TABLE    Int*2   Buffer to be checksumed
C        BEGOFF   Int*4   Starting byte (relative to 0)
C        LENGTH   Int*4   Number of bytes to checksum
C
C Output parameters:
C
C        RESULT   Int*4   Checksum of buffer
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
	SUBROUTINE X2CHKSUM(TABLE,BEGOFF,LENGTH,RESULT1,RESULT2)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*2   TABLE(*)          !Buffer to checksum
	INTEGER*4   BEGOFF              !Starting byte
	INTEGER*4   LENGTH              !Number of bytes
	INTEGER*4   RESULT1             !GTECH Checksum of buffer
	INTEGER*4   RESULT2             !IEOR Checksum of buffer
	INTEGER*4   TEMP                !Temp result buffer
	INTEGER*4   I                   !Work variable
C
C CALCULATE GTECH CHECKSUM.
C
C***      TYPE *,'=========== BUFFER =========='
C***      WRITE(5,9000) (TABLE(I),I=1,(LENGTH/2)+1)
C***9000  FORMAT(5X,20(8(Z4,1X),/))
 
	RESULT1=0
	DO 100 I=BEGOFF,BEGOFF+LENGTH-1
	  CALL ILBYTE(TEMP,TABLE,I)
	  RESULT1=IAND(I+37*(RESULT1+TEMP),'FF'X)
100	CONTINUE
C
C CHECKSUM BUFFER.
C
	RESULT2=0
	DO 200 I=BEGOFF,BEGOFF+LENGTH-1
	  CALL ILBYTE(TEMP,TABLE,I)
	  RESULT2=IEOR(RESULT2,TEMP)
200	CONTINUE
C
	RETURN
	END
