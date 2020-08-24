C
C SUBROUTINE X2STRSUM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STRSUM.FOV                                 $
C  $Date::   17 Apr 1996 16:37:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2stnchk.for;1 **
C
C V02 24-MAR-94 GPR SWAPPED THE BYTES IN THE CHKSUM
C
C =================================================================
C X2STRSUM
C
C This subroutine will calculate both the GTECH checksum and the
C IEOR checksum for the input buffer.
C
C Input parameters:
C
C     CHKBUF      Int*2(400)      Buffer to checksum
C     CHKLEN      Int*4           Number of bytes to checksum
C
C Output parameters:
C
C     CHKSUM      Int*2           Checksum (GTECH and IEOR) of buffer.
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
	SUBROUTINE X2STRSUM(CHKBUF,CHKLEN,CHKSUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*2   CHKSUM          !Calculated checksum
	INTEGER*2   CHKBUF(400)     !Checksum buffer
	INTEGER*4   CHKLEN          !Length to checksum
	INTEGER*4   TMPSUM2, TMPSUM1
C
C CALCULATE GTECH CHECKSUM AND STORE IT INTO OUTPUT ARRAY
C BYTE POSITION 0.
C
	CHKSUM=0
	TMPSUM1=0
	TMPSUM2=0
C
C CALCULATE CHECKSUM USING GTECH AND IOR AND STORE IT INTO OUTPUT
C ARRAY BYTE POSITION 0 AND 1.
C
	CALL X2CHKSUM(CHKBUF,0,CHKLEN,TMPSUM1,TMPSUM2)
        CALL ISBYTE(TMPSUM2,CHKSUM,0)                               ! V02
        CALL ISBYTE(TMPSUM1,CHKSUM,1)                               ! V02
C
	RETURN
	END
