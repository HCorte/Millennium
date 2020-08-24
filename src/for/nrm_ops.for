C
C SUBROUTINE OPS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]OPS.FOV                                      $
C  $Date::   17 Apr 1996 14:19:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ops.for;1 **
C
C OPS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 27-JUN-89 MBK ORIGINAL RELEASE
C
C CALL OPS("STRING",INTVAL,HEXVAL)
C
C INTVAL - WILL BE PRINTED IN INT FORMAT
C HEXVAL - WILL BE PRINTED IN HEX FORMAT
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
	SUBROUTINE OPS(STRING,INTVAL,HEXVAL)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER*(*) STRING
	INTEGER*4     INTVAL
	INTEGER*4     HEXVAL
	INTEGER*4     ST
C
	CHARACTER*132 BUFFER
        CHARACTER*50  TEXT
C
	TEXT=STRING
C
	WRITE(UNIT=BUFFER,FMT=900) TEXT, INTVAL,HEXVAL
900	FORMAT(A,':I[',I11,']H[',Z8,']')
C
	CALL MLOG(BUFFER,ST)
C
	RETURN
	END
