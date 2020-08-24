C
C SUBROUTINE OPS1
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]OPS1.FOV                                     $
C  $Date::   17 Apr 1996 14:19:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ops1.for **
C
C OPS1.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V02 MODIFIED OPS TO PASS ENCODE_FORMAT AND PARAMETERS
C V01 27-JUN-89 MBK ORIGINAL RELEASE
C
C CALL OPS1(PAR0,PAR1,PAR2,PAR3,ENCODE_FORMAT)
C
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
	SUBROUTINE OPS1(STRING,PAR0,PAR1,PAR2,PAR3,ENCODE_FORMAT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER*(*) ENCODE_FORMAT
	CHARACTER*(*) STRING
	INTEGER*4 PAR0,PAR1,PAR2,PAR3,ST
C
	CHARACTER*64 BUFFER
	CHARACTER*1  CHBUFFER(132)
	CHARACTER*24 TEXT
	CHARACTER*60 VALUES
	EQUIVALENCE (BUFFER,CHBUFFER,TEXT)
	EQUIVALENCE (CHBUFFER(25),VALUES)
C
C
	TEXT=STRING
	WRITE (VALUES,ENCODE_FORMAT) PAR0,PAR1,PAR2,PAR3
C
	CALL MLOG(BUFFER,ST)
C
	RETURN
	END
