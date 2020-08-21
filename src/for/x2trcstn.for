C
C SUBROUTINE X2TRCSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2TRCSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:39:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xmgr.for;1 **
C
C
C++++++++++++++++++++++++++++++++++++++++++
C
C     X2TRCSTN(BUFFER)    ;DUMP 256 BYTES STARTING AT BUFFER
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
	SUBROUTINE X2TRCSTN(BUFFER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*2 BUFFER(*)
	INTEGER*4 FILE_INIT/0/
	INTEGER*4 ST, BLOCK_SIZE1
C
C     THIS FILE SHOULD BE ADDED TO BLDSYS !!!!!
C
	INTEGER*4 X2X_TRACE_FILE1(4) /'X2XT','RAC1','.FIL',0/
	PARAMETER (BLOCK_SIZE1=1)
C
C     INITIALIZE TRACE FILE IF NEEDED
	IF (FILE_INIT.EQ.0) THEN
	   CALL OPENW(X2X_TRACE_UNIT1,X2X_TRACE_FILE1,4,0,0,ST)
	   IF (ST.NE.0) RETURN
	   X2X_TRACE_BLOCK1=0               !STARTING BLOCK NO
	   CALL IOINIT(X2X_TRACE_FDB1,X2X_TRACE_UNIT1,BLOCK_SIZE1*256)
	   CALL GETSIZ_USED(X2X_TRACE_UNIT1,X2X_TRACE_SIZE1)  !FILE SIZE1
	   X2X_TRACE_SIZE1=X2X_TRACE_SIZE1/BLOCK_SIZE1    !IN BLOCKS
	   FILE_INIT=-1                    !INITIALIZED NOW
	ENDIF
C
	X2X_TRACE_BLOCK1=MOD(X2X_TRACE_BLOCK1+1,X2X_TRACE_SIZE1)
	IF (X2X_TRACE_BLOCK1.EQ.0) X2X_TRACE_BLOCK1=1
	CALL WRITEW(X2X_TRACE_FDB1,X2X_TRACE_BLOCK1,BUFFER,ST)
	RETURN
	END
