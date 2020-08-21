C
C SUBROUTINE I4TOHEX.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]I4TOHEX.FOV                                  $
C  $Date::   17 Apr 1996 13:34:12                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C     I4TOHEX.FTN
C     -----------
C
C     CONVERT INTEGER*4 TABLE INTO CHARACTER*1 ASCII HEX TABLE
C
C
C     CALL I4TOHEX(INTEGER_TABLE,HEX_TABLE,LENGTH_IN_BYTES)
C     IN:
C     INTEGER_TABLE  - DATA TO BE CONVERTED
C     LENGTH_IN_BYTES - # OF BYTES TO BE CONVERTED
C     OUT:
C     HEX_TABLE      - CHARACTER*1 TABLE FOR ASCII HEX OUTPUT
C
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C COPYRIGHT 1994 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE I4TOHEX(INT,HEX,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 INT(*),LEN, OFF, NIBLE
	CHARACTER*1 HEX(*)
C
	DO 10, OFF=1,LEN
	   CALL GETNIBLE(NIBLE,INT,OFF)
	   IF (NIBLE.LT.10) THEN
	      HEX(OFF)=CHAR(NIBLE+48)
	   ELSE
	      HEX(OFF)=CHAR(NIBLE+55)
	   ENDIF
10	CONTINUE
	RETURN
	END
