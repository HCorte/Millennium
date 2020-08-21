C
C V02 14-JUN-2000 UXN HBLKRDY added.
C V01 08-OCT-1999 UXN Initial release.
C
C This file contains following subroutines and functions -
C
C            SUBROUTINE GOLS_OPEN_TMF(FDB, TMF_NAME, ST)
C            SUBROUTINE GOLS_READ_TMF(FDB,BLOCK,BUFFER,ST)
C            SUBROUTINE GOLS_CLOSE_TMF(FDB)
C 	     SUBROUTINE GOLS_TMF_NAME(NAME)
C  INTEGER*4 FUNCTION   GOLS_ENVIRONMENT()
C  INTEGER*4 FUNCTION   GOLS_SYSTYPE()
C  INTEGER*4 FUNCTION   GOLS_CDC()
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Subroutine to open TMF file.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE GOLS_OPEN_TMF(FDB, TMF_NAME, ST)
	IMPLICIT NONE
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 FDB(*)
	INTEGER*4 TMF_NAME(*)
	INTEGER*4 ST
C
	INTEGER*4 LUN
C
	LUN = 7
	CALL GETLUN(LUN)
	CALL OPENW(LUN,SFNAMES(1,PTMF),4,0,0,ST)
        CALL IOINIT(FDB,LUN,32*256)
	CALL FASTMOV(SFNAMES(1,PTMF), TMF_NAME, 5)
	END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Subroutine to read a block from TMF file.
C GOLS_READ_TMF(FDB,BLOCK,BUFFER,ST)
C Parameters -
C FDB(*) - file description block
C BLOCK  - tmf block number
C BUFFER - buffer of tmf block data
C ST     - 0- ok
C          1- block is not available yet
C          2- last block of TMF for this CDC
C        144- end of file
C         -1- invalid block number or FDB
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE GOLS_READ_TMF(FDB,BLOCK,BUFFER,ST)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:DISKIO.DEF'
C
	INTEGER*4 FDB(*), BLOCK, BUFFER(*),ST
	INTEGER*4 BLK
	LOGICAL   LAST
C
	ST   = 0
	BLK  = HBLKRDY
C
	IF(BLOCK.GT.BLK) THEN
	    ST = 1		    ! BLOCK NOT AVAILABLE YET
	    RETURN
	ENDIF

	LAST = BLOCK.EQ.BLK .AND. DAYSTS.EQ.DSCLOS
C

	IF(FDB(FDB_LUN).LE.0 .OR. BLOCK.LE.0) THEN  
	    ST = -1 ! Invalid block or FDB
	ELSE
	    CALL READW(FDB,BLOCK,BUFFER,ST)
	    IF(LAST.AND.ST.EQ.0) ST = 2
	ENDIF
C
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Subroutine to close TMF file.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE GOLS_CLOSE_TMF(FDB)
	IMPLICIT NONE
	INTEGER*4 FDB(*)
C
	CALL CLOSEFIL(FDB)
C
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	INTEGER*4 FUNCTION GOLS_ENVIRONMENT()
	IMPLICIT NONE
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'

	GOLS_ENVIRONMENT = P(TSTMOD)
	
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	INTEGER*4 FUNCTION GOLS_SYSTYPE()
	IMPLICIT NONE
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'

	GOLS_SYSTYPE = P(SYSTYP)
	
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	INTEGER*4 FUNCTION GOLS_CDC()
	IMPLICIT NONE
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'

	GOLS_CDC = DAYCDC
	
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	SUBROUTINE GOLS_TMF_NAME(NAME)
	IMPLICIT NONE
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 NAME(*)

	CALL FASTMOV(SFNAMES(1,PTMF), NAME, 5)

	END
