C
C V02 11-APR-2001 UXN TMF sizes updated.
C V01 27-FEB-2001 UXN Initial release.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM TMFMON
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 EXTEND_BLKS
	PARAMETER(EXTEND_BLKS = 4000) ! NUMBER OF 8kB blocks.

	INTEGER*4 FREE_BLOCK_CNT
	PARAMETER(FREE_BLOCK_CNT = 1000) ! NUMBER OF EMPTY 8kB blocks in TMF

 	INTEGER*4 ZEROS(2048)/2048*0/
	INTEGER*4 MFDB(7),BFDB(7),ST,I
	INTEGER*4 MAXINT
	PARAMETER(MAXINT='7FFFFFFF'X)
C
	INTEGER*4 DISKSW_SAV
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
5	CONTINUE
C
C OPEN both TMF files.
C
	CALL OPENW(1,SFNAMES(1,PTMF),4,0,0,ST)
	CALL IOINIT(MFDB,1,32*256)
	IF(ST.NE.0) THEN
	   CALL OPS('Error opening primary TMF file',ST,ST)
	   MTM_BLK_CNT = MAXINT      ! LET LOGGER DEAL WITH ERRORS
	ENDIF

	CALL VAXGETFSIZ(1,SFSIZES(PTMF))

	IF(P(DISKSW).NE.0) THEN
          CALL OPENW(2,SFNAMES(1,BTMF),4,0,0,ST)
	  CALL IOINIT(BFDB,2,32*256)
	  IF(ST.NE.0) THEN
	     CALL OPS('Error opening secondary TMF file',ST,ST)
	     BTM_BLK_CNT = MAXINT    ! LET LOGGER DEAL WITH ERRORS
	  ENDIF
	  CALL VAXGETFSIZ(2,SFSIZES(BTMF))
	ENDIF
C
	DISKSW_SAV = P(DISKSW)
C
10	CONTINUE
C
	IF(TMFMON_OK_TO_DIE.EQ.1) GOTO 100
        IF(DAYSTS.EQ.DSCLOS) GOTO 100
C
	IF(DISKSW_SAV.NE.P(DISKSW)) THEN
	   CALL CLOSEFIL(MFDB)
	   CALL CLOSEFIL(BFDB)
	   GOTO 5
	ENDIF
C
	CALL XWAIT(100,1,ST)
	SFSIZES(PTMF) = MTM_BLK_CNT * 16
	SFSIZES(BTMF) = BTM_BLK_CNT * 16
C
C EXTEND primary TM file.
C
	IF(HBLOCK + FREE_BLOCK_CNT .GT. MTM_BLK_CNT) THEN
	   CALL OPS('Extending primary TMF file',EXTEND_BLKS,HBLOCK)
	   CALL EXTEND(1, EXTEND_BLKS*16, ST)
           IF(.NOT.ST) THEN
                CALL OPS('Failed to extend primary TMF',ST,ST)
		MTM_BLK_CNT = MAXINT ! LET LOGGER DEAL WITH ERRORS
           ELSE
                CALL OPSTXT('Clearing primary TMF extension')
                DO I=1,EXTEND_BLKS
                   CALL WRITEW(MFDB,MTM_BLK_CNT+1,ZEROS,ST)
		   IF(ST.NE.0) THEN
		      MTM_BLK_CNT = MAXINT ! LET LOGGER DEAL WITH ERRORS
		      GOTO 20
		   ENDIF
	           MTM_BLK_CNT = MTM_BLK_CNT + 1
                ENDDO
                CALL OPSTXT('Primary TMF extension cleared')
           ENDIF
	 ENDIF
20	 CONTINUE
	 IF(P(DISKSW).EQ.0) GOTO 10
C
C EXTEND BACKUP TM file.
C
	IF(HBLOCK + FREE_BLOCK_CNT .GT. BTM_BLK_CNT) THEN
	   CALL OPS('Extending backup TMF file',EXTEND_BLKS,HBLOCK)
	   CALL EXTEND(2, EXTEND_BLKS*16, ST)
           IF(.NOT.ST) THEN
                CALL OPS('Failed to extend backup TMF',ST,ST)
		BTM_BLK_CNT = MAXINT ! LET LOGGER DEAL WITH ERRORS
           ELSE
                CALL OPSTXT('Clearing backup TMF extension')
                DO I=1,EXTEND_BLKS
                   CALL WRITEW(BFDB,BTM_BLK_CNT+1,ZEROS,ST)
                   IF(ST.NE.0) THEN
                      BTM_BLK_CNT = MAXINT ! LET LOGGER DEAL WITH ERRORS
                      GOTO 40
                   ENDIF
	           BTM_BLK_CNT = BTM_BLK_CNT + 1
                ENDDO
                CALL OPSTXT('Backup TMF extension cleared')
           ENDIF
	 ENDIF
C
40	 CONTINUE
C
	 GOTO 10
C
100	 CONTINUE
C
	 CALL CLOSEFIL(MFDB)
	 CALL CLOSEFIL(BFDB)
	 CALL GSTOP(GEXIT_SUCCESS)
	 END	
