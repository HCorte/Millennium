C SUBROUTINE COPVLF
C  
C V05 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V04 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V03 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V02 07-OCT-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE COPVLF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
C
	INTEGER*4  TUBSIZ
	PARAMETER (TUBSIZ=I4BUCSIZ*7)
	INTEGER*4 CNTWRIT, CNTREAD, CNTUPD, ST
	INTEGER*4 VLFBUF(TUBSIZ),NEWBUF(TUBSIZ)
	BYTE V1BUF(VFLEN*4*4)
	EQUIVALENCE (V1BUF,V4BUF)
C
C OPEN VALIDATION FILES
C
	TYPE*,IAM(),' Copying Validation file'
	CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),1,ST,0)
	CALL ITUBSIZE(VLF,TUBSIZ)
C
C
	CALL IOPEN(SFNAMES(1,VLC),VLC,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLC),1,ST,0)
	CALL ITUBSIZE(VLC,TUBSIZ)
C
C SCAN VALIDATION FILE
C
	CNTUPD=0
	CNTREAD=0
	CNTWRIT=0
100	CONTINUE
	CALL ISREAD(V4BUF,VLF,VLFBUF,ST)
	IF(ST.EQ.ERREND) THEN
	  CALL ICLOSE(VLF,VLFBUF,ST)
	  IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),4,ST,0)
	  CALL ICLOSE(VLC,NEWBUF,ST)
	  IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLC),4,ST,0)
	  TYPE*,IAM(),' Number of tickets read    = ',CNTREAD
	  TYPE*,IAM(),' Number of tickets written = ',CNTWRIT
	  TYPE*,IAM(),' Validation file copy complete'
	  RETURN
	ENDIF
	CNTREAD=CNTREAD+1
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),2,ST,0)
C
C
	CALL ISWRIT(V4BUF,VLC,NEWBUF,ST)
	CNTWRIT=CNTWRIT+1
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLC),3,ST,0)
	GOTO 100
	END
