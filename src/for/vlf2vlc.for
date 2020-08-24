C
C PROGRAM VLF2VLC
C $Log:   GXAFXT:[GOLS]VLF2VLC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:54:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   30 Jan 1994 22:14:18   HXK
C  Initial revision.
C  
C     Rev 1.0   21 Jan 1993 17:02:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - VLF2VLC.for **
C
C VLF2VLC.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C V02 12-NOV-91 MTK INITIAL RELEASE FOR NETHERLANDS
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM VLF2VLC
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:PRMVAL.DEF'
	INCLUDE 'INCLIB:PRMVLF.DEF'
	INCLUDE 'INCLIB:HSHCOM.DEF'

	INTEGER*4  TUBSIZ

	PARAMETER (TUBSIZ=I4BUCSIZ*7)
C
	INTEGER*4 OFDB(7),NFDB(7)
	INTEGER*4 COPBUF(12800)
	INTEGER*4 BLOCK
	INTEGER*4 NEWSIZ, OLDSIZ, K, ST
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C
C COPY VLF TO VLC USING BIG BLOCK I/0
C
	CALL GETSCONF(SCFREC,ST)
C
C
C
C CREATE NEW FILES
C
        IF(SCFSFN(1,VLF).NE.SCFSFN(1,VLC)) THEN
          TYPE*,IAM(),'Validation copy file must have same volume as vlf'
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        IF(SCFFSZ(VLC).NE.SCFFSZ(VLF)) THEN
          TYPE*,IAM(),'Validation copy file must be same size as vlf'
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        CALL CRTFIL(SCFSFN(1,VLC),SCFFSZ(VLC),ST)
        IF(ST.NE.0) THEN
          WRITE(5,904) (SCFSFN(K,VLC),K=1,5)
          CALL GPAUSE
        ENDIF
C
C
	CALL OPENW(VLF,SCFSFN(1,VLF),4,0,0,ST)
	CALL IOINIT(OFDB,VLF,200*256)
	IF(ST.NE.0) THEN
	  WRITE(5,900) IAM(),(SCFSFN(K,VLF),K=1,5),ST
	  CALL GPAUSE
	ENDIF
C
C
	CALL OPENW(VLC,SCFSFN(1,VLC),4,0,0,ST)
	CALL IOINIT(NFDB,VLC,200*256)
	IF(ST.NE.0) THEN
	  WRITE(5,900) IAM(),(SCFSFN(K,VLC),K=1,5),ST
	  CALL GPAUSE
	ENDIF
C
C CHECK SIZE OF OLD AND NEW FILES
C
	CALL GETSIZ(VLF,OLDSIZ)
	CALL GETSIZ(VLC,NEWSIZ)
	IF(OLDSIZ.NE.NEWSIZ) THEN
	  TYPE*,IAM(),'File size of VLF copy must be the same as VLF'
	  TYPE*,IAM(),'Correct file size and rerun VLF2VLC'
	  TYPE*,IAM(),'        '
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C
	WRITE(5,901) IAM(),(SCFSFN(K,VLF),K=1,5),(SCFSFN(K,VLC),K=1,5)
	BLOCK=0
10	CONTINUE
	BLOCK=BLOCK+1
	CALL READW(OFDB,BLOCK,COPBUF,ST)
	IF(ST.NE.0.AND.ST.NE.144) THEN
	  WRITE(5,902) IAM(),(SCFSFN(K,VLF),K=1,5),ST,BLOCK
	  CALL GPAUSE
	ENDIF
	CALL WRITEW(NFDB,BLOCK,COPBUF,ST)
	IF(ST.EQ.144) GOTO 15
	IF(ST.NE.0) THEN
	  WRITE(5,903) IAM(),(SCFSFN(K,VLC),K=1,5),ST,BLOCK
	  CALL GPAUSE
	ENDIF
	GOTO 10
15	CONTINUE
	CALL CLOSEFIL(NFDB)
	CALL CLOSEFIL(OFDB)
	TYPE*,IAM(),'VLF -> VLC file copy completed'

	CALL GSTOP(GEXIT_SUCCESS)

900	FORMAT(1X,A,1X,5A4,' open error > ',I4)
901	FORMAT(1X,A,1X,'Copying ',5A4,' to ',5A4)
902	FORMAT(1X,A,1X,5A4,' read error > ',I4,' Block > ',I5)
903	FORMAT(1X,A,1X,5A4,' write error > ',I4,' Block > ',I5)
904	FORMAT(1X,A,1X,5A4,' Iopen error > ',I4)
905	FORMAT(1X,A,1X,5A4,' Iwrite error > ',I4)
906	FORMAT(1X,A,1X,I6,' records copied from ',5A4)
907	FORMAT(1X,A,1X,5A4,' Iclosb error > ',I4)
	END
