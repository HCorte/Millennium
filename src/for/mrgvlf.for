C
C *** PROGRAM MRGVLF ***
C *** Pre-Baseline Source Module - mrgvlf.for ***
C
C V12 02-MAR-2000 RXK 114 in LODREC,LODBUF size for VLW (used to be 113)
C V11 12-Mar-1997 RXK At end VLW closed 
C V10 30-Jan-1994 HXK VLF is NOT copied to VLC (this is now done by VLF2VLC 
C                 task) Use double buffered read to increase speed.
C V09 31-Aug-1993 JJ increased memory buffer size.
C V08 27-Aug-1993 JJ DOUBLE MEMORY BUFFER
C V07 17-Aug-1993 CONSOLE FIXED CALL TO IGTBLK
C V06 16-Aug-1993 MJD IMPLIMENTED WALTERS FAST MERGE FROM SPAIN.
C V05 20-May-1993 PJS Modified the "check-in-messages keyword" to provide 
C                     fixed-length expansion.
C V04 17-May-1993 DAB Initial revision.
C V03 23-APR-1992 TKO Do not check for multiple of 200
C V02 03-MAR-1992 TKO Use parameters for disk sizes
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
C
	PROGRAM MRGVLF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:PRMVAL.DEF'
	INCLUDE 'INCLIB:PRMVLF.DEF'
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4  VSIZE, TUBSIZ
        INTEGER*4  I4VLWSIZ
        PARAMETER (I4VLWSIZ=8192)
	PARAMETER (VSIZE=8000000)
	PARAMETER (TUBSIZ=I4BUCSIZ*7)
C
	INTEGER*4 FDB(7),OFDB(7),NFDB(7),BIGVLF(VSIZE)
        INTEGER*4 LODBUF(72,114,2)
	INTEGER*4 NEWBUF(TUBSIZ)
	INTEGER*4 SCFNAM(2),COUNT,BLOCK,CIND,NIND
	INTEGER*4 NEWSIZ, OLDSIZ,I,K,ST
	DATA SCFNAM/'SCF.','FIL '/
C
	INTEGER*4 LODREC(72*114,2)
	
C	INTEGER*4 COPBUF(12800),STWINCNT,SECTORS

        EQUIVALENCE(LODBUF(1,1,1),LODREC(1,1))
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET


C
C
C OPEN SCF
C
	CALL OPENW(1,SCFNAM,4,0,0,ST)
	CALL IOINIT(FDB,1,SCFSEC*256)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),'SCF.FIL open error > ',ST
	  CALL GPAUSE
	ENDIF
	CALL READW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),'SCF.FIL read error > ',ST
	  CALL GPAUSE
	ENDIF
	CALL CLOSEFIL(FDB)

C
C
C OPEN VLF FILE AND VLC FILE
C
	CALL OPENW(VLF,SCFSFN(1,VLF),4,0,0,ST)
	CALL IOINIT(OFDB,VLF,200*256)
	IF(ST.NE.0) THEN
	  WRITE(5,900) IAM(),(SCFSFN(K,VLF),K=1,5),ST
	  CALL GPAUSE
	ENDIF
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
	CALL GETSIZ_USED(VLF,OLDSIZ)
	CALL GETSIZ_USED(VLC,NEWSIZ)
	IF(OLDSIZ.NE.NEWSIZ) THEN
	  TYPE*,IAM(),'File size of VLF copy must be the same as VLF'
	  TYPE*,IAM(),'Correct file size and rerun MRGVLF'
	  TYPE*,IAM(),'        '
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	CALL CLOSEFIL(NFDB)
	CALL CLOSEFIL(OFDB)
C

C
	CALL IOPEN(SCFSFN(1,VLC),VLC,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) THEN
	  WRITE(5,904) IAM(),(SCFSFN(K,VLC),K=1,5),ST
	  CALL GPAUSE
	ENDIF
	CALL IINIB(VLC,BIGVLF,VSIZE)
	CALL ITUBSIZE(VLC,TUBSIZ)
C
C
	CALL OPENW(VLW,SCFSFN(1,VLW),4,0,0,ST)
	CALL IOINIT(NFDB,VLW,I4VLWSIZ*4)
	IF(ST.NE.0) THEN
	  WRITE(5,900) IAM(),(SCFSFN(K,VLW),K=1,5),ST
	  CALL GPAUSE
	ENDIF
C
C
        COUNT=0
C
C START FILE SCANS
C AND DETERMINE LOW AND HIGH BUCKET FOR THIS SCAN.
C
	  BLOCK=1
	  CIND=1
	  NIND=2
C
C
40	  CONTINUE
	  IF(BLOCK.EQ.1) THEN
	    CALL READW(NFDB,BLOCK,LODREC(1,CIND),ST)
	    IF(ST.NE.0) THEN
	      WRITE(5,902) IAM(),(SCFSFN(K,VLW),K=1,5),ST,BLOCK
	      CALL GPAUSE
	    ENDIF
	    BLOCK=BLOCK+1
	    CALL READW(NFDB,BLOCK,LODREC(1,NIND),ST)
	    IF(ST.NE.0) THEN
              WRITE(5,902) IAM(),(SCFSFN(K,VLW),K=1,5),ST,BLOCK
              CALL GPAUSE
            ENDIF
	  ELSE
	    CALL WAITFORREAD(NFDB,ST)
            IF(ST.NE.0) THEN
              WRITE(5,902) IAM(),(SCFSFN(K,VLW),K=1,5),ST,BLOCK
              CALL GPAUSE
            ENDIF
	    BLOCK=BLOCK+1
	    CIND=NIND
	    NIND=MOD(CIND,2)+1
	    CALL READNW(NFDB,BLOCK,LODREC(1,NIND),ST)
            IF(ST.NE.0) THEN
              WRITE(5,902) IAM(),(SCFSFN(K,VLW),K=1,5),ST,BLOCK
              CALL GPAUSE
            ENDIF
	  ENDIF
C
C
          DO 100 I=1,113
          IF(LODBUF(VFSSER,I,CIND).EQ.0) GOTO 200
          COUNT=COUNT+1
          IF(MOD(COUNT,50000).EQ.0) TYPE*,IAM(),COUNT,' records loaded'
          CALL IWRIBF(LODBUF(1,I,CIND),VLC,BIGVLF,NEWBUF,ST)
          IF(ST.NE.0) THEN
             WRITE(5,905) IAM(),(SCFSFN(K,VLC),K=1,5),ST
             CALL GPAUSE
          ENDIF
100       CONTINUE
          GOTO 40
C
C
C
200	  CONTINUE
C
C
	WRITE(5,906) IAM(),COUNT,(SCFSFN(K,VLW),K=1,5)
	CALL ICLOSB(VLC,BIGVLF,NEWBUF,ST)
	IF(ST.NE.0) THEN
	  WRITE(5,907) IAM(),(SCFSFN(K,VLC),K=1,5),ST
	  CALL GPAUSE
	ENDIF
C
	CALL CLOSEFIL(NFDB)
	TYPE*,IAM(),'VLF merge complete'
	CALL GSTOP(GEXIT_SUCCESS)
C
900	FORMAT(1X,A,1X,5A4,' open error > ',I4)
901	FORMAT(1X,A,1X,'Copying ',5A4,' to ',5A4)
902	FORMAT(1X,A,1X,5A4,' read error > ',I4,' Block > ',I5)
903	FORMAT(1X,A,1X,5A4,' write error > ',I4,' Block > ',I5)
904	FORMAT(1X,A,1X,5A4,' Iopen error > ',I4)
905	FORMAT(1X,A,1X,5A4,' Iwrite error > ',I4)
906	FORMAT(1X,A,1X,I7,' records copied from ',5A4)
907	FORMAT(1X,A,1X,5A4,' Iclosb error > ',I4)
	END
