C MLMRGVLF.FOR
C
C V06 14-FEB-2000 UXN Size for LODBUF fixed. 
C                     ( LODREC(72*113,2) changed to LODREC(72*114,2) )
C V05 16-DEC-1999 OXK Removed unnecessary question 'make another copy'
C V04 28-JUN-1999 UXN Make sure VLF->VLC copy is done.
C V03 25-MAY-1999 RXK VLF->VLC copy separated into MLCOPVLF.
C V02 27-APR-1999 RXK Started and FMAINT done in MULTIVLF.  
C V01 11-JAN-1999 GLS INITIAL RELEASE FOR FINLAND (FROM MRGVLF.FOR)
C
C MULTI-FILE VLF MERGE PROGRAM
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
C
        PROGRAM MLMRGVLF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:PRMVAL.DEF'
        INCLUDE 'INCLIB:PRMVLF.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:MULNAM.DEF'
C
        INTEGER*4  VSIZE, TUBSIZ
        INTEGER*4  I4VLWSIZ
        PARAMETER (I4VLWSIZ=8192)
        PARAMETER (VSIZE=8000000)
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
C
        INTEGER*4 FDB(7),NFDB(7),BIGVLF(VSIZE)
        INTEGER*4 NEWBUF(TUBSIZ)
        INTEGER*4 COUNT,BLOCK,CIND,NIND
        INTEGER*4 NEWSIZ, OLDSIZ,I,K,STWINCNT,ST
        INTEGER*4 SECTORS
        INTEGER*4 TSK, TSKNUM, ITSK
        CHARACTER * 8 TNAME          
        INTEGER*4 INDTSK                     !FUNCTION
        INTEGER*4 DUM,FLAG,STATUS
C
        INTEGER*4 LODREC(72*114,2)
        INTEGER*4 LODBUF(72,114,2)
        EQUIVALENCE(LODBUF(1,1,1),LODREC(1,1))
C
        CALL COPYRITE
        CALL SNIF_AND_WRKSET
C
C OPEN SCF
C
	CALL GETSCONF(SCFREC,ST)
        IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)

C
C MAKE VLF->VLC COPY
C	
	COUNT = 0
10	CONTINUE
        IF(VLCSTS.NE.WCOP) THEN
	    CALL XWAIT(2,2,ST)
	    COUNT = COUNT + 1
	    IF(MOD(COUNT,60).EQ.1) 
     *           TYPE*,IAM(),'Waiting for completion of VLF->VLC'
            GOTO 10
	ENDIF

        CALL IOPEN(SCFSFN(1,VLC),VLC,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST.NE.0) THEN
          WRITE(6,904) IAM(),(SCFSFN(K,VLC),K=1,5),ST
          CALL GPAUSE
        ENDIF
        CALL IINIB(VLC,BIGVLF,VSIZE)
        CALL ITUBSIZE(VLC,TUBSIZ)
C
C MAIN LOOP 
C
        DO 9999 ITSK=1,WINCNT
C
          TSKNUM=WINWTSK(ITSK)
          IF(TSKNUM.LT.1.OR.TSKNUM.GT.MAXWTSK) WRITE(6,908) IAM(),TSKNUM
C
          TNAME = TSKWNAM(TSKNUM)
          TSK = INDTSK(TNAME)
          IF(TSK.LT.1.OR.TSK.GT.MAXWTSK) WRITE(6,909) IAM(),TNAME,TSK
C
          CALL OPENW(VLW,VLWNAM(1,TSK),4,0,0,ST)
          CALL IOINIT(NFDB,VLW,I4VLWSIZ*4)
          IF(ST.NE.0) THEN
            WRITE(6,900) IAM(),(VLWNAM(K,TSK),K=1,5),ST
            CALL GPAUSE
          ENDIF
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
40        CONTINUE
          IF(BLOCK.EQ.1) THEN
            CALL READW(NFDB,BLOCK,LODREC(1,CIND),ST)
            IF(ST.NE.0) THEN
              WRITE(6,902) IAM(),(VLWNAM(K,TSK),K=1,5),ST,BLOCK
              CALL GPAUSE
            ENDIF
            BLOCK=BLOCK+1
            CALL READW(NFDB,BLOCK,LODREC(1,NIND),ST)
            IF(ST.NE.0) THEN
              WRITE(6,902) IAM(),(VLWNAM(K,TSK),K=1,5),ST,BLOCK
              CALL GPAUSE
            ENDIF
          ELSE
            CALL WAITFORREAD(NFDB,ST)
            IF(ST.NE.0) THEN
              WRITE(6,902) IAM(),(VLWNAM(K,TSK),K=1,5),ST,BLOCK
              CALL GPAUSE
            ENDIF
            BLOCK=BLOCK+1
            CIND=NIND
            NIND=MOD(CIND,2)+1
            CALL READNW(NFDB,BLOCK,LODREC(1,NIND),ST)
            IF(ST.NE.0) THEN
              WRITE(6,902) IAM(),(VLWNAM(K,TSK),K=1,5),ST,BLOCK
              CALL GPAUSE
            ENDIF
          ENDIF
C
          DO 100 I=1,113
          IF(LODBUF(VFSSER,I,CIND).EQ.0) GOTO 200
          COUNT=COUNT+1
          IF(MOD(COUNT,50000).EQ.0) TYPE*,IAM(),COUNT,' records loaded'
          CALL IWRIBF(LODBUF(1,I,CIND),VLC,BIGVLF,NEWBUF,ST)
          IF(ST.NE.0) THEN
             WRITE(6,905) IAM(),(SCFSFN(K,VLC),K=1,5),ST
             CALL GPAUSE
          ENDIF
100       CONTINUE
          GOTO 40
C
200       CONTINUE
C
          WRITE(6,906) IAM(),TSKWNAM(TSKNUM),
     *                       COUNT,(VLWNAM(K,TSK),K=1,5)
          CALL XWAIT(2,2,ST)                             !WAIT
          CALL CLOSEFIL(NFDB)
C
          STSTSK(TSKNUM)=MRGDON
C
9999    CONTINUE
C

        CALL ICLOSB(VLC,BIGVLF,NEWBUF,ST)
        IF(ST.NE.0) THEN
          WRITE(6,907) IAM(),(SCFSFN(K,VLC),K=1,5),ST
          CALL GPAUSE
        ENDIF
C
        CALL GSTOP(GEXIT_SUCCESS)
C
900     FORMAT(1X,A,1X,5A4,' open error > ',I4)
902     FORMAT(1X,A,1X,5A4,' read error > ',I4,' Block > ',I5)
904     FORMAT(1X,A,1X,5A4,' Iopen error > ',I4)
905     FORMAT(1X,A,1X,5A4,' Iwrite error > ',I4)
906     FORMAT(/1X,A,1X,A8,' > ',I7,' records copied from ',5A4/)
907     FORMAT(1X,A,1X,5A4,' Iclosb error > ',I4)
908     FORMAT(1X,A,1X,'Wrong Winsel Task Number >',I4)
909     FORMAT(1X,A,1X,'Wrong Winsel Task Name >',A8,' for task ',I4)
910	FORMAT(1X,A,'Initialize ',5A4, ' with WINCLR and rerun MLCOPVLF')
        END
