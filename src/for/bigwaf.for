C
C PROGRAM BIGWAF
C
C PROGRAM TO GENERATE THE ASCII FILE OF BIG WINNERS
C
C V02 09-JUN-1999 UXN Cartel number added to selling agent.
C V01 18-FEB-1999 RXK Initial release.
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM BIGWAF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'

        INTEGER*4  CDC  
        INTEGER*4  NUM
        INTEGER*4  TOTAMT
        INTEGER*4  KGIND,KGTYP
        INTEGER*4  I,J,K
        INTEGER*4  ST,CARTEL
        INTEGER*4  TIME,HOURS,MINS,SECS
        INTEGER*4  AGTFLAGS(NUMAGT)                 !List of selling agents
        INTEGER*4  CAGTFLAGS(NUMAGT)                !List of cashing agents
        INTEGER*4  FDB(7)
 
        INTEGER*4  MAXCNT                           !Dim.for sort table
        PARAMETER  (MAXCNT=100000)
        INTEGER*4  STAB(8,MAXCNT)                   !Sort table

        CHARACTER  AGTSTUFF*100                      !Agent inf for output
        INTEGER*4  IAGTSTUFF(25)
        EQUIVALENCE(AGTSTUFF,IAGTSTUFF)

        CHARACTER  CAGTSTUFF*8                      !Cashing agent # for output
        INTEGER*4  ICAGTSTUFF(2)
        EQUIVALENCE(CAGTSTUFF,ICAGTSTUFF)

        INTEGER*4  BINAGT(24,NUMAGT)
        INTEGER*4  CBINAGT(2,NUMAGT)

        INTEGER*4  CRIT                             !"big" amount value (units)
        PARAMETER  (CRIT=1000*DOLL_BASE/DYN_VALUNIT)!is 1000 mk
        INTEGER*4  TUBSIZ     
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
        INTEGER*4  VLFBUF(TUBSIZ) 
C
        INTEGER*2  DATE(LDATE_LEN)

        CHARACTER   CZERO             
        DATA CZERO/Z0/

        CALL COPYRITE
C
        CDC = DAYCDC
        IF(DAYSTS.NE.DSCLOS) THEN
            TYPE*,IAM(),' Invalid daysts > ',DAYSTS
            CALL GPAUSE
        ENDIF
C
        DATE(5)=CDC
        CALL LCDATE(DATE)
        WRITE(5,900) IAM(),(DATE(K),K=7,13)

        NUM = 0
        TOTAMT = 0
        CALL FASTSET(0,STAB,8*MAXCNT)
        CALL FASTSET(0,AGTFLAGS,NUMAGT)
        CALL FASTSET(0,CAGTFLAGS,NUMAGT)
C
C OPEN VALIDATION FILE FOR SEQUENTIAL READ
C
        CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),1,ST,0)
        CALL ITUBSIZE(VLF,TUBSIZ)
C
C SCAN VLF FILE FOR TODAYS VALIDATIONS
C
100     CONTINUE
        CALL ISREAD(V4BUF,VLF,VLFBUF,ST)
        IF(ST.EQ.ERREND) THEN
            CALL ICLOSE(VLF,VLFBUF,ST)
            GOTO 1000
        ENDIF
C
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),2,ST,0)
C
        CALL LOGVAL(VALREC,V4BUF)
        IF(VALREC(VCCDC).NE.CDC) GOTO 100
        IF(VALREC(VSTAT).NE.VCASH .AND. VALREC(VSTAT).NE.VCASHX .AND.
     *         VALREC(VSTAT).NE.VBANK) GOTO 100
        IF((VALREC(VPAMT)+VALREC(VKPAMT)).LT.CRIT) GOTO 100         
C
C FILL MEMORY TABLE
C
        NUM = NUM + 1 
        IF(NUM.GT.MAXCNT) THEN
           TYPE*,IAM(),' Memory sort table overflow '
           CALL GPAUSE
           TYPE*,IAM(),' Output FTP-file will not contain all records'
           GOTO 1000
        ENDIF
C
        TOTAMT = TOTAMT + VALREC(VPAMT) + VALREC(VKPAMT)
        STAB(1,NUM) = VALREC(VGTYP)
        STAB(2,NUM) = VALREC(VGIND)
        STAB(3,NUM) = VALREC(VPAMT)
        STAB(4,NUM) = VALREC(VKGME)
        STAB(5,NUM) = VALREC(VKPAMT)
        STAB(6,NUM) = VALREC(VSTER)
        STAB(7,NUM) = VALREC(VCTER)           ! =0 if bankwinner 
        IF(VALREC(VSTAT).EQ.VCASH.OR.VALREC(VSTAT).EQ.VCASHX) 
     *     STAB(8,NUM) = 1
        IF(VALREC(VSTAT).EQ.VBANK) STAB(8,NUM) = 2
        AGTFLAGS(VALREC(VSTER)) = 1
        IF(VALREC(VCTER).GT.0) CAGTFLAGS(VALREC(VCTER)) = 1
C
        GOTO 100
C
1000    CONTINUE
        CALL ICLOSE(VLF,VLFBUF,ST)
        IF(NUM.EQ.0) THEN
           TYPE*,IAM(),' No big winners today'
           GOTO 2000
        ENDIF
C
C SORT MEMORY TABLE
C
        CALL ISORTB(STAB,NUM,3,8)
C
C GET AGENT INFORMATION
C
        CALL OPENW(2,SFNAMES(1,ASF),4,0,0,ST)
        CALL IOINIT(FDB,2,ASFSEC*256)
        IF(ST.NE.0) THEN
           CALL FILERR(SFNAMES(1,ASF),1,ST,0)
           CALL CLOSEFIL(FDB)
           CALL GSTOP(GEXIT_FATAL)
        ENDIF

        DO K=1,NUMAGT
           IF(AGTFLAGS(K).GT.0.OR.CAGTFLAGS(K).GT.0) THEN
              CALL READW(FDB,K,ASFREC,ST)
              IF(ST.NE.0) THEN
                 CALL FILERR(SFNAMES(1,ASF),2,ST,K)
                 CALL CLOSEFIL(FDB)
                 CALL GSTOP(GEXIT_FATAL)
              ENDIF
	      CALL ASCBIN(ASFINF,SCHAN,LCHAN,CARTEL,ST)
              WRITE(AGTSTUFF(1:4),915) CARTEL
              WRITE(AGTSTUFF(6:12),911) (ASFBYT(J),J=SAGNO,EAGNO)  
              WRITE(AGTSTUFF(14:40),912) (ASFBYT(J),J=SNAME,ENAME)  
              WRITE(AGTSTUFF(42:68),912) (ASFBYT(J),J=SSTRT,SSTRT+26)
              WRITE(AGTSTUFF(70:74),913) (ASFBYT(J),J=SZIPC,SZIPC+4)
              WRITE(AGTSTUFF(76:100),914) (ASFBYT(J),J=SCITY,ECITY)
              DO I=1,100
                 IF(AGTSTUFF(I:I).EQ.CZERO) AGTSTUFF(I:I)=' '
              END DO
              CALL FASTMOV(IAGTSTUFF(1),BINAGT(1,K),25)
           ENDIF
           IF(CAGTFLAGS(K).GT.0) THEN
              WRITE(CAGTSTUFF(2:8),911) (ASFBYT(J),J=SAGNO,EAGNO)  
              DO I=1,8
                 IF(CAGTSTUFF(I:I).EQ.CZERO) CAGTSTUFF(I:I)=' '
              END DO
              CALL FASTMOV(ICAGTSTUFF(1),CBINAGT(1,K),2)
           ENDIF
        ENDDO 
        CALL CLOSEFIL(FDB)
C
C OPEN OUTPUT FTP FILE
C
2000    CONTINUE
        OPEN (UNIT=10, FILE='FILE:BIGWIN.FTP', IOSTAT=ST,
     *       STATUS='NEW', DISP='KEEP', ACCESS='SEQUENTIAL',
     *       FORM='FORMATTED', RECORDTYPE='VARIABLE',
     *       RECL=160, CARRIAGECONTROL='LIST')
        IF(ST.NE.0) THEN
           TYPE*,IAM(),' FILE:BIGWIN.FTP Open error  st - ',ST
           CALL USRCLOS1(10)
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C WRITE HEADER
C
        CALL GETTIM(TIME)
        HOURS = TIME/3600
        MINS = (TIME-HOURS*3600) / 60
        SECS = TIME - HOURS*3600 - MINS*60

        WRITE(10,9001) 100,DATE(VYEAR2),DATE(VMON),DATE(VDAY),
     *                 HOURS,MINS,SECS 
C
C WRITE DATA RECORDS
C 
        DO I=NUM,1,-1
           IF(STAB(4,I).GT.0) THEN
              KGTYP = GNTTAB(GAMTYP,STAB(4,I))
              KGIND = GNTTAB(GAMIDX,STAB(4,I))
           ELSE
              KGTYP = 0
              KGIND = 0 
           ENDIF
           CALL FASTMOV(BINAGT(1,STAB(6,I)),IAGTSTUFF,24)
           IF(STAB(7,I).GT.0) THEN
              CALL FASTMOV(CBINAGT(1,STAB(7,I)),ICAGTSTUFF,2)
              WRITE(10,9002) 101,
     *           STAB(1,I),STAB(2,I),CMONY(STAB(3,I),15,VALUNIT),
     *           KGTYP,KGIND,CMONY(STAB(5,I),15,VALUNIT),
     *           AGTSTUFF,CAGTSTUFF,STAB(8,I)
           ELSE
              WRITE(10,90021) 101,
     *           STAB(1,I),STAB(2,I),CMONY(STAB(3,I),15,VALUNIT),
     *           KGTYP,KGIND,CMONY(STAB(5,I),15,VALUNIT),
     *           AGTSTUFF,STAB(8,I)
           ENDIF
        ENDDO
C
C WRITE END RECORD
C
        WRITE(10,9003) 109,NUM,CMONY(TOTAMT,15,VALUNIT)
	CLOSE(10)
        WRITE(5,901) IAM(),(DATE(K),K=7,13)
        CALL GSTOP(GEXIT_SUCCESS)

900     FORMAT(1X,A,' Creation of big winners FTP-file for ',7A2,' started')
901     FORMAT(1X,A,' Big winners FTP-file creation for ',7A2,' completed')
911     FORMAT(7A1)
912     FORMAT(27A1)
913     FORMAT(5A1)
914     FORMAT(18A1)
915     FORMAT(I4)
9001    FORMAT(1X,I3,2X,I4,2(I2.2),1X,3(I2.2))
9002    FORMAT(1X,I3,I4,I4,A15,I4,I4,A15,A100,A8,I2)
90021   FORMAT(1X,I3,I4,I4,A15,I4,I4,A15,A100,' 0000000',I2)
9003    FORMAT(1X,I3,I8,A15)

        END
