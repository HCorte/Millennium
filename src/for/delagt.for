C PROGRAM DELAGT
C
C DELAGT.FOR
C
C V10 15-MAR-2011 GPW NUMAGT=12288
C V09 17-MAR-1999 RXK Added check of the due in IPS invoice.
C V08 29-JAN-1999 RXK End of file checking added.
C V07 28-MAR-1995 HXK Fix for disabling wagering on deleted agent slot
C V06 23-MAR-1995 PXB Disabled wagering on deleted agents terminal.
C V05 09-MAR-1994 JXP Changed name of report to DELAGT.REP
C V04 11-JAN-1994 JXP Added the report option and reorganised the .def's
C V03 05-JAN-1994 JXP Changed declaration order of .def's
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C
C DELETED AGENT PROGRAM.
C REMOVE AGENT FROM LIVE AGENT SALES FILE AND PUT IN DELETED
C AGENTS FILE (DASF.FIL).
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM DELAGT
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
C COPYRITE.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++
C CONFIDENTIAL PROPRIETARY INFORMATION                          V01
C
C This item is the property of GTECH Corporation, West Greenwich,
C Rhode Island, and contains confiential and trade secret
C information.  It may not be transferred from the custody or control
C of GTECH except as authorized in writing by an officer of GTECH.
C Neither this item nor the information it contains may be used,
C transferred, reproduced, published, or disclosed, in whole or in
C part, and directly or indirectly, except as expressly authorized by
C an officer of GTECH, pursuant to written agreement.
C
C
C COPYRITE.DEF-----------------------------------------------------
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMLVL.DEF'
        INCLUDE 'INCLIB:RECUSE.DEF'
C

        INTEGER*4 REC, I, J, EXT, AGT
        INTEGER*4 DFDB(7),AFDB(7),BUF(ASFSEC*64)
        INTEGER*4 ST,ANUM,FUNNUM,MAXFUN
C       INTEGER*4 UID,SECLEV,SIND
        INTEGER*2 DDAT(12)

        INTEGER*4 LINCNT, PAGE, COPY, K, WORK, ALLTRA

        CHARACTER*3 FUN
        PARAMETER(MAXFUN=4)           
        CHARACTER*3 FUNABV(MAXFUN)   
        CHARACTER*6 CCDTE
        CHARACTER CZERO,LOTNUM(LAGNO),CDTE(6)
        EQUIVALENCE (CDTE,CCDTE)


        DATA LINCNT/70/,PAGE/0/
        DATA CZERO/Z0/
        DATA FUNABV/'DEL','RES','REP','EXT'/
C
C
5        CONTINUE
         CALL CLRSCR(5)
C        CALL SGNON(DELIND,SECLEV,UID,SIND,ST)
C        CALL USRCLOS1(     2)
C        IF (ST.EQ.1) THEN
C          TYPE*,IAM(),' USER file does not exist '
C          CALL GSTOP(GEXIT_FATAL)
C        ENDIF
C        IF (SECLEV.NE.SUPERUSE) THEN
C         TYPE*,IAM(),' Security level does not authorize access'
C         CALL XWAIT(2,2,ST)
C         GOTO 5
C        ENDIF
C
C OPEN AGENT SALES FILE AND DELETED AGENTS FILE
C
        CALL CLRSCR(5)
        CALL OPENW(1,SFNAMES(1,ASF),0,0,0,ST)
        CALL IOINIT(AFDB,1,ASFSEC*256)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,ASF),1,ST,0)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C
        CALL OPENW(2,SFNAMES(1,DAG),0,0,0,ST)
        CALL IOINIT(DFDB,2,ASFSEC*256)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,DAG),1,ST,0)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C PRINT MENU AND GET FUNCTION
C
10      CONTINUE
        CALL CLRSCR(5)
        WRITE(5,900) IAM()
        CALL WIMG(5,'Enter function ')
        READ (5,901) FUN
C
C GET FUNCTION NUMBER
C
      DO 510 I=1,MAXFUN
         IF(FUN.EQ.FUNABV(I)) FUNNUM=I
510   CONTINUE
C
C
        IF(FUN.EQ.'DEL') GOTO 1000
        IF(FUN.EQ.'RES') GOTO 2000
        IF(FUN.EQ.'REP') GOTO 3000
        IF(FUN.EQ.'EXT') GOTO 5000
C
C INVALID FUNCTION
C
        CALL CLRSCR(5)
        WRITE(5,902) IAM()
        CALL XWAIT(2,2,ST)
        GOTO 10
C
C DELETE ACTIVE AGENT
C
1000    CONTINUE
        CALL CLRSCR(5)
        CALL INPNUM('Enter terminal number to be deleted [E-Exit]',
     *              AGT,1,NUMAGT,EXT)
        IF(EXT.LT.0) GOTO 10
C
C CHECK IF AGENT IS IN FILE
C
        CALL READW(AFDB,AGT,ASFREC,ST)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,ASF),2,ST,AGT)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        DO 1010 I=SAGNO,EAGNO
        IF(ASFBYT(I).NE.CZERO.AND.ASFBYT(I).NE.' ') GOTO 1020
1010    CONTINUE
        CALL CLRSCR(5)
        WRITE(5,903) IAM(),AGT
        CALL XWAIT(2,2,ST)
        GOTO 1000
1020    CONTINUE
        ANUM=0
        CALL ASCBIN(ASFINF,SAGNO,LAGNO,ANUM,ST)
C
C MAKE SURE AGENT IS INACTIVE
C
        IF(ASFINV(ASFACT,1).NE.0.OR.ASFITINV(ASFITDUE).NE.0) THEN
           WRITE(5,911) IAM(),(ASFBYT(I),I=SAGNO,EAGNO)
           CALL XWAIT(3,2,ST)
           GOTO 1000
        ENDIF
C
C CHECK DAYS AFTER LAST INVOICE
C
        ALLTRA = 0
        DO 1022 I=1,9

           IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 1022
C
           DO J = 1, MAXGAM
              IF(ASFDAY(GSCNT,J,I).NE.0)  ALLTRA = ALLTRA + 1
              IF(ASFDAY(GCCNT,J,I).NE.0)  ALLTRA = ALLTRA + 1
              IF(ASFDAY(GVCNT,J,I).NE.0)  ALLTRA = ALLTRA + 1
              IF(ASFDAY(GCLCNT,J,I).NE.0) ALLTRA = ALLTRA + 1
              IF(ASFDAY(GDCNT,J,I).NE.0)  ALLTRA = ALLTRA + 1
              IF(ASFDAY(GRCNT,J,I).NE.0)  ALLTRA = ALLTRA + 1
              IF(ASFDAY(GTKCHG,J,I).NE.0) ALLTRA = ALLTRA + 1
           ENDDO

1022    CONTINUE

        IF(ALLTRA.NE.0) THEN
           WRITE(5,912) IAM(),(ASFBYT(I),I=SAGNO,EAGNO)
           CALL XWAIT(3,2,ST)
           GOTO 1000
        ENDIF
C
C FIND NEXT AVAILABLE SLOT IN DELETED AGENTS FILE
C
1025    CONTINUE
        REC=0
1030    REC=REC+1
        CALL READW(DFDB,REC,BUF,ST)
        IF(ST.EQ.144) THEN
          CALL CLRSCR(5)
          TYPE*,IAM(),' Deleted agent file is full'
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,DAG),2,ST,REC)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        DO 1040 I=2,129
        IF(BUF(I).NE.0) GOTO 1030
1040    CONTINUE
C
C SLOT EMPTY, PRESERVE TERMINAL NUMBER AND WRITE TO DELETED
C AGENTS FILE.
C
        ASFDNM=AGT
        DDAT(5) = DAYCDC
        CALL CDATE(DDAT)
        WRITE (CCDTE,990) DDAT(1),DDAT(2),DDAT(3)
        DO 1045 I=1,6
        ASFBYT(SDELD+I-1)=CDTE(I)
1045    CONTINUE
        CALL WRITEW(DFDB,REC,ASFREC,ST)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,DAG),3,ST,REC)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C CLEAR AGENTS SLOT IN AGENTS SALES FILE
C
        CALL FASTSET(0,ASFREC,ASFSEC*64)

        WORK = 0
        CALL BSET(WORK,AGTWAG)
        CALL BSET(WORK,AGTCAN)
        CALL BSET(WORK,AGTVAL)
        CALL BSET(WORK,AGTDES)
        CALL BSET(WORK,AGTTKM)
        CALL BINASC(ASFINF,STTYP,LTTYP,WORK)

        CALL WRITEW(AFDB,AGT,ASFREC,ST)

        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,ASF),3,ST,AGT)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        WRITE(5,909) IAM(),AGT,(ASFBYT(I),I=SAGNO,EAGNO)
        CALL XWAIT(2,2,ST)
        GOTO 1000
C
C RESTORE DELETED AGENT
C
2000    CONTINUE
        CALL CLRSCR(5)
        CALL WIMG(5,'Enter deleted agent''s lottery number [E-Exit] ')
        READ(5,908) LOTNUM
        IF(LOTNUM(1).EQ.'E') GOTO 10
C
C SEARCH DELETED AGENT FILE FOR AGENT
C
        REC=0
2010    REC=REC+1
        CALL READW(DFDB,REC,ASFREC,ST)
        IF(ST.EQ.144) GOTO 2025
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,DAG),2,ST,REC)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C CHECK FOR END OF FILE
C
        DO 2020 I=1,128
        IF(ASFINF(I).NE.0) GOTO 2030
2020    CONTINUE
C
C END OF FILE AGENT NOT FOUND
C
2025    CONTINUE
        CALL CLRSCR(5)
        WRITE(5,905) IAM(),LOTNUM
        CALL XWAIT(2,2,ST)
        GOTO 2000
C
C
2030    CONTINUE
        DO 2040 I=SAGNO,EAGNO
        IF(ASFBYT(I).NE.LOTNUM(I)) GOTO 2010
2040    CONTINUE
       CALL FASTMOV(ASFREC,BUF,ASFSEC*64)
C
C FOUND AGENT GET NEW TERMINAL NUMBER
C
2050    CONTINUE
        CALL CLRSCR(5)
        CALL INPNUM('Enter new terminal number [E-Exit] ',AGT,
     *              1,NUMAGT,EXT)
        IF(EXT.LT.0) GOTO 10
C
C CHECK IF SLOT ALREADY USED
C
        CALL READW(AFDB,AGT,ASFREC,ST)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,ASF),2,ST,AGT)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        DO 2060 I=SAGNO,EAGNO
        IF(ASFBYT(I).NE.CZERO.AND.ASFBYT(I).NE.' ') THEN
          WRITE(5,906) IAM(),AGT
          CALL XWAIT(2,2,ST)
          GOTO 2050
        ENDIF
2060    CONTINUE
C
C TERMINAL SLOT AVAILABLE WRITE RECORD TO AGENT SALES FILE.
C
2080    CONTINUE
        CALL WRITEW(AFDB,AGT,BUF,ST)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,ASF),3,ST,AGT)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C COMPRESS DELETED AGENTS FILE
C
2085    CONTINUE
        CALL READW(DFDB,REC+1,BUF,ST)
        IF(ST.EQ.144) GOTO 2100
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,DAG),2,ST,REC+1)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C CHECK FOR END OF FILE
C
        DO 2090 I=2,129
        IF(BUF(I).NE.0) GOTO 2130
2090    CONTINUE
C
C END OF FILE, COMPRESSION COMPLETE
C CLEAR LAST SLOT
C
2100    CONTINUE
        CALL FASTSET(0,BUF,ASFSEC*64)
        CALL WRITEW(DFDB,REC,BUF,ST)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,DAG),3,ST,REC)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL CLRSCR(5)
        WRITE(5,907) IAM(),LOTNUM,AGT
        CALL XWAIT(2,2,ST)
        GOTO 2000
C
C
2130    CONTINUE
        CALL WRITEW(DFDB,REC,BUF,ST)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,DAG),3,ST,REC)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        REC=REC+1
        GOTO 2085

3000  CONTINUE                                                                  
C                                                                               
C LOOP FOR ALL RECORDS THAT CONTAIN SOMETHING                                   
C                                                                               
C
C OPEN DELETED AGENT FILE AND PRINT FILE
C
C
        CALL INPNUM('Enter number of report copies: ',COPY,0,20,EXT)
        CALL ROPEN('DELAGT.REP',6,ST)
C
C
        REC=0
3010    REC=REC+1
        CALL READW(DFDB,REC,ASFREC,ST)
        IF(ST.EQ.144) THEN
          CALL CLRSCR(5)
          TYPE*,IAM(),' Deleted agent file processed'
          GOTO 3100
        ELSEIF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,DAG),2,ST,REC)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C         IF(ST.NE.0) THEN
C         WRITE(5,941) ST,REC
C         CALL USRCLOS1(     2)
C         STOP
C         ENDIF
C
C CHECK FOR END OF FILE
C
        DO 3020 I=SAGNO,EAGNO
          IF(ASFBYT(I).EQ.CZERO.OR.ASFBYT(I).EQ.' ') GOTO 3010
3020    CONTINUE
        AGT=ASFDNM
        IF(AGT.LE.0) GOTO 3010
        IF(LINCNT.GT.50) THEN
          LINCNT=0
          CALL TITLE('DELETED AGENT REPORT','DAGREP  ',1,6,PAGE,
     *               DAYCDC)
          WRITE(6,9160)
        ENDIF
C
C REPLACE ALL ZEROES WITH BLANKS
C
        DO 3040 I=1,ALENGTH
          IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '
3040    CONTINUE
C
        LINCNT=LINCNT+1

      WRITE(6,9170) ASFDNM,(ASFBYT(K),K=SAGNO,EAGNO),                       
     *                  (ASFBYT(K),K=SNAME,SNAME+26),                              
     *                  (ASFBYT(K),K=SSTRT,SSTRT+29),                              
     *                  (ASFBYT(K),K=SCITY,ECITY),                              
     *                  (ASFBYT(K),K=STELE,ETELE),                              
     *                  (ASFBYT(K),K=SDELD,EDELD)                               

        GOTO 3010
C
C END OF FILE
C
3100    CONTINUE
        WRITE(6,952)
        CALL USRCLOS1(     6)
        CALL SPOOL('DAGREP.REP',COPY,ST)

        CALL XWAIT(2,2,ST)
        GOTO 10


5000    CONTINUE
        CALL USRCLOS1(     1)
        CALL USRCLOS1(     2)
        CALL CLRSCR(5)
        CALL GSTOP(GEXIT_SUCCESS)
C
C
900     FORMAT(1X,A,' Deleted agent maintenance program ',//,
     *         19X,' DEL >> Delete active agent ',/,
     *         19X,' RES >> Restore deleted agent',/,
     *         19X,' REP >> Generate deleted agent report ',/,                      
     *         19X,' EXT >> Program exit ',//)
901     FORMAT(A3)
902     FORMAT(1X,A,' Invalid function ')
903     FORMAT(1X,A,' Terminal ',I5,' not currently assigned ')
904     FORMAT(//,1X,A,' Terminal ',I5,' deleted ')
905     FORMAT(1X,A,' Agent number ',<LAGNO>A1,' not found ')
906     FORMAT(1X,A,' Terminal ',I5,' currently used ')
907     FORMAT(//,1X,A,1X,<LAGNO>A1,' restored as terminal ',I5)
908     FORMAT(<LAGNO>A1)
909     FORMAT(1X,A,' Terminal ',I5,' Agent ',<LAGNO>A1,' deleted')
910     FORMAT(1X,A,1X,A,1X,8A1)
911     FORMAT(1X,A,' Agent ',<LAGNO>A1,' has not been inactive for a full',
     *         ' invoice period')
912     FORMAT(1X,A,' Agent ',<LAGNO>A1,' has been active after last invoice')
990     FORMAT(I2.2,I2.2,I2.2)

C
C
C
940     FORMAT(' DASF.FIL open error - ',Z8)
C
941     FORMAT(' DASF.FIL read error - ',Z8,' record - ',I4)
C
950     FORMAT(//,2X,' AGENT',3X,'TERMINAL',9X,'AGENT NAME',
     *         17X,'Y T D SALES',6X,'Y T D  COMM',4X,'Y T D BON',4X,
     *         'Y T D AVG',6X,'DATE DEL.',//)
C
951     FORMAT(2X,8A1,3X,I4,4X,30A1,5X,I7,'.',I2.2,7X,I6,'.',I2.2,5X,
     *         I6,'.',I2.2,4X,I6,'.',I2.2,6X,2A1,'/',2A1,'/',2A1)
C
952     FORMAT(' -- END OF REPORT --')
C
C
960     FORMAT(//,2X,'DELETED TOTALS',
     *           38X,I9,'.',I2.2,
     *            5X,I8,'.',I2.2,
     *            3X,I8,'.',I2.2,
     *            2X,I8,'.',I2.2)

9060  FORMAT(1X,'Agent have not been properly deactivated',                     
     *       1X,'--- Invalid request ')                                         
9070  FORMAT(1X,'Deleted agent file is FULL ')                                  
9080  FORMAT(I2.2,I2.2,I2.2)                                                    
9090  FORMAT(1X,'Terminal > ',I5,' is now deleted ')                            
9100  FORMAT(7A1)                                                               
9110  FORMAT(1X,'Agent number ',7A1,' can not be found ')                       
9120  FORMAT(1X,'Terminal > ',I5,' is currently being used  ')                  
9130  FORMAT(//,1X,7A1,' restored as terminal > ',I5)                           


9160  FORMAT(//,1X,'TERM   AGENT',4X,'STORE NAME',19X,'ADDRESS',25X,   
     *          'CITY',13X,'TELEPHONE',5X,'DEL. DATE',//)
C       9170  FORMAT(1X,I5,2X,6A1,'-',A1,2X,27A1,2X,27A1,2X,20A1,      
9170  FORMAT(1X,I4,3X,7A1,2X,27A1,2X,30A1,2X,18A1,2X,
     *          12A1,3X,6A1)                            
      END                                                                       
