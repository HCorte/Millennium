C
C V02 17-MAR-1999 RXK Added check of the due in IPS invoice.
C V01 11-JAN-1999 RXK Initial release.
C
C DELETION OF LISTED AGENTS. SEE ALSO PROGRAM DELAGT.
C REMOVE AGENT FROM LIVE AGENT SALES FILE AND PUT INTO DELETED
C AGENTS FILE (DASF.FIL).
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DELAGT_LIST(DEL_LIST,NUMTODEL,DELCNT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C

        INTEGER*4 DEL_LIST(1000), NUMTODEL, DELCNT
        INTEGER*4 REC, I, J, K, EXT, AGT
        INTEGER*4 DFDB(7),AFDB(7),BUF(ASFSEC*64)
        INTEGER*4 ST
        INTEGER*2 DDAT(12)

        INTEGER*4 LINCNT, PAGE, COPY, WORK, ALLTRA

        CHARACTER*6 CCDTE
        CHARACTER CZERO,CDTE(6)
        EQUIVALENCE (CDTE,CCDTE)


        DATA LINCNT/70/,PAGE/0/
        DATA CZERO/Z0/
C
C OPEN AGENT SALES FILE AND DELETED AGENTS FILE
C
        CALL OPENW(1,SFNAMES(1,ASF),0,0,0,ST)
        CALL IOINIT(AFDB,1,ASFSEC*256)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,ASF),1,ST,0)
          RETURN
        ENDIF
C
        CALL OPENW(2,SFNAMES(1,DAG),0,0,0,ST)
        CALL IOINIT(DFDB,2,ASFSEC*256)
        IF(ST.NE.0) THEN
          CALL FILERR(SFNAMES(1,DAG),1,ST,0)
          RETURN
        ENDIF

        CALL ROPEN('DELAGTA.REP',6,ST)

        DELCNT = 0
C
C DELETE ACTIVE AGENT
C
        DO 9000 J=1,NUMTODEL
           AGT = DEL_LIST(J) 
C
C CHECK IF AGENT IS IN FILE
C
           CALL READW(AFDB,AGT,ASFREC,ST)
           IF(ST.NE.0) THEN
             CALL FILERR(SFNAMES(1,ASF),2,ST,AGT)
             GOTO 9000 
           ENDIF
           DO 1010 I=SAGNO,EAGNO
              IF(ASFBYT(I).NE.CZERO.AND.ASFBYT(I).NE.' ') GOTO 1020
1010       CONTINUE
           WRITE(5,903) IAM(),AGT
           CALL XWAIT(2,2,ST)
           GOTO 9000
C
C MAKE SURE AGENT IS INACTIVE
C
1020       CONTINUE
           IF(ASFINV(ASFACT,1).NE.0.OR.ASFITINV(ASFITDUE).NE.0) THEN
              WRITE(5,904) IAM(),(ASFBYT(I),I=SAGNO,EAGNO)
              GOTO 9000
           ENDIF
C
C CHECK DAYS AFTER LAST INVOICE
C
           ALLTRA = 0
           DO 1022 I=1,9

              IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 1022
C
              DO K = 1, MAXGAM
                 IF(ASFDAY(GSCNT,K,I).NE.0)  ALLTRA = ALLTRA + 1
                 IF(ASFDAY(GCCNT,K,I).NE.0)  ALLTRA = ALLTRA + 1
                 IF(ASFDAY(GVCNT,K,I).NE.0)  ALLTRA = ALLTRA + 1
                 IF(ASFDAY(GCLCNT,K,I).NE.0) ALLTRA = ALLTRA + 1
                 IF(ASFDAY(GDCNT,K,I).NE.0)  ALLTRA = ALLTRA + 1
                 IF(ASFDAY(GRCNT,K,I).NE.0)  ALLTRA = ALLTRA + 1
                 IF(ASFDAY(GTKCHG,K,I).NE.0) ALLTRA = ALLTRA + 1
              ENDDO

1022       CONTINUE

           IF(ALLTRA.NE.0) THEN
              WRITE(5,905) IAM(),(ASFBYT(I),I=SAGNO,EAGNO)
              CALL XWAIT(3,2,ST)
              GOTO 9000
           ENDIF
C
C FIND NEXT AVAILABLE SLOT IN DELETED AGENTS FILE
C
1025       CONTINUE
           REC=0
1030       REC=REC+1
           CALL READW(DFDB,REC,BUF,ST)
           IF(ST.EQ.144) THEN
              TYPE*,IAM(),' Deleted agent file is full'
              GOTO 9900
           ENDIF
           IF(ST.NE.0) THEN
              CALL FILERR(SFNAMES(1,DAG),2,ST,REC)
              GOTO 9900
           ENDIF
           DO 1040 I=2,129
              IF(BUF(I).NE.0) GOTO 1030
1040       CONTINUE
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
1045       CONTINUE
           CALL WRITEW(DFDB,REC,ASFREC,ST)
           IF(ST.NE.0) THEN
              CALL FILERR(SFNAMES(1,DAG),3,ST,REC)
              GOTO 9900
           ENDIF
C
C WRITE REPORT
C
           IF(LINCNT.GT.50) THEN
              LINCNT=0
              CALL TITLE('AUTOMATICALLY DELETED AGENT REPORT',
     *                   'AUTODELA',1,6,PAGE,DAYCDC)
              WRITE(6,9160)
           ENDIF
C
C REPLACE ALL ZEROES WITH BLANKS
C
           DO 3040 I=1,ALENGTH
              IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '
3040       CONTINUE
C
           LINCNT=LINCNT+1

           WRITE(6,9170) ASFDNM,(ASFBYT(K),K=SAGNO,EAGNO),
     *                  (ASFBYT(K),K=SNAME,ENAME),                              
     *                  (ASFBYT(K),K=SSTRT,ESTRT),                              
     *                  (ASFBYT(K),K=SCITY,ECITY),                              
     *                  (ASFBYT(K),K=STELE,ETELE),                              
     *                  (ASFBYT(K),K=SDELD,EDELD)                               

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
              GOTO 9900
           ENDIF
           WRITE(5,909) IAM(),AGT,(ASFBYT(I),I=SAGNO,EAGNO)

           DELCNT=DELCNT + 1           

9000    CONTINUE  

9900    CONTINUE

        WRITE(6,952)
        CALL USRCLOS1(     6)
        CALL USRCLOS1(     1)
        CALL USRCLOS1(     2)

        RETURN
C
C
903     FORMAT(1X,A,' Terminal ',I4,' not currently assigned ')
904     FORMAT(1X,A,' Agent ',<LAGNO>A1,' has not been inactive for a full',
     *         ' invoice period')    
905     FORMAT(1X,A,' Agent ',<LAGNO>A1,' has been active after last invoice')
909     FORMAT(1X,A,' Terminal ',I4,' Agent ',<LAGNO>A1,' deleted')
990     FORMAT(I2.2,I2.2,I2.2)
952     FORMAT('0END OF REPORT')
9160  FORMAT(//,1X,'TERM   AGENT',4X,'STORE NAME',19X,'ADDRESS',25X,   
     *          'CITY',13X,'TELEPHONE',5X,'DEL. DATE',//)
C       9170  FORMAT(1X,I4,3X,6A1,'-',A1,2X,27A1,2X,27A1,2X,20A1,      
9170  FORMAT(1X,I4,3X,7A1,2X,27A1,2X,30A1,2X,27A1,2X,
     *          12A1,3X,6A1)                            
      END                                                                       
