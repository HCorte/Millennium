C ASFKATIUPD.for                                                            
C  
C V13 15-MAR-2011 GPW NUMAGT=12288
C V12 18-NOV-2010 FJG Batch2: Fix NOBOUNDS checking errors
C V11 18-JUN-2000 OXK Fix for opening input disk file (CCFILNAM)
C V10 29-JAN-1999 RXK Added DELAGT feature.
C V09 16-APR-1997 RXK Date and time of the creation of the update file 
C                     displayed; Redmax is back
C V08 04-APR-1997 RXK Set agent type for new agents only 
C V07 27-MAR-1997 RXK Report layout changed
C V06 18-MAR-1997 RXK Ticket flag set for each agent, creation of report added
C V05 25-FEB-1997 RXK Update of agent redmax removed
C V04 20-FEB-1997 RXK Bug fixed in random creation of passwords
C V03 19-FEB-1997 RXK Algorithm for passwords changed (agent number used for 
C                     seed, RND64 used)  
C V02 14-FEB-1997 RXK Fix for agent number, creation of passwords added
C V01 07-FEB-1997 RXK Initial revision. 
C  
C PROGRAM TO UPDATE ASF.FIL WITH INFORMATION FROM KATI SYSTEM 
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
C Copyright 1997 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      PROGRAM ASFKATIUPD         
      IMPLICIT NONE                                                  
                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF' 
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:RECAGT.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'

C REPORT PARAMETERS
C -----------------
      INTEGER*4    REPLU/8/
      CHARACTER    HEAD*40
      INTEGER*4    PAGE

C INPUT ASCII FILE PARAMETERS
C ---------------------------
      INTEGER*4    ASCII_REC_LEN
      PARAMETER(ASCII_REC_LEN=529)
      INTEGER*4    ASCII_LU/7/
      CHARACTER    ASCII_REC*529        !  >= ASCII_REC_LEN       

C ASF PARAMETERS
C --------------
      INTEGER*4    ASFFDB(7)
      CHARACTER    C_ASFBYT*760         ! 512 WARNING ALENGTH HARDCODE
      EQUIVALENCE  (C_ASFBYT,ASFBYT)    !ASFBYT is equivalent to ASFINF

C LOCAL VARIABLE
C --------------
      INTEGER*4    UPDCNT    
      INTEGER*4    NEWCNT    
      INTEGER*4    ST
      INTEGER*4    RECN
      INTEGER*4    I,J,K
      INTEGER*4    ERRCNT
      INTEGER*4    AGENT
      INTEGER*4    REC_NBR
      INTEGER*4    LOOKUP(NUMAGT)/NUMAGT*0/ 
      INTEGER*4    OPNSLOT(NUMAGT)/NUMAGT*0/
      INTEGER*4    SLOT
      INTEGER*4    NEXT
      INTEGER*4    SEED
      INTEGER*4    PW(NUMCLERK)
      INTEGER*4    BITMAP
      INTEGER*4    FLAG
      INTEGER*2    DATE(LDATE_LEN)
      INTEGER*4    SKIPPED

      LOGICAL      NEW

      CHARACTER    CZERO*1/Z0/
      CHARACTER*6  C6
      CHARACTER*1  C1
      CHARACTER*2  C2RTYPE
      CHARACTER*1  SDATE(6)
      CHARACTER*6  SDATE1
      CHARACTER*4  DDMM
      CHARACTER*2  YY
      EQUIVALENCE  (SDATE,SDATE1,DDMM)
      EQUIVALENCE  (SDATE(5),YY)
      CHARACTER*3  LANG(3)/'Eng','Fin','Swe'/
      CHARACTER*4  PREFIX
      CHARACTER*8  EDATE        
      INTEGER*4    FILNAME(10)            
      CHARACTER    CFILNAME(40)            
      CHARACTER*40 CCFILNAM
      EQUIVALENCE  (CFILNAME,FILNAME,CCFILNAM)
      LOGICAL      CLR_COMM_INFO
      STRUCTURE /DATE_S/ 
         INTEGER*4  YEAR
         INTEGER*4  MONTH
         INTEGER*4  DAY
      END STRUCTURE
      RECORD /DATE_S/ TODAY,END_DATE
      INTEGER*4    NUMTODEL
      INTEGER*4    DELCNT
      INTEGER*4    DEL_LIST(1000)
      INTEGER*4    AGTEND_DAY,AGTEND_MONTH,AGTEND_YEAR
      LOGICAL      DELETE_AGT

C CALL  COPYRITE  SUBROUTINE                                                    
C --------------------------
      CALL COPYRITE                                                             
                                                                               
      TYPE *                                                                    
      TYPE *,'<<<<< ASFKATIUPD ASF Update with KATI file V01 >>>>>'
      TYPE *                                                                    

      UPDCNT = 0
      ERRCNT = 0
      NEWCNT = 0
      ST = 0
      RECN = 0
      SLOT=0
      NEXT=0
      SKIPPED = 0
      NUMTODEL = 0
C
C OPEN REPORT FILE
C -----------------
      CALL ROPEN('ASFKATI.REP',REPLU,ST)
      IF(ST.NE.0) THEN
         TYPE*,IAM(),'ASFKATI.REP Open error  st - ',ST
         CALL USRCLOS1 (REPLU)
         CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
C GET TODAYS DATE IN FORMAT YYMMDD
C
      DATE(VCDC) = DAYCDC
      CALL LCDATE(DATE)
      TODAY.YEAR  = DATE(VYEAR2)
      TODAY.MONTH = DATE(VMON)
      TODAY.DAY   = DATE(VDAY)
C
C
C OPEN INPUT DISK FILE           
C ---------------------                                            

100   CONTINUE                                                                  
      CALL WIMG(5,'Enter file name (VOLN:FILNAME)        ')
      READ(5,9000) FILNAME
      IF (FILNAME(1).EQ.'    ') GOTO 100
      IF(CFILNAME(1).EQ.'E '.OR.CFILNAME(1).EQ.'e ') 
     *   CALL GSTOP(GEXIT_OPABORT)
      OPEN(UNIT=ASCII_LU,
     *     FILE=CCFILNAM,
     *     IOSTAT=ST,
     *     STATUS='OLD',  
     *     ORGANIZATION='SEQUENTIAL',
     *     ACCESS='SEQUENTIAL')
      IF(ST.NE.0) THEN
         TYPE*,IAM(),CFILNAME,' Open error, status =',ST
         CALL USRCLOS1(ASCII_LU)
         GOTO 100
      ENDIF
C
C OPEN AGENT SALES FILE                                                         
C ---------------------                                                    
      CALL OPENW (ASF,SFNAMES(1,ASF),4,0,0,ST)
      IF(ST.NE.0) CALL FILERR (SFNAMES(1,ASF),1,ST,0)
 
      CALL IOINIT(ASFFDB,ASF,ASFSEC*256)  ! in bytes, not in sectors

C READ INPUT FILE
C -----------------                                  

200   CONTINUE                                                                  
      RECN = RECN + 1 
      READ(UNIT=ASCII_LU,                          
     *        IOSTAT=ST,
     *        END=1000,
     *        FMT='(<ASCII_REC_LEN>A)') ASCII_REC
      IF(ST.NE.0) THEN
         CALL FILERR(FILNAME,2,ST,RECN)
         CALL USRCLOS1(ASCII_LU)
         CALL GSTOP (GEXIT_FATAL)
       ENDIF

C READ ASF AND UPDATE INSTANT TICKET INVOICE FIELDS
C ---------------------------------------------------

      READ(ASCII_REC(3:4),9001) C2RTYPE     !record type
      IF(C2RTYPE.EQ.'60') THEN                     
         GOTO 300                           !header 
      ELSEIF(C2RTYPE.EQ.'61') THEN
         GOTO 400                           !data record
      ELSEIF(C2RTYPE.EQ.'69') THEN
         GOTO 1000                          !end 
      ELSE
         GOTO 200                           !read next
      ENDIF
C
C READ HEADER RECORD   
C -------------------
300   CONTINUE
      TYPE*,IAM(),'KATI update file from ',ASCII_REC(5:6),'-',
     *      ASCII_REC(7:8),'-', ASCII_REC(9:12),' ', ASCII_REC(13:14),':',
     *      ASCII_REC(15:16),':',ASCII_REC(17:18) 
      CALL WIMG(5,'Is this correct (Y/N) ')
      CALL YESNO(FLAG)
      IF(FLAG.NE.1) GOTO 100
C
C CREATE TABLES OF USED AND UNUSED SLOT NUMBERS
C ----------------------------------------------
      DO 350 I=1,NUMAGT
         IF (MOD(I-1,1000).EQ.0) THEN
            TYPE*,' Building lookup table in progress...',I
         ENDIF
         CALL READW(ASFFDB,I,ASFREC,ST)
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,I)

         DO J=SAGNO,EAGNO
            IF(ASFBYT(J).NE.' '.AND.ASFBYT(J).NE.CZERO) THEN
               CALL ASCBIN(ASFINF,SAGNO,LAGNO,LOOKUP(I),ST)
               GOTO 340
            ENDIF
         END DO
         SLOT=SLOT+1
         OPNSLOT(SLOT)=I
         GOTO 350

340      CONTINUE
         AGTEND_DAY = 0
         CALL ASCBIN(ASFINF,SEDAT+0,2,AGTEND_DAY,ST)
         IF(AGTEND_DAY.EQ.0) GOTO 350
         AGTEND_MONTH = 0   
         CALL ASCBIN(ASFINF,SEDAT+2,2,AGTEND_MONTH,ST)
         AGTEND_YEAR = 0
         CALL ASCBIN(ASFINF,SEDAT+4,2,AGTEND_YEAR,ST)
         IF(AGTEND_MONTH.EQ.0) THEN
            TYPE *,IAM(),'Agent ',(ASFINF(J),J=SAGNO,EAGNO),
     *                   ' has an invalid end date'
            GOTO 350
         ENDIF
         IF(AGTEND_YEAR.LT.77) THEN  
            AGTEND_YEAR = AGTEND_YEAR + 2000
         ELSE
            AGTEND_YEAR = AGTEND_YEAR + 1900
         ENDIF

         DELETE_AGT =.FALSE.
         IF(AGTEND_YEAR.LT.TODAY.YEAR) THEN
            DELETE_AGT = .TRUE.
         ELSEIF(AGTEND_YEAR .EQ. TODAY.YEAR .AND. 
     *         AGTEND_MONTH .LT. TODAY.MONTH) THEN
            DELETE_AGT = .TRUE.
         ELSEIF(AGTEND_YEAR .EQ. TODAY.YEAR  .AND.
     *         AGTEND_MONTH .EQ. TODAY.MONTH .AND.
     *         AGTEND_DAY   .LE. TODAY.DAY) THEN
            DELETE_AGT = .TRUE.
         ENDIF
         IF(DELETE_AGT) THEN
            NUMTODEL = NUMTODEL +1
            IF(NUMTODEL.GT.1000) THEN
               TYPE *,IAM(),'Agent ',(ASFINF(J),J=SAGNO,EAGNO),
     *                      ' will be deleted on next run' 
            ELSE
               DEL_LIST(NUMTODEL) = I
            ENDIF
         ENDIF
350   CONTINUE

      TYPE *,IAM(),'Lookup table completed '
      TYPE *,IAM(),' '

      READ(ASCII_REC( 5: 8),9002) DDMM
      READ(ASCII_REC(11:12),9001) YY
      WRITE (HEAD,9020) ASCII_REC(5:6),ASCII_REC(7:8),ASCII_REC(9:12)
      CALL TITLE(HEAD,'ASFKATI.REP',1,REPLU,PAGE,DAYCDC)
      WRITE(REPLU,9019)
      WRITE(REPLU,9017)
      WRITE(REPLU,9019)

      GOTO 200
C
C START TO READ DATA RECORD   
C --------------------------
400   CONTINUE
C
C CHECK THE END DATE... IF THE END DATE IS LESS THAN TODAYS DATE THEN
C SKIP THE RECORD...
C
      READ(ASCII_REC(366:371),9007) C6        !agent 6 bytes
      READ(ASCII_REC(372:372),9011) C1        !agent check number
      AGENT = 10 * CTOI(C6,K) + CTOI(C1,K)
      EDATE(1:8) = ASCII_REC(406:413) ! YEAR/MONTH/DAY
      READ(EDATE(1:4),'(I4)') END_DATE.YEAR
      READ(EDATE(5:6),'(I2)') END_DATE.MONTH
      READ(EDATE(7:8),'(I2)') END_DATE.DAY
      CLR_COMM_INFO = .FALSE.
C
C Compare the dates...
C
      IF(END_DATE.YEAR.NE.0.AND.END_DATE.MONTH.NE.0.AND.END_DATE.DAY.NE.0) THEN
        IF(END_DATE.YEAR.LT.TODAY.YEAR) THEN
            CLR_COMM_INFO = .TRUE.
        ELSEIF(END_DATE.YEAR  .EQ. TODAY.YEAR .AND. 
     *         END_DATE.MONTH .LT. TODAY.MONTH) THEN
            CLR_COMM_INFO = .TRUE.
        ELSEIF(END_DATE.YEAR  .EQ. TODAY.YEAR  .AND.
     *         END_DATE.MONTH .EQ. TODAY.MONTH .AND.
     *         END_DATE.DAY   .LE. TODAY.DAY) THEN
            CLR_COMM_INFO = .TRUE.
        ENDIF
      ENDIF
      REC_NBR = 0
      NEW = .TRUE.
      DO I=1,NUMAGT
         IF (AGENT.EQ.LOOKUP(I)) THEN
             REC_NBR = I
             NEW =.FALSE.
             GOTO 450
         ENDIF
      ENDDO

450   CONTINUE
      IF(CLR_COMM_INFO.AND.NEW) THEN
         TYPE*,' Cannot clear communication info for agent #',AGENT,
     *         ' Agent not found!'
         ERRCNT = ERRCNT + 1
         GOTO 200
      ENDIF       
C
C  FIND NEXT AVAILABLE OPEN SLOT IN THE ASF.FIL.
C ----------------------------------------------
      IF(NEW) THEN                        !get terminal number
         NEXT=NEXT+1                      !if no more available slots
         IF(OPNSLOT(NEXT).LE.0) THEN      !reject the transaction
            ERRCNT = ERRCNT + 1
            WRITE(5,9012) AGENT
            GOTO 300
         ENDIF
         REC_NBR = OPNSLOT(NEXT)
         TYPE*,IAM(),' For agent #',AGENT,' set term # ',REC_NBR
         LOOKUP(REC_NBR) = AGENT
         CALL FASTSET (0,ASFREC,ASFLEN)
         NEWCNT = NEWCNT + 1
      ELSE
         CALL READW (ASFFDB,REC_NBR,ASFREC,ST)
         IF(ST.NE.0) THEN 
            CALL FILERR(SFNAMES(1,ASF),2,ST,REC_NBR)
            TYPE*,' Ter #:',REC_NBR,' *** not found ***'
            ERRCNT = ERRCNT + 1                      
         ENDIF
         UPDCNT=UPDCNT+1                                                        
      ENDIF
C
C IF RETAILER END DATE IS REACHED (FOR GVT) THEN CLEAR GVTID.
C
      IF(CLR_COMM_INFO) THEN
         READ(ASCII_REC(373:373),9011) C1
         IF(C1.EQ.'2') THEN           ! CLEAR COMM INFO FOR GVT ONLY !!!
            DO I=SGFID,EGCKS
              C_ASFBYT(I:I) = ' '
            ENDDO       
         ENDIF                        ! Set end date
         C_ASFBYT(SEDAT+0:SEDAT+1)=EDATE(7:8) ! DAY
         C_ASFBYT(SEDAT+2:SEDAT+3)=EDATE(5:6) ! MONTH
         C_ASFBYT(SEDAT+4:SEDAT+5)=EDATE(3:4) ! YEAR
         TYPE*,'End date updated for agent #',AGENT
         SKIPPED = SKIPPED + 1
         GOTO 510                      ! WRITE THIS RECORD BACK TO ASF.
      ENDIF
C
C CONTINUE TO READ DATA RECORD
C ------------------------------

      WRITE(C_ASFBYT(SAGNO:EAGNO),9015) AGENT

      READ(ASCII_REC( 39: 42),9002) C_ASFBYT(SCHAN:ECHAN) 
      READ(ASCII_REC( 50: 76),9008) C_ASFBYT(SNAME:ENAME) 
      READ(ASCII_REC( 77:103),9008) C_ASFBYT(SSTRT:ESTRT) 
      READ(ASCII_REC(104:108),9009) C_ASFBYT(SZIPC:EZIPC) 
      READ(ASCII_REC(109:126),9014) C_ASFBYT(SCITY:ECITY) 
      READ(ASCII_REC(134:145),9010) C_ASFBYT(STELE:ETELE) 
      READ(ASCII_REC(254:280),9010) C_ASFBYT(SCONT:ECONT) 
      READ(ASCII_REC(293:296),9002) C_ASFBYT(SMONO:EMONO) 
      READ(ASCII_REC(297:300),9002) C_ASFBYT(SMONC:EMONC) 
      READ(ASCII_REC(301:304),9002) C_ASFBYT(STUEO:ETUEO) 
      READ(ASCII_REC(305:308),9002) C_ASFBYT(STUEC:ETUEC) 
      READ(ASCII_REC(309:312),9002) C_ASFBYT(SWEDO:EWEDO) 
      READ(ASCII_REC(313:316),9002) C_ASFBYT(SWEDC:EWEDC) 
      READ(ASCII_REC(317:320),9002) C_ASFBYT(STHUO:ETHUO) 
      READ(ASCII_REC(321:324),9002) C_ASFBYT(STHUC:ETHUC) 
      READ(ASCII_REC(325:328),9002) C_ASFBYT(SFRIO:EFRIO) 
      READ(ASCII_REC(329:332),9002) C_ASFBYT(SFRIC:EFRIC) 
      READ(ASCII_REC(333:336),9002) C_ASFBYT(SSATO:ESATO) 
      READ(ASCII_REC(337:340),9002) C_ASFBYT(SSATC:ESATC) 
      READ(ASCII_REC(341:344),9002) C_ASFBYT(SSUNO:ESUNO) 
      READ(ASCII_REC(345:348),9002) C_ASFBYT(SSUNC:ESUNC) 
      READ(ASCII_REC(349:349),9011) C_ASFBYT(SLANG:ELANG) 
      READ(ASCII_REC(378:381),9002) C_ASFBYT(SRENT:ERENT) 
      READ(ASCII_REC(452:457),9007) C_ASFBYT(SARED:EARED)
C
C CHANGE LANGUAGE DEFINITION FROM THAT OF VEIKKAUS TO GTECH'S.
C--------------------------------------------------------------
C     Veikkaus:  1,3 = Finnish,  2,4 = Swedish
C     GTECH:     0 = English,  1 = Finnish,  2 = Swedish

      IF(C_ASFBYT(SLANG:ELANG).EQ.'3') C_ASFBYT(SLANG:ELANG)='1'
      IF(C_ASFBYT(SLANG:ELANG).EQ.'4') C_ASFBYT(SLANG:ELANG)='2'
C
C FORCE THE STARTUP DATE
C --------------------------  
      IF(NEW) THEN
         C_ASFBYT(SSDAT:ESDAT)=SDATE1
         IF(C_ASFBYT(SSDAT:ESDAT).EQ.'      ') WRITE(5,9013) IAM(),AGENT
C
C SET AGENT TYPE 
C ---------------
         BITMAP = 0

         READ(ASCII_REC(373:373),9011) C1    
         IF(C1.EQ.'1'.OR.C1.EQ.'3') CALL BSET(BITMAP,AGTTOI)  !terminal

         READ(ASCII_REC(510:510),9011) C1    
         IF(C1.EQ.'1') CALL BSET(BITMAP,AGTPRV)          !priveleged
           
         READ(ASCII_REC(511:511),9011) C1     
         IF(C1.EQ.'1') CALL BSET(BITMAP,AGTXFR)          !pack transfer allowed

         CALL BSET(BITMAP,AGTTKM)

         WRITE(C_ASFBYT(STTYP:ETTYP),9016) BITMAP
C
C GENERATE PASSWORDS
C -------------------
         SEED = AGENT
         DO I=1,NUMCLERK
500         CONTINUE
            CALL RND64(SEED,1,1,9999,4)
            PW(I) = SEED 
            IF(I.GE.2) THEN 
               DO K=1,I-1
                  IF(PW(K).EQ.PW(I)) THEN
                     SEED = SEED + 1 
                     GOTO 500
                  ENDIF
               ENDDO
            ENDIF 
         ENDDO

         WRITE(C_ASFBYT(SPAS1:EPAS1),9004) PW(1)
         WRITE(C_ASFBYT(SPAS2:EPAS2),9004) PW(2)
         WRITE(C_ASFBYT(SPAS3:EPAS3),9004) PW(3)
         WRITE(C_ASFBYT(SPAS4:EPAS4),9004) PW(4)
         WRITE(C_ASFBYT(SPAS5:EPAS5),9004) PW(5)
         WRITE(C_ASFBYT(SPAS6:EPAS6),9004) PW(6)
         WRITE(C_ASFBYT(SPAS7:EPAS7),9004) PW(7)
         WRITE(C_ASFBYT(SPAS8:EPAS8),9004) PW(8)
 
      ENDIF
510   CONTINUE
      CALL WRITEW (ASFFDB,REC_NBR,ASFREC,ST)                         
      IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,REC_NBR)

C WRITE REPORT
C -------------
      I = CTOI(C_ASFBYT(ELANG:ELANG),K)
      PREFIX = '    ' 
      IF(NEW) PREFIX = 'Uusi'

      WRITE(REPLU,9018) PREFIX,
     *    C_ASFBYT(SAGNO:EAGNO-1),
     *    C_ASFBYT(EAGNO:EAGNO),
     *    REC_NBR,
     *    C_ASFBYT(SCHAN:ECHAN),          
     *    C_ASFBYT(SNAME:ENAME),
     *    C_ASFBYT(SSTRT:ESTRT),
     *    C_ASFBYT(SCITY:ECITY),
     *    C_ASFBYT(SZIPC:EZIPC),
     *    C_ASFBYT(STELE:ETELE),
     *    LANG(I+1),
     *    C_ASFBYT(SRENT:ERENT)

      GOTO 200                                                                  
                                                                               
1000  CONTINUE                                                                  

      CALL CLOSEFIL(ASFFDB)                                                     
      CLOSE(UNIT=ASCII_LU)

      IF(NUMTODEL.GT.0) CALL DELAGT_LIST(DEL_LIST,NUMTODEL,DELCNT)

      WRITE(5,9006)                                                             
      WRITE(5,9003) UPDCNT,   'Updated               '
      WRITE(5,9003) NEWCNT,   'Added      '
      WRITE(5,9003) DELCNT,   'Deleted      '
      IF(NUMTODEL.GT.DELCNT) 
     *   WRITE(5,9003) NUMTODEL-DELCNT,   'Not Deleted      '
      IF(ERRCNT.GT.0)  WRITE(5,9003) ERRCNT, 'Errors (Not Processed)'     
                                                                   
      IF((UPDCNT+NEWCNT+DELCNT).EQ.0) THEN
         WRITE(5,9005)                                    
      ENDIF

      CALL GSTOP(GEXIT_SUCCESS)                          
                                                                               
C     =================== Format Statements ================                    
                                                                               
9000  FORMAT(10A4) 
9001  FORMAT(A2)
9002  FORMAT(A4)
9003  FORMAT(12X,I5,T21,'Record(s) ',A) 
9004  FORMAT(I4.4)
9005  FORMAT(/,1X,'ASFKATIUPD: There have been no updates today',/)
9006  FORMAT(1X,'ASFKATIUPD:',T21,A,T40,F14.2)
9007  FORMAT(A6)
9008  FORMAT(A27)
9009  FORMAT(A5)
9010  FORMAT(A12)
9011  FORMAT(A1)
9012  FORMAT(/,' Agent ',I7,' cannot be added. No more available',
     *         ' open slots in the ASF.FIL ')
9013  FORMAT(1X,A,1X,'Bad startup date for agent ',I7)
9014  FORMAT(A18)
9015  FORMAT(I7.7)
9016  FORMAT(I10.10)
9017  FORMAT(1X,'       Agent  Term Crtl Name',T53,'Street',T83,'City',
     *       T101,'Zip code ','Phone',T122,'Lng Rent')
9018  FORMAT(1X,A4,1X,A6,'-',A1,1X,I5,1X,A4,1X,A27,A30,A18,A9,A12,A3,1X,A4) 
9019  FORMAT(1X,131('='),/)
9020  FORMAT(' ASF UPDATE WITH KATI FILE (',A2,'.',A2,'.',A4,')')
C
      END
