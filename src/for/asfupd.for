C
C ASFUPD.for                                                                    
C
C V15 18-NOV-2010 FJG Batch2: Fix NOBOUNDS checking errors
C
C $Log:   GXAFXT:[GOLS]ASFUPD.FOV  $
C  
C     Rev 1.1   18 Dec 1996 11:56:52   HXK
C  Update from TEBE project (MXP,WXW,PXN,MJF)
C  
C     Rev 1.1   30 Oct 1996 13:39:12   RXK
C  Force starting date for the agent to be the same as
C  the date of creation the ftp tape/file. (wxw)
C  
C     Rev 1.0   17 Apr 1996 12:13:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.5   19 Jul 1995 15:36:48   PXB
C  Bug Fix. Will now not treat file header record as an agent record.
C  
C     Rev 1.4   02 Sep 1994 17:59:54   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.4   26 Jun 1994 13:49:04   HXK
C  FTP version.
C  
C     Rev 1.3   14 Nov 1993 22:13:24   HXK
C  HAD TO REJIG LANGUAGE CODE STUFF, PRINT OUT TERMINAL NUMBERS TO CONSOLE.
C  
C     Rev 1.2   13 Nov 1993 21:34:32   HXK
C  AMENDED ASFUPD FOR LANGUAGE CODES (AND RESTORED COMMENTED OUT READS
C  AND WRITES (?)).
C  
C     Rev 1.1   08 Sep 1993 17:10:26   HXN
C  Updated AGTINF fields according to VEIKKAUS desiradata.
C  
C     Rev 1.0   03 Sep 1993 11:39:28   HXK
C  Initial revision.
C       
c V04  2-JUL-93 HJK CONVERTED TO VAX
C V03 28-OCT-91 PP  MODIFIED AGENTS ARE WRITTEN TO A                            
C                   DIFFERENT REPORT THAN THE ADDED ONES                        
C V02 26-JUL-91 HJK CHANGED TO INCLUDE SHOP HOURS                               
C V01 21-SEP-89 MGM INITIAL RELEASE FOR FINLAND                                 
C                                                                               
C PROGRAM TO READ AGENT MODIFICATIONS FILE SENT FROM THE LOTTERY                
C AND APPLY CHANGES TO OUR AGENT SALES FILE (ASF.FIL)                           
C                                                                               
C IF AGENT ALREADY EXISTS MODIFY THE RECORD WITH INFORMATION                    
C SENT FROM THE LOTTERY.  IF AGENT DOESN'T EXIST ADD HIM IN.                    
C                                                                               
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
C Copyright 1993 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      PROGRAM ASFUPD                                                            
      IMPLICIT NONE                                                  
                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF' 
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:RECAGT.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
                                                                               
C REPORT PARAMETERS
C -----------------
      INTEGER*4    REPLU1/9/
      INTEGER*4    REPLU2/8/
      INTEGER*4    REPLU3/7/

C TAPE PARAMETERS
C ---------------
      INTEGER*4	   TAPE_BLOCK_SIZE,
     *		   TAPE_BLOCK_SIZE_BYTES,
     *		   DISK_LU,
     *		   TAPE_LU
      PARAMETER    (TAPE_BLOCK_SIZE = 128,   ! I4 or 512 bytes 
     *		    TAPE_BLOCK_SIZE_BYTES = TAPE_BLOCK_SIZE*4,
     *		    DISK_LU         = 4, 
     *		    TAPE_LU         = 3 
     *             )


      CHARACTER*20 TAPE_NAME
      INTEGER*4    TAPE_FDB(7),
     *	           TAPE_REC(TAPE_BLOCK_SIZE) ! 128 I4    or 512 bytes

      CHARACTER    TAPE_ASFBYT*760               !TAPE REC in characters WARNING ALENGTH HARDCODE
      EQUIVALENCE  (TAPE_REC,TAPE_ASFBYT)


C       Header layout
C	-------------
        INTEGER*4 LH_DATE,SH_DATE,               !date
     *	          LH_TIME,SH_TIME,               !time
     *		  LH_DAYNBR,SH_DAYNBR,           !day number
     *		  LH_NBREC,SH_NBREC              !number of records
   
        PARAMETER(LH_DATE   = 6)
        PARAMETER(LH_TIME   = 6)
        PARAMETER(LH_DAYNBR = 3)
        PARAMETER(LH_NBREC  = 5)

        PARAMETER(SH_DATE   = 1,
     *            SH_TIME   = 8,
     *            SH_DAYNBR = 15,
     *		  SH_NBREC  = 19
     *           )   


	CHARACTER HDATE*6,
     *		  HTIME*6,
     *		  HDAYNBR*3,
     *            HNBREC*5

	CHARACTER*6 SDATE


C ASF PARAMETERS
C --------------
      INTEGER*4    ASFFDB(7)

      CHARACTER    C_ASFBYT*760         !512 WARNING ALENGTH HARDCODE
      EQUIVALENCE  (C_ASFBYT,ASFBYT)    !ASFBYT is equivalent to ASFINF

      CHARACTER    OLD_ASFBYT*760       !keeping original AGTINF field



C LOCAL VARIABLE
C --------------
      INTEGER*4    REC_NBR    /0/,
     *	           TAPE_REC_NBR /0/

      INTEGER*4    OPNSLOT(NUMAGT)                                             
      INTEGER*4    LOOKUP(NUMAGT)  !contains Agent # only.


      CHARACTER    DATE*4                                                      
      CHARACTER    ACTION*7                                                    
      CHARACTER    CZERO*1/Z0/                                                 
      CHARACTER    TEXT*13                                                   
                                                                               
      INTEGER*4    XDATE                                                       
      INTEGER*4    DEVICE                                                      

      INTEGER*4    NUMADD/0/                                                   
      INTEGER*4    NUMCHG/0/                                                   


      INTEGER*4    ST
      INTEGER*4    I
      INTEGER*4    PAGE
      INTEGER*4    FDB
      INTEGER*4    K
      INTEGER*4    ANSWER
      INTEGER*4    J
      INTEGER*4    SLOT
      INTEGER*4    NEXT
      INTEGER*4    ERRCNT
      INTEGER*4    REC
      INTEGER*4    REPLU
      INTEGER*4    AGENT
                                
      LOGICAL      MODIFY/.FALSE./
                                                                               
      DATA NEXT/0/,SLOT/0/,ERRCNT/0/                                            
      COMMON       SCFREC                                                       

      EQUIVALENCE (XDATE,DATE)                                                  
C      EQUIVALENCE (RECORDS,TAPE_ASFBYT(9:12))

       INTEGER*4   DISK_FDB(7)           
       INTEGER*4   FILNAME(7)            
       CHARACTER   CFILNAME(28)            
       EQUIVALENCE (CFILNAME,FILNAME)
       INTEGER*4   DISKOPENZ
       EXTERNAL    DISKOPENZ 


C Begin code --------------------------------------------------


C CALL  COPYRITE  SUBROUTINE                                                    
C --------------------------
      CALL COPYRITE                                                             
                                                                               
      TYPE *                                                                    
      TYPE *,'<<<<< ASFUPD Agent Sales File Update  V01 >>>>>'                  
      TYPE *                                                                    

C OPEN REPORT FILES
C -----------------
      CALL ROPEN('ASFUPD.REP',REPLU1,ST)      
      IF(ST.NE.0) THEN
         TYPE*,IAM(),'ASFUPD.REP Open error  st - ',ST
         CALL USRCLOS1 (REPLU1)
         CALL GSTOP(GEXIT_FATAL)
      ENDIF
                                                                               
      CALL ROPEN('ASFCMP1.REP',REPLU2,ST)       
      IF(ST.NE.0)THEN                                                           
         TYPE *,' Error opening ASFCMP1.REP > ',ST                              
         CALL GSTOP(GEXIT_FATAL)
C        CALL GPAUSE
      ENDIF                                                                     

      CALL ROPEN('ASFCMP2.REP',REPLU3,ST)                 
      IF(ST.NE.0)THEN                                                           
         TYPE *,' Error opening ASFCMP2.REP > ',ST                              
         CALL GSTOP(GEXIT_FATAL)
C        CALL GPAUSE
      ENDIF                                                                     

      CALL TITLE('AGENT UPDATE EXCEPTIONS','ASFUPD.REP',                        
     *            1,REPLU1,PAGE,DAYCDC)             
      WRITE(REPLU1,9000)              
                                                                               
C GET DEVICE                                                                    
C ----------                                            
100   CONTINUE                                                                  
      CALL INPNUM('Enter device type (1-Disk , 2-Tape) :   ',                   
     *             DEVICE,1,2,ST)                                               
      IF(ST.LT.0) STOP                                                          

      IF(DEVICE .EQ. 2) THEN                                                    
         CALL WIMG (5,'Mount tape and enter mag name : ')
         ACCEPT 801, TAPE_NAME
801      FORMAT(A) 
         CALL TAPOPEN (TAPE_FDB,TAPE_NAME,ST)
         IF(ST.NE.0) THEN
            TYPE*,'Tape Open error  - ',ST,' on ',TAPE_NAME
            CALL GSTOP (GEXIT_SUCCESS)
         ENDIF   
         CALL XREWIND (TAPE_FDB,ST)
         IF(ST.NE.0) THEN
            TYPE*,'Tape rewind error  - ',ST,' on ',TAPE_NAME
            CALL GSTOP (GEXIT_SUCCESS)
         ENDIF
         CALL TAPINT(TAPE_FDB,TAPE_LU,TAPE_BLOCK_SIZE*4) ! in bytes
      ELSEIF(DEVICE .EQ. 1) THEN                                                    
120   CONTINUE                                                                  
           CALL WIMG(5,'Enter file name (VOLN:FILNAME)        ')
           READ(5,901) FILNAME
	   IF (FILNAME(1).EQ.'    ') GOTO 120
           IF(CFILNAME(1).EQ.'E '.OR.CFILNAME(1).EQ.'e ') 
     *        CALL GSTOP(GEXIT_OPABORT)
           OPEN(UNIT=DISK_LU,FILE=FILNAME,IOSTAT=ST,
     *       STATUS='OLD', BLOCKSIZE=512,
     *       ORGANIZATION='SEQUENTIAL',
     *       ACCESS='SEQUENTIAL',
     *       USEROPEN=DISKOPENZ,
     *       RECORDTYPE='FIXED')
          IF(ST.NE.0) THEN
              TYPE*,IAM(),CFILNAME,' Open error,  status =',ST
              CALL USRCLOS1(DISK_LU)
              GOTO 120
          ENDIF
          CALL IOINIT(DISK_FDB,DISK_LU,TAPE_BLOCK_SIZE*4) ! in bytes
          CALL READW(DISK_FDB,TAPE_REC_NBR,TAPE_REC,ST)   ! header

	  !---- Added 19-July-1995. 
	  !---- Increment tape record number here so as when the
	  !---- next file read takes place it will pick up the data
	  !---- record. 
          TAPE_REC_NBR = TAPE_REC_NBR + 1

      ELSE
         TYPE*,IAM(),'Option not available'
         CALL GSTOP (GEXIT_SUCCESS)
      ENDIF

      IF(DEVICE.EQ.2) THEN
C READ TAPE HEADER INFO                        
C ---------------------                        
         CALL FASTSET (0,TAPE_REC,TAPE_BLOCK_SIZE)
         CALL RTAPEW  (TAPE_FDB,TAPE_REC,ST)
         IF(ST.NE.0)THEN                                                        
            TYPE *,'Error reading tape - ',ST                                   
            CALL GPAUSE
         ENDIF                                                                 
      ENDIF                                                                 

      HDATE  (1:6) = TAPE_ASFBYT(1:6)        
      HTIME  (1:6) = TAPE_ASFBYT(8:13)        
      HDAYNBR(1:3) = TAPE_ASFBYT(15:17)        
      HNBREC (1:5) = TAPE_ASFBYT(19:23)        

      TYPE*,IAM(),' DATE         :',HDATE
      TYPE*,IAM(),' TIME         :',HTIME
      TYPE*,IAM(),' DAYNBR       :',HDAYNBR
      TYPE*,IAM(),' NBREC        :',HNBREC
                                                                               
      CALL WIMG (5,'Is this the correct file? (Y/N)         ')                  
      CALL YESNO(ANSWER)                                                        
      IF (ANSWER .NE. 1) THEN
         IF(DEVICE.EQ.2) THEN
	    CALL XREWIND (TAPE_FDB,ST)
            CALL TAPCLOS (TAPE_FDB,ST) !CLOSE TAPE.
	 ENDIF
         CALL GSTOP (GEXIT_SUCCESS)
      ENDIF
C
C GET THE START DATE FOR NEW AGENT. THE SAME AS THE DATE ON THE FTP FILE  
C DO SOME ACROBATICS TO HAVE IT IN DDMMYY FORMAT
C
	SDATE = '      '
	J=8
        DO 20 I=1,5,2
          J=J-2
          SDATE(J-1:J)=HDATE(I:I+1)
20      CONTINUE
C
C OPEN AGENT SALES FILE                                                         
C ---------------------                                                    

      CALL OPENW (ASF,SFNAMES(1,ASF),4,0,0,ST)
      IF(ST.NE.0) CALL FILERR (SFNAMES(1,ASF),1,ST,0)
 
      CALL IOINIT(ASFFDB,ASF,ASFSEC*256)  ! in bytes, not in sectors

      DO 50 I=1,NUMAGT                                                          
	 IF (MOD(I-1,1000).EQ.0) THEN
            TYPE*,' Building lookup table in progress...',I
	 ENDIF

         CALL FASTSET (0,ASFREC,ASFLEN)
         CALL READW(ASFFDB,I,ASFREC,ST)                                         
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,I)

         DO J=SAGNO,EAGNO                                                    
            IF(ASFBYT(J).NE.' '.AND.ASFBYT(J).NE.CZERO) THEN 
               CALL ASCBIN(ASFINF,SAGNO,LAGNO,LOOKUP(I),ST)
	       GOTO 50
	    ENDIF
 	 END DO

         SLOT=SLOT+1                                                            
         OPNSLOT(SLOT)=I                                                        
 50   CONTINUE                                                                  


      TYPE*,' '
      TYPE*,' Number of free slots in ASF.FIL :',SLOT

C      DO I=1,NUMAGT
C         IF (OPNSLOT(I).NE.0) THEN
C            TYPE*,' OPNSLOT(I) :',I,OPNSLOT(I)
C         ENDIF
C      END DO

      TYPE*,' '
C      DO I=1,NUMAGT
C         IF (LOOKUP(I).NE.0) THEN
C            WRITE (REPLU1,5005) I,LOOKUP(I)
C 5005	    FORMAT(1X,' LOOKUP(I) :',I5,'  ',I12)
C         ENDIF
C      END DO

      TYPE *,IAM(),'Lookup table completed '                                          
      TYPE *,IAM(),' '

C LOOP READING TAPE
C -----------------                                                                               

300   CONTINUE                                                                  

      CALL FASTSET (0,TAPE_REC,TAPE_BLOCK_SIZE)
      IF(DEVICE.EQ.2) THEN
         CALL RTAPEW  (TAPE_FDB,TAPE_REC,ST)            
         IF(ST.EQ.136.OR.ST.EQ.144.OR.ST.EQ.152) GOTO 1000 ! EOT

         IF(ST.NE.0)THEN                                                     
	   TYPE *,'Error reading tape - ',ST                                 
	   CALL GPAUSE                                     
         END IF                                                              
      ELSE
         CALL READW(DISK_FDB,TAPE_REC_NBR,TAPE_REC,ST)                                         
         IF(ST.EQ.136.OR.ST.EQ.144.OR.ST.EQ.152) GOTO 1000 ! EOT
         IF(ST.NE.0) THEN
	    CALL FILERR(FILNAME,2,ST,I)
            CALL GSTOP (GEXIT_FATAL)
	 ENDIF
      ENDIF
C     Process request
C     ---------------
      TAPE_REC_NBR = TAPE_REC_NBR + 1
      REC_NBR      = 0
      OLD_ASFBYT   = ' '
      MODIFY       = .FALSE.

      AGENT = CTOI (TAPE_ASFBYT(1:LAGNO),K)   !LAGNO=7

      DO 410 J=1,NUMAGT                                                         
         IF (AGENT.EQ.LOOKUP(J)) THEN
            MODIFY  = .TRUE.         
	    REC_NBR = J

C           WRITE (REPLU1,5005) J,LOOKUP(J)
C 5005	    FORMAT(1X,' J,   LOOKUP(J) :',I5,'  ',I12)

            GOTO 420                                                            
         ENDIF                                                                  
410   CONTINUE                                                                  
                                                                               
420   CONTINUE                                                                  
                                                                                
      IF(MODIFY) THEN         ! MODIFY AGENT

C	 Read original record and update ONLY the Agent identification
C	 section, write updated record back to the asf.fil            
C	 -------------------------------------------------

         DO REC=1,NUMAGT                                                    
            IF (AGENT.EQ.LOOKUP(REC)) THEN
                REC_NBR = REC
                GOTO 501                                  
            ENDIF
	 ENDDO
                                                                               
 501     CONTINUE 

         TYPE*,IAM(),TAPE_REC_NBR,' Agent #:',AGENT,' Ter #:',REC_NBR

         CALL FASTSET (0,ASFREC,ASFLEN)
         CALL READW (ASFFDB,REC_NBR,ASFREC,ST)
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,REC_NBR)

         OLD_ASFBYT(1:512)=C_ASFBYT(1:512) !save original record for comparison

C	 Update some AGTINF fields only
C	 ------------------------------
         C_ASFBYT(SAGNO:EAGNO) = TAPE_ASFBYT(SAGNO:EAGNO)
         C_ASFBYT(SNAME:ENAME) = TAPE_ASFBYT(SNAME:ENAME)
         C_ASFBYT(SCONT:ECONT) = TAPE_ASFBYT(SCONT:ECONT)

         C_ASFBYT(SSTRT:ESTRT) = TAPE_ASFBYT(SSTRT:ESTRT)
         C_ASFBYT(SCITY:ECITY) = TAPE_ASFBYT(SCITY:ECITY)

         C_ASFBYT(SZIPC:EZIPC) = TAPE_ASFBYT(SZIPC:EZIPC)

         C_ASFBYT(STELE:ETELE) = TAPE_ASFBYT(STELE:ETELE)

         C_ASFBYT(SHOUR:EHOUR) = TAPE_ASFBYT(SHOUR:EHOUR)

         C_ASFBYT(SCHAN:ECHAN) = TAPE_ASFBYT(SCHAN:ECHAN)

         C_ASFBYT(SRENT:ERENT) = TAPE_ASFBYT(SRENT:ERENT)

         C_ASFBYT(SLANG:ELANG) = TAPE_ASFBYT(SLANG:ELANG)

C Change language definition from that of Veikkaus to GTECH's.
C        Veikkaus:  0 = Finnish,  1 = Swedish,  2 = English
C        GTECH:     0 = English,  1 = Finnish,  2 = Swedish

         ASFBYT(ELANG)=CHAR(ICHAR(ASFBYT(ELANG))+1)
         IF(ASFBYT(ELANG).EQ.'3') ASFBYT(ELANG)='0'

         CALL WRITEW (ASFFDB,REC_NBR,ASFREC,ST)                         
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,REC_NBR)

         NUMCHG=NUMCHG+1                                                        
         ACTION='CHANGED'                                                       
	 REPLU = REPLU3                                              
	 TEXT  = 'C H A N G E D'                                       

      ELSE  !ADD NEW AGENT                        

C        Write next available open slot in the asf.fil.          
C        --------------------------------------------          
         NEXT=NEXT+1                      !IF NO MORE AVAILABLE SLOTS           

         IF(OPNSLOT(NEXT).LE.0) THEN      !REJECT THE TRANSACTION               
            ERRCNT = ERRCNT + 1                                                 
            WRITE(REPLU1,9009) TAPE_ASFBYT(SAGNO:EAGNO)      
            GOTO 300                                                            
         ENDIF                                                                  

         TYPE*,IAM(),TAPE_REC_NBR,' Agent #:',AGENT,' Ter #:',OPNSLOT(NEXT)

         CALL READW(ASFFDB,OPNSLOT(NEXT),ASFREC,ST) !don't need to read.
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,OPNSLOT(NEXT))

         CALL FASTSET (0,ASFREC,ASFLEN)
         C_ASFBYT(1:512) = TAPE_ASFBYT(1:512)  !overwrite the whole agtinf
C
C Force the startup date
C
	 C_ASFBYT(SSDAT:ESDAT)=SDATE
         IF(C_ASFBYT(SSDAT:ESDAT).EQ.'      ') WRITE(5,9010) IAM(),AGENT

C Change language definition from that of Veikkaus to GTECH's.
C        Veikkaus:  0 = Finnish,  1 = Swedish,  2 = English
C        GTECH:     0 = English,  1 = Finnish,  2 = Swedish

         ASFBYT(ELANG)=CHAR(ICHAR(ASFBYT(ELANG))+1)
         IF(ASFBYT(ELANG).EQ.'3') ASFBYT(ELANG)='0'

         CALL WRITEW(ASFFDB,OPNSLOT(NEXT),ASFREC,ST)               
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,OPNSLOT(NEXT))

         NUMADD=NUMADD+1                                                        
         ACTION='ADDED  '                                                       
	 REPLU = REPLU2                              
	 TEXT  = 'A D D E D'                                           
      ENDIF                                                                     

      CALL TITLE('AGENT COMPARE REPORT','  ASFCMP',1,REPLU,PAGE,                
     *            DAYCDC)                                                       
      WRITE(REPLU,6050) TEXT                                                  
      WRITE(REPLU,7000)                                                         
      WRITE(REPLU,8000) C_ASFBYT(SAGNO:EAGNO),ACTION,                            
     *              OLD_ASFBYT(SAGNO:EAGNO),C_ASFBYT(SAGNO:EAGNO),                  
     *              OLD_ASFBYT(SNAME:ENAME),C_ASFBYT(SNAME:ENAME),                  
     *              OLD_ASFBYT(SCONT:ECONT),C_ASFBYT(SCONT:ECONT),                  
     *              OLD_ASFBYT(SCCON:ECCON),C_ASFBYT(SCCON:ECCON),
     *              OLD_ASFBYT(SSTRT:ESTRT),C_ASFBYT(SSTRT:ESTRT),                  
     *              OLD_ASFBYT(SCITY:ECITY),C_ASFBYT(SCITY:ECITY),                  
     *              OLD_ASFBYT(SSTTE:ESTTE),C_ASFBYT(SSTTE:ESTTE),
     *              OLD_ASFBYT(SZIPC:EZIPC),C_ASFBYT(SZIPC:EZIPC),
     *              OLD_ASFBYT(SSSNO:ESSNO),C_ASFBYT(SSSNO:ESSNO),
     *              OLD_ASFBYT(STELE:ETELE),C_ASFBYT(STELE:ETELE),                  
     *              OLD_ASFBYT(SPAS1:EPAS1),C_ASFBYT(SPAS1:EPAS1),                  
     *              OLD_ASFBYT(SPAS2:EPAS2),C_ASFBYT(SPAS2:EPAS2),                  
     *              OLD_ASFBYT(SPAS3:EPAS3),C_ASFBYT(SPAS3:EPAS3),                  
     *              OLD_ASFBYT(SPAS4:EPAS4),C_ASFBYT(SPAS4:EPAS4),                  
     *              OLD_ASFBYT(SPAS5:EPAS5),C_ASFBYT(SPAS5:EPAS5),                  
     *              OLD_ASFBYT(SPAS6:EPAS6),C_ASFBYT(SPAS6:EPAS6),                  
     *              OLD_ASFBYT(SPAS7:EPAS7),C_ASFBYT(SPAS7:EPAS7)                   
      WRITE(REPLU,8001) OLD_ASFBYT(SPAS8:EPAS8),C_ASFBYT(SPAS8:EPAS8),              
     *              OLD_ASFBYT(SPRIO:EPRIO),C_ASFBYT(SPRIO:EPRIO),                  
     *              OLD_ASFBYT(SEKEY:EEKEY),C_ASFBYT(SEKEY:EEKEY),                  
     *              OLD_ASFBYT(SEFLG:EEFLG),C_ASFBYT(SEFLG:EEFLG),                  
     *              OLD_ASFBYT(STTYP:ETTYP),C_ASFBYT(STTYP:ETTYP),                  
     *              OLD_ASFBYT(SGAME:EGAME),C_ASFBYT(SGAME:EGAME),                  
     *              OLD_ASFBYT(SARED:EARED),C_ASFBYT(SARED:EARED),                  
     *              OLD_ASFBYT(SARMN:EARMN),C_ASFBYT(SARMN:EARMN),
     *              OLD_ASFBYT(SDSTS:EDSTS),C_ASFBYT(SDSTS:EDSTS),
     *              OLD_ASFBYT(SISTS:EISTS),C_ASFBYT(SISTS:EISTS),
     *              OLD_ASFBYT(SSDAT:ESDAT),C_ASFBYT(SSDAT:ESDAT),
     *              OLD_ASFBYT(SEDAT:EEDAT),C_ASFBYT(SEDAT:EEDAT)
                                                                               
      CALL TITLE('AGENT COMPARE REPORT','  ASFCMP',1,REPLU,PAGE,                
     *            DAYCDC)                                                       
      WRITE(REPLU,7000)                                                         
      WRITE(REPLU,8002) C_ASFBYT(SAGNO:EAGNO),ACTION,                            
     *              OLD_ASFBYT(SMAIN:EMAIN),C_ASFBYT(SMAIN:EMAIN),                  
     *              OLD_ASFBYT(SINST:EINST),C_ASFBYT(SINST:EINST),                  
     *              OLD_ASFBYT(SINSP:EINSP),C_ASFBYT(SINSP:EINSP),                  
     *              OLD_ASFBYT(SACPD:EACPD),C_ASFBYT(SACPD:EACPD),                  
     *              OLD_ASFBYT(SACPT:EACPT),C_ASFBYT(SACPT:EACPT),                  
     *              OLD_ASFBYT(SBRAK:EBRAK),C_ASFBYT(SBRAK:EBRAK),                  
     *              OLD_ASFBYT(SCIRC:ECIRC),C_ASFBYT(SCIRC:ECIRC),
     *              OLD_ASFBYT(SCKL:ECKL),  C_ASFBYT(SCKL:ECKL),
     *              OLD_ASFBYT(SNOTR:ENOTR),C_ASFBYT(SNOTR:ENOTR)                   
      WRITE(REPLU,8003) OLD_ASFBYT(SMONO:EMONO),OLD_ASFBYT(SMONC:EMONC),              
     *              C_ASFBYT(SMONO:EMONO),C_ASFBYT(SMONC:EMONC),                  
     *              OLD_ASFBYT(STUEO:ETUEO),OLD_ASFBYT(STUEC:ETUEC),                  
     *              C_ASFBYT(STUEO:ETUEO),C_ASFBYT(STUEC:ETUEC),                  
     *              OLD_ASFBYT(SWEDO:EWEDO),OLD_ASFBYT(SWEDC:EWEDC),                  
     *              C_ASFBYT(SWEDO:EWEDO),C_ASFBYT(SWEDC:EWEDC),                  
     *              OLD_ASFBYT(STHUO:ETHUO),OLD_ASFBYT(STHUC:ETHUC),                  
     *              C_ASFBYT(STHUO:ETHUO),C_ASFBYT(STHUC:ETHUC),                  
     *              OLD_ASFBYT(SFRIO:EFRIO),OLD_ASFBYT(SFRIC:EFRIC),                  
     *              C_ASFBYT(SFRIO:EFRIO),C_ASFBYT(SFRIC:EFRIC),                  
     *              OLD_ASFBYT(SSATO:ESATO),OLD_ASFBYT(SSATC:ESATC),                  
     *              C_ASFBYT(SSATO:ESATO),C_ASFBYT(SSATC:ESATC),                  
     *              OLD_ASFBYT(SSUNO:ESUNO),OLD_ASFBYT(SSUNC:ESUNC),                  
     *              C_ASFBYT(SSUNO:ESUNO),C_ASFBYT(SSUNC:ESUNC)                   
      WRITE(REPLU,8005) C_ASFBYT(SAGNO:EAGNO),ACTION,                            
     *              OLD_ASFBYT(SPAYT:EPAYT),C_ASFBYT(SPAYT:EPAYT),                  
     *              OLD_ASFBYT(SBDIS:EBDIS),C_ASFBYT(SBDIS:EBDIS),                  
     *              OLD_ASFBYT(SBCOD:EBCOD),C_ASFBYT(SBCOD:EBCOD),                  
     *              OLD_ASFBYT(SBACC:EBACC),C_ASFBYT(SBACC:EBACC)                  
                                                                               
      CALL TITLE('AGENT COMPARE REPORT','  ASFCMP',1,REPLU,PAGE,                
     *            DAYCDC)                                                       
      WRITE(REPLU,7000)                                                         
      WRITE(REPLU,8006) C_ASFBYT(SAGNO:EAGNO),ACTION,                            
     *              OLD_ASFBYT(SATYP:EATYP),C_ASFBYT(SATYP:EATYP),                  
     *              OLD_ASFBYT(SBUSC:EBUSC),C_ASFBYT(SBUSC:EBUSC),                  
     *              OLD_ASFBYT(SCMPT:ECMPT),C_ASFBYT(SCMPT:ECMPT),
     *              OLD_ASFBYT(SRESR:ERESR),C_ASFBYT(SRESR:ERESR),                  
     *              OLD_ASFBYT(SRANK:ERANK),C_ASFBYT(SRANK:ERANK),                  
     *              OLD_ASFBYT(SCCDE:ECCDE),C_ASFBYT(SCCDE:ECCDE),                  
     *              OLD_ASFBYT(STERR:ETERR),C_ASFBYT(STERR:ETERR),                  
     *              OLD_ASFBYT(SCHAN:ECHAN),C_ASFBYT(SCHAN:ECHAN)                   
      WRITE(REPLU,8007) C_ASFBYT(SAGNO:EAGNO),ACTION,                            
     *              OLD_ASFBYT(STERN:ETERN),C_ASFBYT(STERN:ETERN),                  
     *              OLD_ASFBYT(SRENT:ERENT),C_ASFBYT(SRENT:ERENT),                  
     *              OLD_ASFBYT(SONLN:EONLN),C_ASFBYT(SONLN:EONLN),                  
     *              OLD_ASFBYT(STCDE:ETCDE),C_ASFBYT(STCDE:ETCDE),                  
     *              OLD_ASFBYT(SSTAT:ESTAT),C_ASFBYT(SSTAT:ESTAT),                  
     *              OLD_ASFBYT(SACOK:EACOK),C_ASFBYT(SACOK:EACOK),                  
     *              OLD_ASFBYT(STLOK:ETLOK),C_ASFBYT(STLOK:ETLOK),                  
     *              OLD_ASFBYT(SLANG:ELANG),C_ASFBYT(SLANG:ELANG),                  
     *              OLD_ASFBYT(STSER:ETSER),C_ASFBYT(STSER:ETSER),                  
     *              OLD_ASFBYT(SCNTY:ECNTY),C_ASFBYT(SCNTY:ECNTY),                  
     *              OLD_ASFBYT(SDELD:EDELD),C_ASFBYT(SDELD:EDELD),                  
     *              OLD_ASFBYT(SGCTY:EGCTY),C_ASFBYT(SGCTY:EGCTY),                  
     *              OLD_ASFBYT(SOSTE:EOSTE),C_ASFBYT(SOSTE:EOSTE),                 
     *              OLD_ASFBYT(SFZON:EFZON),C_ASFBYT(SFZON:EFZON)
                  
C     ==================== End of Main Loop ==================                  
C                                                                               
      GOTO 300                                                                  
                                                                               
1000  CONTINUE                                                                  
      CALL CLOSEFIL(ASFFDB)                                                     
      IF(DEVICE.EQ.2) THEN
         CALL XREWIND (TAPE_FDB,ST)
         CALL TAPCLOS (TAPE_FDB,ST) !CLOSE TAPE.
      ELSE
      CALL CLOSEFIL(FDB)                                                     
      ENDIF

      WRITE(5,9003) NUMADD,       'Added                 '        
      WRITE(5,9003) NUMCHG,       'Changed               '
      WRITE(5,9006)                                                             
      WRITE(5,9003) NUMADD+NUMCHG,'Processed             '    
      WRITE(5,9006)                                                             
      IF(ERRCNT.GT.0)THEN                                                       
            WRITE(5,9003) ERRCNT,    'Errors (Not Processed)'     
C            CALL SPOOL('ASFUPD.REP',1,ST)                                          
      ENDIF                                                                     
                                                                               
      IF((NUMADD+NUMCHG).EQ.0) WRITE(5,9005)                                    
                                                                               
C     =================== Format Statements ================                    
                                                                               
901   FORMAT(7A4)                                                                
9000  FORMAT(1X,129('='),/)                                                     
9001  FORMAT(I1)                                                                
9002  FORMAT(A4)                                                                
9003  FORMAT(1X,I5,T15,'Record (s) ',A22) 
9014  FORMAT(1X,5A4,' read error ',I4,' record ',I4)                            
9024  FORMAT(1X,5A4,' write error ',I4,' record ',I4)                           
9005  FORMAT(/,1X,'ASFUPD: There have been no updates today',/)                 
9006  FORMAT('====================================')                            
9007  FORMAT(/,1X,' No header record for file ....',/)                          
9008  FORMAT(/,1X,'File for period ending: ',6A2,/,                             
     *       1X,'Records to be processed:              ',I4)                    
9009  FORMAT(/,' Agent ',A7,' cannot be added. No more available',              
     *         ' open slots in the ASF.FIL ')                                   
9010  FORMAT(1X,A,1X,'Bad startup date for agent',I4)

6050  FORMAT(61X,A13)                                                           
7000  FORMAT(1X,T25,'*** BEFORE ***',T90,'*** AFTER ***')                       
8000  FORMAT(1X,131('='),//,1X,'AGENT ',A7,' INFORMATION: ',A7,//,              
     *       1X,'AGENT NUMBER',T25,A7,T65,'AGENT NUMBER',T90,A7,/,              
     *       1X,'SALES PLACE ',T25,A27,T65,'SALES PLACE',T90,A27,/,             
     *       1X,'CONTACT     ',T25,A27,T65,'CONTACT    ',T90,A27,/,             
     *       1X,'CLM. CONTACT',T25,A20,T65,'CLM. CONTACT',T90,A27/,
     *       1X,'STREET ADDR ',T25,A30,T65,'STREET ADDR',T90,A27,/,             
     *       1X,'CITY        ',T25,A18,T65,'CITY       ',T90,A20,/,             
     *       1X,'SITE        ',T25,A2,T65,'SITE        ',T90,A2,/,
     *       1X,'POST CODE   ',T25,A9,T65,'POST CODE   ',T90,A9,/,
     *       1X,'TAX ID      ',T25,A9,T65,'TAX ID      ',T90,A9,/,
     *       1X,'TELEPHONE   ',T25,A12,T65,'TELEPHONE  ',T90,A12,/,             
     *       1X,'PASSWORD 1  ',T25,A4,T65,'PASSWORD 1  ',T90,A4,/,              
     *       1X,'PASSWORD 2  ',T25,A4,T65,'PASSWORD 2  ',T90,A4,/,              
     *       1X,'PASSWORD 3  ',T25,A4,T65,'PASSWORD 3  ',T90,A4,/,              
     *       1X,'PASSWORD 4  ',T25,A4,T65,'PASSWORD 4  ',T90,A4,/,              
     *       1X,'PASSWORD 5  ',T25,A4,T65,'PASSWORD 5  ',T90,A4,/,              
     *       1X,'PASSWORD 6  ',T25,A4,T65,'PASSWORD 6  ',T90,A4,/,              
     *       1X,'PASSWORD 7  ',T25,A4,T65,'PASSWORD 7  ',T90,A4)                
8001  FORMAT(1X,'PASSWORD 8   ',T25,A4,T65,'PASSWORD 8 ',T90,A4,/,              
     *       1X,'PRIORITY     ',T25,A2,T65,'PRIORITY   ',T90,A2,/,              
     *       1X,'ENCRIPT KEY  ',T25,A5,T65,'ENCRIPT KEY',T90,A5,/,              
     *       1X,'ENCRIPT FLAG ',T25,A1,T65,'ENCRIPT FLAG',T90,A1,/,             
     *       1X,'TERMINAL TYPE',T25,A10,T65,'TERMINAL TYPE',T90,A10,/,          
     *       1X,'GAME TYPE    ',T25,A10,T65,'GAME TYPE  ',T90,A10,/,            
     *       1X,'RED MAX      ',T25,A6,T65,'RED MAX    ',T90,A6,/,
     *       1X,'RED MIN      ',T25,A4,T65,'RED MIN    ',T90,A4,/,
     *       1X,'DAILY STAT   ',T25,A1,T65,'DAILY STAT ',T90,A4,/,
     *       1X,'INSTANT STAT ',T25,A1,T65,'INSTANT STAT',T90,A4,/,
     *       1X,'START DATE   ',T25,A6,T65,'START DATE ',T90,A6,/,
     *       1X,'END DATE     ',T25,A6,T65,'END DATE   ',T90,A6)
8002  FORMAT(1X,131('='),//,1X,'AGENT ',A7,                                     
     *       ' HOTLINE INFORMATION: ',A7,//,                                    
     *       1X,'MAINTANCE CO  ',T25,A2,T65,'MAINTANCE CO',T90,A2,/,            
     *       1X,'INSTALL DAY   ',T25,A6,T65,'INSTALL DAY ',T90,A6,/,            
     *       1X,'INSPECTION DAY',T25,A6,T65,'INSPECTION DAY',T90,A6,/,          
     *       1X,'ACPT DAY      ',T25,A6,T65,'ACPT DAY    ',T90,A6,/,            
     *       1X,'ACPT TYPE     ',T25,A1,T65,'ACPT TYPE   ',T90,A1,/,            
     *       1X,'BREAK         ',T25,A9,T65,'BREAK       ',T90,A9,/,            
     *       1X,'CIRCUIT       ',T25,A9,T65,'CIRCUIT     ',T90,A9,/,            
     *       1X,'CKL           ',T25,A5,T65,'CKL         ',T90,A9,/,
     *       1X,'NO TERMINAL   ',T25,A1,T65,'NO TERMINAL ',T90,A1)              
8003  FORMAT(1X,'MON HRS',T25,A4,'-',A4,T65,'MON HRS',T90,A4,'-',A4,/,          
     *       1X,'TUE HRS',T25,A4,'-',A4,T65,'TUE HRS',T90,A4,'-',A4,/,          
     *       1X,'WED HRS',T25,A4,'-',A4,T65,'WED HRS',T90,A4,'-',A4,/,          
     *       1X,'THU HRS',T25,A4,'-',A4,T65,'THU HRS',T90,A4,'-',A4,/,          
     *       1X,'FRI HRS',T25,A4,'-',A4,T65,'FRI HRS',T90,A4,'-',A4,/,          
     *       1X,'SAT HRS',T25,A4,'-',A4,T65,'SAT HRS',T90,A4,'-',A4,/,          
     *       1X,'SUN HRS',T25,A4,'-',A4,T65,'SUN HRS',T90,A4,'-',A4)            
8005  FORMAT(//,1X,'AGENT ',A7,' BANKING INFORMATION: ',A7,//,                  
     *       1X,'PAY TYPE      ',T25,A1,T65,'PAY TYPE     ',T90,A1,/,           
     *       1X,'BANK DISTRICT ',T25,A1,T65,'BANK DISTRICT',T90,A1,/,           
     *       1X,'BANK CODE     ',T25,A1,T65,'BANK CODE    ',T90,A1,/,           
     *       1X,'BANK ACCOUNT ',T25,A14,T65,'BANK ACCOUNT',T90,A14)           
8006  FORMAT(1X,131('='),//,1X,'AGENT ',A7,                                     
     *       ' MARKETING INFORMATION: ',A7,//,                                  
     *       1X,'AGENT TYPE    ',T25,A2,T65,'AGENT TYPE   ',T90,A2,/,           
     *       1X,'BUSINESS CODE ',T25,A4,T65,'BUSINESS CODE',T90,A4,/,           
     *       1X,'COMPANY TYPE  ',T25,A1,T65,'COMPANY TYPE ',T90,A4,/,
     *       1X,'RES RATE      ',T25,A1,T65,'RES RATE     ',T90,A1,/,           
     *       1X,'SALES RANKING ',T25,A5,T65,'SALES RANKING',T90,A5,/,           
     *       1X,'COUNTY CODE   ',T25,A2,T65,'COUNTY CODE  ',T90,A3,/,           
     *       1X,'TERRITORY     ',T25,A2,T65,'DISTRICT CODE',T90,A3,/,           
     *       1X,'CARTEL CODE   ',T25,A4,T65,'CARTEL CODE  ',T90,A4)           
8007  FORMAT(//,1X,'GTRACK AGENT ',A7,' INFORMATION: ',A7,//,                   
     *       1X,'TERMINAL #   ',T25,A5,T65,'TERMINAL #   ',T90,A5,/,            
     *       1X,'RENTAL       ',T25,A4,T65,'RENTAL       ',T90,A4,/,            
     *       1X,'ONLINE CODE  ',T25,A1,T65,'ONLINE CODE  ',T90,A1,/,            
     *       1X,'TERM CODE    ',T25,A1,T65,'TERM CODE    ',T90,A1,/,            
     *       1X,'TERM STATUS  ',T25,A1,T65,'TERM STATUS  ',T90,A1,/,            
     *       1X,'POWER CHECK OK',T25,A1,T65,'POWER CHECK OK',T90,A1,/,          
     *       1X,'TELEPHONE OK ',T25,A1,T65,'TELEPHONE OK ',T90,A1,/,            
     *       1X,'LANGUAGE CODE',T25,A1,T65,'LANGUAGE CODE',T90,A1,/,            
     *       1X,'TERM SERIAL #',T25,A1,T65,'TERM SERIAL #',T90,A1,/,            
     *       1X,'COUNTY NAME  ',T25,A10,T65,'COUNTY NAME ',T90,A10,/,           
     *       1X,'DATE DELETED ',T25,A6,T65,'DATE DELETED ',T90,A6,/,            
     *       1X,'OWNERS CITY  ',T25,A17,T65,'OWNERS CITY ',T90,A17,/,           
     *       1X,'OWNERS STATE ',T25,A17,T65,'OWNERS STATE',T90,A17,/,   
     *       1X,'SERVICE ZONE ',T25,A3,T65,'SERVICE ZONE',T90,A3)
                                                                               
      CALL GSTOP(GEXIT_SUCCESS)                          

      END   ! ASFUPD.FCC

