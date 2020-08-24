C
C ASFX2XUPD.for                                                            
C
C V04 18-NOV-2010 FJG Batch2: Fix NOBOUNDS checking errors
C
C $Log:   GXAFIP:[GOLS]ASFX2XUPD.FOV  $
C  
C     Rev 1.3   28 Mar 1997 16:51:22   HXK
C  Added check for station class
C  
C     Rev 1.2   25 Mar 1997 15:18:24   RXK
C  Changes for GVT 
C  
C     Rev 1.1   13 Sep 1996  2:03:34   HXK
C  Fix for strange last minute change to X2X adress and group number
C  
C     Rev 1.0   09 Sep 1996 12:34:34   RXK
C  Initial revision.
C  
C PROGRAM TO READ AGENT MODIFICATIONS FILE SENT FROM THE LOTTERY                
C AND APPLY CHANGES TO OUR AGENT SALES FILE (ASF.FIL)
C MODIFICATIONS CONCERN WITH X2X INFORMATION ONLY (SPECTRA III CONVERSION )
C                                                                               
C AGENTS ARE SUPPOSED TO EXIST, NO NEW AGENTS ADDED
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
C Copyright 1996 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      PROGRAM ASFX2XUPD         
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
      INTEGER*4    REPLU/7/
      CHARACTER    HEAD*40

C INPUT ASCII FILE PARAMETERS
C ---------------------------
      INTEGER*4    ASCII_REC_LEN
      PARAMETER(ASCII_REC_LEN=80)
      INTEGER*4	   ASCII_LU/6/
      CHARACTER    ASCII_REC*80    ! == ASCII_REC_LEN       

C Header layout
C --------------

	CHARACTER HDATE*10,           ! date
     *		  HTIME*8             ! time

C ASF PARAMETERS
C --------------

      INTEGER*4    ASFFDB(7)
      CHARACTER    C_ASFBYT*760         !512 WARNING ALENGTH HARDCODE
      EQUIVALENCE  (C_ASFBYT,ASFBYT)    !ASFBYT is equivalent to ASFINF

C LOCAL VARIABLE
C --------------
      INTEGER*4    REC_NBR 
      INTEGER*4    RECS /0/

      INTEGER*4    OPNSLOT(NUMAGT)                                             
      INTEGER*4    LOOKUP(NUMAGT)  !contains Agent # only.

!     CHARACTER    FILNAM*16  REMOVE WARNING
      CHARACTER    CZERO*1/Z0/                                                 
                                                                               
      INTEGER*4    NUMCHG/0/                                                   

      INTEGER*4    ST/0/
      INTEGER*4    I
      INTEGER*4    PAGE
      INTEGER*4    K
!     INTEGER*4    STATUS   REMOVE WARNING
      INTEGER*4    J
      INTEGER*4    SLOT
      INTEGER*4    ERRCNT
      INTEGER*4    REC
      INTEGER*4    AGENT
      INTEGER*4    NUMSUM
      INTEGER*4    START
      INTEGER*4    NUMEXCL
      INTEGER*4    CLS
      INTEGER*4    STN
      CHARACTER*2  C2
      CHARACTER*5  C5
                                
      LOGICAL      GVT
      LOGICAL      MODIFY/.FALSE./
      LOGICAL      PROMPT/.FALSE./

      DATA SLOT/0/,ERRCNT/0/                                            

      INTEGER*4   FILNAME(5)            
      CHARACTER   CFILNAME(20)            
      EQUIVALENCE (CFILNAME,FILNAME)

      INTEGER*4   YESNO
      CHARACTER   CYESNO(4)
      EQUIVALENCE (CYESNO,YESNO)

C CALL  COPYRITE  SUBROUTINE                                                    
C --------------------------
      CALL COPYRITE                                                             
                                                                               
      TYPE *                                                                    
      TYPE *,'<<<<< ASFX2XUPD Agent Sales File Comm Part Update V01 >>>>>'
      TYPE *                                                                    

C ASK OPERATER FOR AUTOMATIC OR PROMPT MODE 
C ----------------------------------------- 
10    CONTINUE
      CALL WIMG(5,'Do you want to be prompted for each agent (Y/N)? ')
      READ(5,902) YESNO
      IF (YESNO.EQ.'    ') GOTO 10
      IF(CYESNO(1).EQ.'E '.OR.CYESNO(1).EQ.'e ') CALL GSTOP(GEXIT_OPABORT)
      IF(CYESNO(1).EQ.'Y '.OR.CYESNO(1).EQ.'y ') THEN
	 PROMPT = .TRUE.
      ELSE
	 PROMPT = .FALSE.
      ENDIF

C OPEN INPUT DISK FILE           
C ----------                                            
100   CONTINUE                                                                  
      CALL WIMG(5,'Enter file name (VOLN:FILNAME)        ')
      READ(5,901) FILNAME
      IF (FILNAME(1).EQ.'    ') GOTO 100
      IF(CFILNAME(1).EQ.'E '.OR.CFILNAME(1).EQ.'e ') 
     *   CALL GSTOP(GEXIT_OPABORT)
      OPEN(UNIT=ASCII_LU,
     *     FILE=FILNAME,
     *     IOSTAT=ST,
     *     STATUS='OLD',  
     *     ORGANIZATION='SEQUENTIAL',
     *     ACCESS='SEQUENTIAL')
      IF(ST.NE.0) THEN
         TYPE*,IAM(),CFILNAME,' Open error, status =',ST
         CALL USRCLOS1(ASCII_LU)
         GOTO 100
      ENDIF
      READ(UNIT=ASCII_LU,                     !header
     *        IOSTAT=ST,
     *        FMT='(<ASCII_REC_LEN>A)') ASCII_REC
      HDATE = ASCII_REC(2:11)
      HTIME = ASCII_REC(12:19)
C
C OPEN REPORT FILE
C -----------------
      CALL ROPEN('ASFX2XUPD.REP',REPLU,ST)      
      IF(ST.NE.0) THEN
         TYPE*,IAM(),'ASFX2XUPD.REP Open error  st - ',ST
         CALL USRCLOS1 (REPLU)
         CALL GSTOP(GEXIT_FATAL)
      ENDIF
      WRITE (HEAD,9001) HDATE,HTIME                              
      CALL TITLE(HEAD,'ASFX2XUPD.REP',1,REPLU,PAGE,DAYCDC)             
      WRITE(REPLU,9000)              
      WRITE(REPLU,8000)
      WRITE(REPLU,9000)              
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

      TYPE *,IAM(),'Lookup table completed '         
      TYPE *,IAM(),' '

C LOOP READING INPUT FILE
C -----------------                                  

300   CONTINUE                                                                  

      READ(UNIT=ASCII_LU,                             !data record
     *        IOSTAT=ST,
     *        FMT='(<ASCII_REC_LEN>A)') ASCII_REC
      IF(ASCII_REC(1:1).EQ.'9') GOTO 1000             ! EOF
      IF(ST.NE.0) THEN
	 CALL FILERR(FILNAME,2,ST,I)
         CALL GSTOP (GEXIT_FATAL)
       ENDIF

C     Process request
C     ---------------
      RECS = RECS + 1
      REC_NBR      = 0
      MODIFY       = .FALSE.

      AGENT = CTOI (ASCII_REC(2:LAGNO+1),K)
      DO 410 J=1,NUMAGT                                                         
         IF (AGENT.EQ.LOOKUP(J)) THEN
            MODIFY  = .TRUE.         
	    REC_NBR = J
            GOTO 420                                                            
         ENDIF                                                                  
410   CONTINUE
      TYPE*,IAM(),' Agent #:',AGENT,' Ter #:',REC_NBR,' *** not found ***'
      ERRCNT = ERRCNT + 1                      
                                                                               
420   CONTINUE                                                                  

      IF(PROMPT) THEN
         TYPE*,IAM(),' Agent #:',AGENT,' Ter #:',REC_NBR,' to be updated'
         CALL WIMG(5,'Do you want to update this agent (Y/N)? ')
         READ(5,902) YESNO
         IF (YESNO.EQ.'    ') GOTO 420
         IF(CYESNO(1).EQ.'E '.OR.CYESNO(1).EQ.'e ') CALL GSTOP(GEXIT_OPABORT)
         IF(CYESNO(1).EQ.'Y '.OR.CYESNO(1).EQ.'y ') THEN
	    IF(MODIFY) MODIFY = .TRUE.  !redundant but makes intention explicit
         ELSE
	    MODIFY = .FALSE.
         ENDIF

      ENDIF
	
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
                                                                               
501      CONTINUE 

         CALL READW (ASFFDB,REC_NBR,ASFREC,ST)
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,REC_NBR)

C	 Update AGTINF comm info fields
C	 ------------------------------
         READ(ASCII_REC(9:10),903) C2
         CLS = CTOI(C2,K)
	 IF(CLS.NE.2 .AND.
     *      CLS.NE.9 .AND.
     *      CLS.NE.12) THEN
	    TYPE*,IAM(),'Agent# ',AGENT,' has bad station class ',CLS
	 ENDIF
         GVT = .FALSE.
         IF(CLS.EQ.9 .OR. CLS.EQ.12) GVT=.TRUE.
         C_ASFBYT(SSCLS:ESCLS) = ASCII_REC(9:10)
C
         READ(ASCII_REC(11:15),904) C5
         STN = CTOI(C5,K)
         IF(GVT) THEN
             IF(STN.LT.4201 .OR. STN.GT.6144) THEN
                TYPE*,IAM(),' GVT Agent #:',AGENT,' Ter #:',REC_NBR,
     *              '  station #',STN
                ERRCNT=ERRCNT + 1
                GOTO 300
             ENDIF
         ELSE
             IF(STN.LT.1001 .OR. STN.GT.4080) THEN
                TYPE*,IAM(),' SPIII Agent #:',AGENT,' Ter #:',REC_NBR,
     *              '  station #',STN
                ERRCNT=ERRCNT + 1
                GOTO 300
             ENDIF
         ENDIF         
         C_ASFBYT(SXSTN:EXSTN) = ASCII_REC(11:15)
         START = NUMEXCL(ASCII_REC(16:20)) +16
         C_ASFBYT(SXPRT:EXPRT) = ASCII_REC(START:20)
         START = NUMEXCL(ASCII_REC(21:22)) +21
         C_ASFBYT(SDROP:EDROP) = ASCII_REC(START:22)
         START = NUMEXCL(ASCII_REC(23:34)) +23
         IF(.NOT.GVT) C_ASFBYT(SGPHN:EGPHN) = ASCII_REC(START:34)
         IF(ASCII_REC(35:50).NE.'                ') THEN
            START = NUMEXCL(ASCII_REC(35:50)) +35        !GVT and X2X address
            C_ASFBYT(SXADR:EXADR) = ASCII_REC(START:50)  !occupy same space in
            IF(GVT) C_ASFBYT(SGCKS:EGCKS) = '00'         !record
         ENDIF                                     
         START = NUMEXCL(ASCII_REC(51:53)) +51
         IF(.NOT.GVT) C_ASFBYT(SXGRP:EXGRP) = ASCII_REC(START:53)

         CALL WRITEW (ASFFDB,REC_NBR,ASFREC,ST)                         
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,REC_NBR)

         TYPE*,IAM(),' Agent #:',AGENT,' Ter #:',REC_NBR,' modified'
         NUMCHG=NUMCHG+1                                                        

         IF(GVT) THEN
            WRITE(REPLU,8001) C_ASFBYT(SAGNO:EAGNO),REC_NBR,
     *         C_ASFBYT(SSCLS:ESCLS),
     *         C_ASFBYT(SXSTN:EXSTN),
     *         C_ASFBYT(SXPRT:EXPRT),
     *         C_ASFBYT(SDROP:EDROP),
     *         C_ASFBYT(SGFID:EGFID),
     *         C_ASFBYT(SGSER:EGSER),
     *         C_ASFBYT(SGCKS:EGCKS)
         ELSE
            WRITE(REPLU,8002) C_ASFBYT(SAGNO:EAGNO),REC_NBR,
     *         C_ASFBYT(SSCLS:ESCLS),
     *         C_ASFBYT(SXSTN:EXSTN),
     *         C_ASFBYT(SXPRT:EXPRT),
     *         C_ASFBYT(SDROP:EDROP),
     *         C_ASFBYT(SGPHN:EGPHN),
     *         C_ASFBYT(SXADR:EXADR),
     *         C_ASFBYT(SXGRP:EXGRP)
         ENDIF 

      ENDIF                  
C                                                                               
      GOTO 300                                                                  
                                                                               
1000  CONTINUE                                                                  

      CALL CLOSEFIL(ASFFDB)                                                     
      CLOSE(UNIT=ASCII_LU)                                                     

      WRITE(5,9006)                                                             
      WRITE(5,9003) NUMCHG,   'Changed               '
      IF(ERRCNT.GT.0)THEN                                                       
        WRITE(5,9003) ERRCNT, 'Errors (Not Processed)'     
      ENDIF                                                                     
      NUMSUM = CTOI (ASCII_REC(2:5),K)
      IF(NUMSUM.NE.NUMCHG+ERRCNT) THEN
        WRITE(5,9003) NUMSUM, 'Number of agents in file of updates'
      ENDIF
      IF((NUMCHG).EQ.0) WRITE(5,9005)                                    
      WRITE(5,9006)                                                             

      CALL GSTOP(GEXIT_SUCCESS)                          
                                                                               
C     =================== Format Statements ================                    
                                                                               
901   FORMAT(5A4) 
902   FORMAT(A4)
903   FORMAT(A2)
904   FORMAT(A5)
9000  FORMAT(1X,129('='),/)                                                     
9001  FORMAT(' ASF UPDATE REPORT (',A10,',',A8,')')
9003  FORMAT(1X,I5,T21,'Record(s) ',A40) 
9005  FORMAT(/,1X,'ASFX2XUPD: There have been no updates today',/)
9006  FORMAT(1X,70('='))                            
8000  FORMAT(20X,'Agent      Term   Stn cls   Stn num  Port stn  Drop'
     *          '   GVT phone     X2X address/GVT id   Rel grp') 
8001  FORMAT(20X,A7,3X,I5,6X,A2,6X,A5,5X,A5,4X,A2,4X,12X,4X,
     *       A2,1X,A12,1X,A2)
8002  FORMAT(20X,A7,3X,I5,6X,A2,6X,A5,5X,A5,4X,A2,4X,A12,4X,A16,4X,A3)
                                                                               
      END  
                                                                               
C==============================================================

      INTEGER*4 FUNCTION  NUMEXCL(STRING)
      IMPLICIT NONE

      CHARACTER   STRING*(*)

      INTEGER*4   NUMSP
      INTEGER*4   K,STRLEN

      STRLEN = LEN(STRING)
      NUMSP = 0

      DO 100 K = 1,STRLEN
         IF(STRING(K:K).EQ.' ') THEN
            NUMSP = NUMSP + 1
         ELSE
            GOTO 200
         ENDIF
100   CONTINUE
200   CONTINUE
      IF(NUMSP.LT.STRLEN) THEN 
         NUMEXCL = NUMSP
      ELSE
         NUMEXCL = 0
      ENDIF
      RETURN
      END

