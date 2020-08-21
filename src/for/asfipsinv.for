C ASFIPSINV.for                                                            
C
C $Log:   GXAFIP:[GOLS]ASFIPSINV.FOV  $
C  
C     Rev 1.3   07 Apr 1997 16:13:32   HXK
C  Change to read IPS retailer field as this corresponds to
C  GOLS agent field
C  
C     Rev 1.2   07 Feb 1997 16:22:44   RXK
C  Fix for sales amount per game
C  
C     Rev 1.1   05 Feb 1997 17:41:48   RXK
C  File of instant game names created
C  
C     Rev 1.0   28 Jan 1997 18:16:54   RXK
C  Initial revision.
C  
C  
C PROGRAM TO UPDATE ASF.FIL WITH INSTANT TICKET INVOICE INFORMATION 
C AND INSNAM.FIL WITH INSTANT GAME NAMES 
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
      PROGRAM ASFIPSINV         
      IMPLICIT NONE                                                  
                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF' 
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:RECAGT.DEF'

C INPUT ASCII FILE PARAMETERS
C ---------------------------
      INTEGER*4    INPUT_REC_LEN
      PARAMETER(INPUT_REC_LEN=298)
      INTEGER*4	   INPUT_LU/7/
      CHARACTER    INPUT_REC*298   !  >= INPUT_REC_LEN       

C OUTPUT GAME NAME FILE PARAMETERS
C ---------------------------
      INTEGER*4    GN_REC_LEN
      PARAMETER(GN_REC_LEN=14)
      INTEGER*4	   GN_LU/10/

C ASF PARAMETERS
C --------------
      INTEGER*4    ASFFDB(7)

C LOCAL VARIABLE
C --------------
      INTEGER*4    NUMIT    
      INTEGER*4    ST
      INTEGER*4    RECN
      INTEGER*4    I,J
      INTEGER*4    K1,K2
      INTEGER*4    ERRCNT
      INTEGER*4    INVSUM(AITINVLEN)
      INTEGER*4    GAMSUM
      INTEGER*4    AGENT
      INTEGER*2    I2
      CHARACTER*2  B2
      EQUIVALENCE (B2,I2)           
      INTEGER*4    I4
      CHARACTER*4  B4
      EQUIVALENCE (B4,I4)
      CHARACTER    CZERO*1/Z0/
      CHARACTER*10 C10

      INTEGER*4    REC,REC_NBR
      INTEGER*4    LOOKUP(NUMAGT)  !contains Agent # only.

      INTEGER*4   FILNAME(10)            
      CHARACTER   CFILNAME(40)            
      EQUIVALENCE (CFILNAME,FILNAME)


C CALL  COPYRITE  SUBROUTINE                                                    
C --------------------------
      CALL COPYRITE                                                             
                                                                               
      TYPE *                                                                    
      TYPE *,'<<<<< ASFIPSINV Instant Ticket Invoice Update V01 >>>>>'
      TYPE *                                                                    

      CALL FASTSET(0,INVSUM,AITINVLEN)
      GAMSUM = 0
      NUMIT = 0
      ERRCNT = 0
      ST = 0
      RECN = 0

C OPEN INPUT DISK FILE           
C ---------------------                                            

100   CONTINUE                                                                  
      CALL WIMG(5,'Enter file name (VOLN:FILNAME)        ')
      READ(5,9000) FILNAME
      IF (FILNAME(1).EQ.'    ') GOTO 100
      IF(CFILNAME(1).EQ.'E '.OR.CFILNAME(1).EQ.'e ') 
     *   CALL GSTOP(GEXIT_OPABORT)
      OPEN(UNIT=INPUT_LU,
     *     FILE=FILNAME,
     *     IOSTAT=ST,
     *     STATUS='OLD',  
     *     ORGANIZATION='SEQUENTIAL',
     *     ACCESS='SEQUENTIAL')
      IF(ST.NE.0) THEN
         TYPE*,IAM(),CFILNAME,' Open error, status =',ST
         CALL USRCLOS1(INPUT_LU)
         GOTO 100
      ENDIF
C
C OPEN FILE OF INSTANT GAME NAMES  
C --------------------------------
        CALL DFILX('FILE:INSNAM.FIL',0,0,ST)
        OPEN (UNIT=GN_LU, FILE='FILE:INSNAM.FIL', IOSTAT=ST,
     *       STATUS='NEW', DISP='KEEP', ACCESS='SEQUENTIAL',
     *       FORM='FORMATTED', RECORDTYPE='FIXED',
     *       RECL=GN_REC_LEN, CARRIAGECONTROL='LIST')
        IF(ST.NE.0) THEN
           TYPE*,IAM(),'FILE:INSNAM.FIL Open error  st - ',ST
           CALL USRCLOS1(10)
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C OPEN AGENT SALES FILE                                                         
C ---------------------                                                    
      CALL OPENW (ASF,SFNAMES(1,ASF),4,0,0,ST)
      IF(ST.NE.0) CALL FILERR (SFNAMES(1,ASF),1,ST,0)
 
      CALL IOINIT(ASFFDB,ASF,ASFSEC*256)  ! in bytes, not in sectors
C
      DO 200 I=1,NUMAGT
         IF (MOD(I-1,1000).EQ.0) THEN
            TYPE*,' Building lookup table in progress...',I
         ENDIF

         CALL READW(ASFFDB,I,ASFREC,ST)
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,I)
C
C CLEAR PREVIOUS WEEK INVOICE
C ----------------------------
         CALL FASTSET(0,ASFITINV(1),AITINVLEN)
         CALL FASTSET(0,ASFITGSAL(1,1),2*AITGAM)
         CALL WRITEW (ASFFDB,I,ASFREC,ST)                         
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,I)
C
C CREATE AGENT NUMBERS' TABLE
C ---------------------------
         DO J=SAGNO,EAGNO
            IF(ASFBYT(J).NE.' '.AND.ASFBYT(J).NE.CZERO) THEN
               CALL ASCBIN(ASFINF,SAGNO,LAGNO,LOOKUP(I),ST)
               GOTO 200
            ENDIF
         END DO

200    CONTINUE

      TYPE *,IAM(),'Lookup table completed '
      TYPE *,IAM(),' '

C READ INPUT INVOICE FILE
C -----------------                                  

300   CONTINUE                                                                  
      RECN = RECN + 1 
      READ(UNIT=INPUT_LU,                          
     *        IOSTAT=ST,
     *        END=1000,
     *        FMT='(<INPUT_REC_LEN>A)') INPUT_REC
      IF(ST.NE.0) THEN
	 CALL FILERR(FILNAME,2,ST,RECN)
         CALL USRCLOS1(INPUT_LU)
         CALL GSTOP (GEXIT_FATAL)
       ENDIF

C READ ASF AND UPDATE INSTANT TICKET INVOICE FIELDS
C ---------------------------------------------------

      K1=1
      K2=K1+1
      READ(INPUT_REC(K1:K2),9001) B2        !record type
      IF(I2.EQ.1) GOTO 600
      IF(I2.NE.2) GOTO 300
   
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4        !IPS retailer <=> GOLS agent
      AGENT = I4
      REC_NBR = 0
      DO REC=1,NUMAGT
         IF (AGENT.EQ.LOOKUP(REC)) THEN
             REC_NBR = REC
             GOTO 400
         ENDIF
      ENDDO

400   CONTINUE

      IF(REC_NBR.NE.0) THEN
         CALL READW (ASFFDB,REC_NBR,ASFREC,ST)
         IF(ST.NE.0) THEN 
            CALL FILERR(SFNAMES(1,ASF),2,ST,REC_NBR)
            TYPE*,' Ter #:',REC_NBR,' *** not found ***'
            ERRCNT = ERRCNT + 1                      
         GOTO 300
         ENDIF
      ELSE
            TYPE*,' Agent #:',agent,' *** not found ***'
            ERRCNT = ERRCNT + 1                      
         GOTO 300
      ENDIF

      K1=K2+1
      K2=K1+3
C     READ(INPUT_REC(K1:K2),9002) B4        !agent, not used
      K1=K2+1
      K2=K1+3
C     READ(INPUT_REC(K1:K2),9002) B4               ! NOTUSED
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITSCNT) = I4
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4 
      ASFITINV(ASFITSAMT) = I4
      INVSUM(ASFITSAMT) = INVSUM(ASFITSAMT) + ASFITINV(ASFITSAMT)
      ASFITINV(ASFITSAMT) = ASFITINV(ASFITSAMT) / DYN_BETUNIT
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITVCNT)=I4 
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITVAMT) = I4
      INVSUM(ASFITVAMT) = INVSUM(ASFITVAMT) + ASFITINV(ASFITVAMT)
      ASFITINV(ASFITVAMT) = ASFITINV(ASFITVAMT) / DYN_VALUNIT
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITBCNT) = I4
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITBAMT) = I4
      INVSUM(ASFITBAMT) = INVSUM(ASFITBAMT) + ASFITINV(ASFITBAMT)
      ASFITINV(ASFITBAMT) = ASFITINV(ASFITBAMT) / DYN_VALUNIT
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITRAMT) = I4
      INVSUM(ASFITRAMT) = INVSUM(ASFITRAMT) + ASFITINV(ASFITRAMT)
      ASFITINV(ASFITRAMT) = ASFITINV(ASFITRAMT) / DYN_BETUNIT
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITSCM) = I4
      INVSUM(ASFITSCM) = INVSUM(ASFITSCM) + ASFITINV(ASFITSCM)
      ASFITINV(ASFITSCM) = ASFITINV(ASFITSCM) / DYN_BETUNIT
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITPCM) = I4
      ASFITINV(ASFITPCM) = ASFITINV(ASFITPCM) / DYN_BETUNIT
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITADJ) = I4
      INVSUM(ASFITADJ) = INVSUM(ASFITADJ) + ASFITINV(ASFITADJ)
      ASFITINV(ASFITADJ) = ASFITINV(ASFITADJ) / DYN_BETUNIT
      K1=K2+1
      K2=K1+3
      READ(INPUT_REC(K1:K2),9002) B4
      ASFITINV(ASFITDUE) = I4
      INVSUM(ASFITDUE) = INVSUM(ASFITDUE) + ASFITINV(ASFITDUE)
      ASFITINV(ASFITDUE) = ASFITINV(ASFITDUE) / DYN_BETUNIT
      DO I=1,AITGAM
         K1=K2+1
         K2=K1+1
         READ(INPUT_REC(K1:K2),9001) B2
         ASFITGSAL(AITGNUM,I) = I2
         IF(ASFITGSAL(AITGNUM,I).EQ.0) GOTO 500
         K1=K2+1
         K2=K1+1
C        READ(INPUT_REC(K1:K2),9001) B2      ! NOTUSED
         K1=K2+1
         K2=K1+3
         READ(INPUT_REC(K1:K2),9002) B4
         ASFITGSAL(AITGAMT,I) = I4
         GAMSUM = GAMSUM + ASFITGSAL(AITGAMT,I)
         ASFITGSAL(AITGAMT,I) = ASFITGSAL(AITGAMT,I) / DYN_BETUNIT
         K1=K2+1
         K2=K1+3
C        READ(INPUT_REC(K1:K2),9002) B4      ! NOTUSED
      ENDDO      

500   CONTINUE
      CALL WRITEW (ASFFDB,REC_NBR,ASFREC,ST)                         
      IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,REC_NBR)

      NUMIT=NUMIT+1                                                        

      GOTO 300                                                                  
C
C READ GAME NAMES 
C
600   CONTINUE

      READ(INPUT_REC(3:4),9001) B2
      READ(INPUT_REC(25:34),9007) C10
      WRITE(GN_LU,9008) I2,C10
      GOTO 300
                                   
1000  CONTINUE       

      CALL CLOSEFIL(ASFFDB)
      CALL USRCLOS1(GN_LU)
      CALL USRCLOS1(INPUT_LU)
                                                     
      WRITE(5,9006)                                                             
      WRITE(5,9003) NUMIT,   'Updated               '
      IF(ERRCNT.GT.0)THEN                                                       
        WRITE(5,9003) ERRCNT, 'Errors (Not Processed)'     
      ENDIF                                                                     
      IF((NUMIT).EQ.0) THEN
         WRITE(5,9005)                                    
      ELSE
         TYPE*
         WRITE(5,9006) 'Total sales', DFLOAT(INVSUM(ASFITSAMT))/100.D0
         WRITE(5,9006) 'Total cashes', DFLOAT(INVSUM(ASFITVAMT))/100.D0
         WRITE(5,9006) 'Total to bank', DFLOAT(INVSUM(ASFITBAMT))/100.D0
         WRITE(5,9006) 'Total returns', DFLOAT(INVSUM(ASFITRAMT))/100.D0
         WRITE(5,9006) 'Total sales comm.', DFLOAT(INVSUM(ASFITSCM))/100.D0
         WRITE(5,9006) 'Total adjustments', DFLOAT(INVSUM(ASFITADJ))/100.D0
         WRITE(5,9006) 'Total due',DFLOAT(INVSUM(ASFITDUE))/100.D0
         IF(INVSUM(ASFITSAMT).NE.GAMSUM) THEN
            TYPE*
            TYPE*,' Attension! Compare sales with IPS system'
            TYPE*
         ENDIF  
      ENDIF

      CALL GSTOP(GEXIT_SUCCESS)                          
                                                                               
C     =================== Format Statements ================                    
                                                                               
9000  FORMAT(10A4) 
9001  FORMAT(A2)
9002  FORMAT(A4)
9003  FORMAT(12X,I5,T21,'Record(s) ',A) 
9005  FORMAT(/,1X,'ASFIPSINV: There have been no updates today',/)
9006  FORMAT(1X,'ASFIPSINV:',T21,A,T40,F14.2)
9007  FORMAT(A10)
9008  FORMAT(I4,A10)
C
      END
