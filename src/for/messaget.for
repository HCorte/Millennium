C
C LIBRARY MESSAGET
C
C SUBSTITUTION FOR MESSAGEQ
C
C V01 17-MAR-09 TRG RELEASED FOR SCML
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
C
C
C
C **************************
C
C SUBROUTINE MESSQ_ATTACH
C
C **************************
C         
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE  MESSQ_ATTACH(ST)
	IMPLICIT NONE      

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
        INCLUDE 'INCLIB:MESSAGET.DEF'
        
        INTEGER*4  ST,STS
   
        INP_MESSAGEQ_CNT = 0 
        CALL OPSTXT('***********************************************')    
        CALL OPSTXT('* Warning  Warning  Warning  Warning  Warning *')                
        CALL OPSTXT('*                                             *')
        CALL OPSTXT('* Test version of COMMGR with EUROMILLION Sim *')      
        CALL OPSTXT('*                                             *')
        CALL OPSTXT('* Warning  Warning  Warning  Warning  Warning *')                
        CALL OPSTXT('***********************************************')      
        CALL CHECK_SIM_DATA(1, STS)
        IF(STS .NE. 1) THEN
         CALL OPSTXT('Error Loading simfile !!!!!')              	 
        ENDIF         
        EURTIMERTOP = -1
        EURTIMERBOT = -1
        EURSERIALNUM = 0
        CALL FASTSET(0, EURTIMERLIST, NUMPRO+2)        
        
C        CALL TEST_SIM
            
        ST = PAMS__SUCCESS
        
        END 

C
C **************************
C
C SUBROUTINE MESSQ_PUT
C
C **************************
C         
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE  MESSQ_PUT(ST)
	IMPLICIT NONE      

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
        INCLUDE 'INCLIB:MESSAGET.DEF'

        COMMON /TO_EUROMIL/MESS_TO_EUROMIL,MESS_TO_LEN
        BYTE MESS_TO_EUROMIL(1024)
        INTEGER*4 MESS_TO_LEN
C        
C
C Parameter
C        
        INTEGER*4   ST
           
        RECORD /QUEUE_MESSAGEQ/ TMPQ
   
        INTEGER*4   IND, I
        INTEGER*4   I4TEMP
        INTEGER*2   I2TEMP(2)
        BYTE        I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C Get Agent
C
        IND = 0
        I1TEMP(4) = ZEXT (MESS_TO_EUROMIL(IND+1))
        I1TEMP(3) = ZEXT (MESS_TO_EUROMIL(IND+2))
        I1TEMP(2) = ZEXT (MESS_TO_EUROMIL(IND+3))
        I1TEMP(1) = ZEXT (MESS_TO_EUROMIL(IND+4))
        TMPQ.AGT  = I4TEMP
C
C GET (5-8 BYTES) THE SEQUENCE NUMBER OF MESSAGEQ 
C                
        IND = 4
        I1TEMP(4) = ZEXT (MESS_TO_EUROMIL(IND+1))
        I1TEMP(3) = ZEXT (MESS_TO_EUROMIL(IND+2))
        I1TEMP(2) = ZEXT (MESS_TO_EUROMIL(IND+3))
        I1TEMP(1) = ZEXT (MESS_TO_EUROMIL(IND+4))
        TMPQ.MSQ  = I4TEMP
C
C GET (9-10 BYTES) THE BUFFER NUMBER OF MILLENNIUM
C        
        IND = 8
        I1TEMP(2) = ZEXT (MESS_TO_EUROMIL(IND+1))
        I1TEMP(1) = ZEXT (MESS_TO_EUROMIL(IND+2))
        TMPQ.BUF  = I4TEMP        
C
C GET (11-12 BYTES) CDC DATE OF MILLENNIUM
C        
        IND = 10
        I1TEMP(2) = ZEXT (MESS_TO_EUROMIL(IND+1))
        I1TEMP(1) = ZEXT (MESS_TO_EUROMIL(IND+2))
        TMPQ.CDC = I4TEMP
C
C GET (13-14 BYTES) TERMINAL NUMBER
C        
        IND = 12
        I1TEMP(2) = ZEXT (MESS_TO_EUROMIL(IND+1))
        I1TEMP(1) = ZEXT (MESS_TO_EUROMIL(IND+2))
        TMPQ.TER  = I4TEMP
C
C PUT (15-MESSLEN BYTES) THE MESSAGE FROM ALTURA
C        
        TMPQ.ILN = MESS_TO_LEN - IND
        IND = 14
        DO I=1,TMPQ.ILN
         TMPQ.MSI(I) = ZEXT (MESS_TO_EUROMIL(IND+I))
        ENDDO
        TMPQ.OLN = 0
C
C PUT MESSAGE INTO BUFFER
C                
        INP_MESSAGEQ_CNT = INP_MESSAGEQ_CNT + 1
        INP_MESSAGEQ(INP_MESSAGEQ_CNT) = TMPQ
C        
C SOME TYPES
C        
C        TYPE *,'***********************'
C        TYPE *,' Input '
C        TYPE *,'***********************'
C        TYPE *,'CNT:',INP_MESSAGEQ_CNT
C        TYPE *,'TER:',TMPQ.TER
C        TYPE *,'AGT:',TMPQ.AGT
C        TYPE *,'CDC:',TMPQ.CDC
C        TYPE *,'BUF:',TMPQ.BUF
C        TYPE *,'MSQ:',TMPQ.MSQ
C        TYPE *,'ILN:',TMPQ.ILN
C        CALL PRINTMSG(TMPQ.MSI, TMPQ.ILN)
C        TYPE *,'MSG:',TMPQ.MSI(1),' ',TMPQ.MSI(2),' ',TMPQ.MSI(3),' ',TMPQ.MSI(4)
C        TYPE *,'***********************'
C        
C SAY EVERITHING IS OK
C        
        ST = PAMS__SUCCESS
        
        END 
C
C **************************
C
C SUBROUTINE MESSQ_GET
C
C **************************
C         
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE  MESSQ_GET(ST)
	IMPLICIT NONE      

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
        INCLUDE 'INCLIB:MESSAGET.DEF'
        
        INTEGER*4  ST               
        
        INTEGER*4  I, IDX        
        RECORD /QUEUE_MESSAGEQ/ TMPQ        

        COMMON /FROM_EUROMIL/MESS_FROM_EUROMIL,MESS_FROM_LEN
        BYTE MESS_FROM_EUROMIL(1024)!array of the message with 1024 bytes of size
        INTEGER*4 MESS_FROM_LEN

        
        INTEGER*4   I4TEMP
        INTEGER*2   I2TEMP(2)
        BYTE        I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        
        !?????onde é que está a ser preenchido esta variavel INP_MESSAGEQ_CNT e o primeiro objecto do array/queue? INP_MESSAGEQ
        IF(INP_MESSAGEQ_CNT .EQ. 0) THEN                	
         ST = PAMS__NOMOREMSG    ! No Message
         RETURN
        ENDIF  
C        
C Build Response
C
        CALL BLDRESPONSE
        TMPQ = INP_MESSAGEQ(1)           
C
C Send Response Back
C
        IDX = 1
C
C Agent
C        
        I4TEMP = TMPQ.AGT  !relembrar que o I4TEMP é equivalente ao array de 4 de I1TEMP -> I1TEMP(4) linha 209       
        MESS_FROM_EUROMIL(IDX)    = I1TEMP(4)
        MESS_FROM_EUROMIL(IDX+1)  = I1TEMP(3)
        MESS_FROM_EUROMIL(IDX+2)  = I1TEMP(2)
        MESS_FROM_EUROMIL(IDX+3)  = I1TEMP(1)
        IDX = IDX + 4
C
C  MESSAGEQ REFERENCE NUMBER
C
        I4TEMP = TMPQ.MSQ
        MESS_FROM_EUROMIL(IDX)    = I1TEMP(4)
        MESS_FROM_EUROMIL(IDX+1)  = I1TEMP(3)
        MESS_FROM_EUROMIL(IDX+2)  = I1TEMP(2)
        MESS_FROM_EUROMIL(IDX+3)  = I1TEMP(1)
        IDX = IDX + 4
C        
C MIL. BUFFER
C
        I4TEMP = TMPQ.BUF         
        MESS_FROM_EUROMIL(IDX)    =  I1TEMP(2)
        MESS_FROM_EUROMIL(IDX+1)  =  I1TEMP(1)
        IDX = IDX + 2
C        
C MIL. CDC ?????
C
        I4TEMP = TMPQ.CDC       
        MESS_FROM_EUROMIL(IDX)    =  I1TEMP(2)
        MESS_FROM_EUROMIL(IDX+1)  =  I1TEMP(1)
        IDX = IDX + 2
C        
C MIL. Terminal
C
        I4TEMP = TMPQ.TER         
        MESS_FROM_EUROMIL(IDX)    =  I1TEMP(2)
        MESS_FROM_EUROMIL(IDX+1)  =  I1TEMP(1)
        IDX = IDX + 2
C
C PUT (15-MESSLEN BYTES) THE MESSAGE TO ALTURA
C        
        DO I=1,TMPQ.OLN
         MESS_FROM_EUROMIL(IDX)   =  TMPQ.MSO(I)
         IDX = IDX + 1
        ENDDO  
        MESS_FROM_LEN = IDX - 1
C
C Get The Buffer and release the Message Queue
C        
        DO I=2, INP_MESSAGEQ_CNT
         INP_MESSAGEQ(I-1) = INP_MESSAGEQ(I)       	
        ENDDO	
        INP_MESSAGEQ_CNT = INP_MESSAGEQ_CNT - 1
        IF(INP_MESSAGEQ_CNT .LT. 0) INP_MESSAGEQ_CNT = 0                
C
C Say Everithing is Ok.
C                                             
        ST = PAMS__SUCCESS
        
        END 

C
C **************************
C
C SUBROUTINE MESSQ_EXIT
C
C **************************
C         
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE  MESSQ_EXIT(ST)
	IMPLICIT NONE      

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
        
        INTEGER*4  ST
        
        ST = PAMS__SUCCESS
        
        END 

C
C **************************
C
C SUBROUTINE BLDRESPONSE
C
C **************************
C         
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE  BLDRESPONSE
	IMPLICIT NONE      

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
        INCLUDE 'INCLIB:MESSAGET.DEF'
        
        RECORD /QUEUE_MESSAGEQ/ TMPQ 
        RECORD /GAM_SAL_STRUCT/ GAMSAL(20), GEM(10)
        INTEGER*4   GAMSALCNT         
        
        INTEGER*4 INPMODE, MTYPE, TYPSB, I, J, IDX, EUMDAT, EMCNT
        INTEGER*4 TCNT, TAMT
 
        INTEGER*4   I4TEMP
        INTEGER*2   I2TEMP(2)
        BYTE        I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

        
        TMPQ = INP_MESSAGEQ(1)           

C
C Get From Sim File
C
        INPMODE = 0
        CALL SELECT_RESPONSE(TMPQ.MSI, TMPQ.MSO, TMPQ.OLN, INPMODE)
                 
        TYPSB =ZEXT(TMPQ.MSO(2))
        MTYPE = ISHFT(TYPSB,-4)

C        TYPE *,'MTYPE:',MTYPE 
       
        IF(MTYPE .EQ. 0) THEN                            ! WAGER  
         CALL SETSERIALNUM(7, TMPQ.MSO, EURSERIALNUM) 	
         CALL SETTIME(19, TMPQ.MSO, P(ACTTIM))
        ELSE IF(MTYPE .EQ. 1) THEN	                 ! VALIDATION
         CALL SETTIME(5, TMPQ.MSO, P(ACTTIM))          	 
         CALL SETSERIALNUM(8, TMPQ.MSO, EURSERIALNUM) 	 
        ELSE IF(MTYPE .EQ. 2) THEN	                 ! CANCEL 
         CALL SETTIME(5, TMPQ.MSO, P(ACTTIM))          	 
         CALL SETSERIALNUM(8, TMPQ.MSO, EURSERIALNUM) 	 
        ENDIF	

        IF(TYPSB .EQ. '61'X) THEN                        ! Results Report  
         CALL SETTIME(5, TMPQ.MSO, P(ACTTIM))          	            
        ENDIF
        
        IF(TMPQ.MSI(2) .EQ. '63'X) THEN                  ! Financial reports  
         IF(TMPQ.MSO(2) .EQ. '90'X) THEN	         ! Summary, Sales, Prizes, Commisions
          TMPQ.OLN = TMPQ.ILN                            ! If anything found on file, take from Host
          DO I=1, TMPQ.OLN
           TMPQ.MSO(I) = TMPQ.MSI(I)
          ENDDO
         ELSE 

          IF(TMPQ.MSI(8) .EQ. 1) THEN          	          ! Class =1 Summary Rep
           IDX = 19                                       ! Just Add Data From EUM
           DO I=1,17
            I1TEMP(4) = ZEXT (TMPQ.MSO(IDX+0))
            I1TEMP(3) = ZEXT (TMPQ.MSO(IDX+1))
            I1TEMP(2) = ZEXT (TMPQ.MSO(IDX+2))
            I1TEMP(1) = ZEXT (TMPQ.MSO(IDX+3))
            EUMDAT = I4TEMP
            I1TEMP(4) = ZEXT (TMPQ.MSI(IDX+0))
            I1TEMP(3) = ZEXT (TMPQ.MSI(IDX+1))
            I1TEMP(2) = ZEXT (TMPQ.MSI(IDX+2))
            I1TEMP(1) = ZEXT (TMPQ.MSI(IDX+3))
            I4TEMP = I4TEMP + EUMDAT  
            TMPQ.MSI(IDX+0) = I1TEMP(4)
            TMPQ.MSI(IDX+1) = I1TEMP(3)
            TMPQ.MSI(IDX+2) = I1TEMP(2)
            TMPQ.MSI(IDX+3) = I1TEMP(1)                        	
            IDX = IDX + 4
           ENDDO
           TMPQ.OLN = TMPQ.ILN                            
           DO I=1, TMPQ.OLN
            TMPQ.MSO(I) = TMPQ.MSI(I)
           ENDDO           	           
          ENDIF

          IF((TMPQ.MSI(8) .EQ. 2) .OR.                    ! Sales, Prizes, Commisions
     *       (TMPQ.MSI(8) .EQ. 4) .OR.
     *       (TMPQ.MSI(8) .EQ. 8)) THEN 
     	   IDX = 18             
           GAMSALCNT = ZEXT (TMPQ.MSI(IDX))           
           
           IDX = IDX + 1
           DO I=1,GAMSALCNT
            GAMSAL(I).GTYP = TMPQ.MSI(IDX+0)
            GAMSAL(I).GIND = TMPQ.MSI(IDX+1)
            IDX = IDX + 2 	
           ENDDO	
           DO I=1,GAMSALCNT
            I1TEMP(4) = ZEXT (TMPQ.MSI(IDX+0))
            I1TEMP(3) = ZEXT (TMPQ.MSI(IDX+1))
            I1TEMP(2) = ZEXT (TMPQ.MSI(IDX+2))
            I1TEMP(1) = ZEXT (TMPQ.MSI(IDX+3))           	
            GAMSAL(I).CNT = I4TEMP
            IDX = IDX + 4
            I1TEMP(4) = ZEXT (TMPQ.MSI(IDX+0))
            I1TEMP(3) = ZEXT (TMPQ.MSI(IDX+1))
            I1TEMP(2) = ZEXT (TMPQ.MSI(IDX+2))
            I1TEMP(1) = ZEXT (TMPQ.MSI(IDX+3))           	            
            GAMSAL(I).AMT = I4TEMP
            IDX = IDX + 4 	
           ENDDO	
C
C Add Em Games
C
     	   IDX = 10             
     	   EMCNT = ZEXT (TMPQ.MSO(IDX))
           IDX = IDX + 1
           DO I=1,EMCNT
            GEM(I).GTYP = TMPQ.MSO(IDX+0)
            GEM(I).GIND = TMPQ.MSO(IDX+1)
            IDX = IDX + 2 	
           ENDDO	
           DO I=1,EMCNT
            I1TEMP(4) = ZEXT (TMPQ.MSO(IDX+0))
            I1TEMP(3) = ZEXT (TMPQ.MSO(IDX+1))
            I1TEMP(2) = ZEXT (TMPQ.MSO(IDX+2))
            I1TEMP(1) = ZEXT (TMPQ.MSO(IDX+3))           	
            GEM(I).CNT = I4TEMP
            IDX = IDX + 4
            I1TEMP(4) = ZEXT (TMPQ.MSO(IDX+0))
            I1TEMP(3) = ZEXT (TMPQ.MSO(IDX+1))
            I1TEMP(2) = ZEXT (TMPQ.MSO(IDX+2))
            I1TEMP(1) = ZEXT (TMPQ.MSO(IDX+3))           	            
            GEM(I).AMT = I4TEMP
            IDX = IDX + 4 	
           ENDDO	
C
C Mix
C           
           DO I=1,EMCNT
            DO J=1,GAMSALCNT	
             IF(GEM(I).GTYP .EQ.  GAMSAL(J).GTYP  .AND.
     *          GEM(I).GIND .EQ.  GAMSAL(J).GIND) THEN
               GAMSAL(J).CNT = GEM(I).CNT
               GAMSAL(J).AMT = GEM(I).AMT	
               IF(GAMSAL(J).GIND .EQ. 15) GAMSAL(J).GIND = 1	
             ENDIF        		     
            ENDDO 
           ENDDO	
C
C Rebuild Message
C          
     	   IDX = 18             
           TMPQ.MSI(IDX) = GAMSALCNT
           IDX = IDX + 1
           DO I=1,GAMSALCNT
            TMPQ.MSI(IDX+0) = GAMSAL(I).GTYP
            TMPQ.MSI(IDX+1) = GAMSAL(I).GIND            
            IDX = IDX + 2 	
           ENDDO	
           TCNT = 0
           TAMT = 0
           DO I=1,GAMSALCNT
            TCNT = TCNT + GAMSAL(I).CNT	
            I4TEMP = GAMSAL(I).CNT
            TMPQ.MSI(IDX+0) = I1TEMP(4)
            TMPQ.MSI(IDX+1) = I1TEMP(3)
            TMPQ.MSI(IDX+2) = I1TEMP(2)
            TMPQ.MSI(IDX+3) = I1TEMP(1)    
            IDX = IDX + 4

            TAMT = TAMT + GAMSAL(I).AMT
            I4TEMP = GAMSAL(I).AMT
            TMPQ.MSI(IDX+0) = I1TEMP(4)
            TMPQ.MSI(IDX+1) = I1TEMP(3)
            TMPQ.MSI(IDX+2) = I1TEMP(2)
            TMPQ.MSI(IDX+3) = I1TEMP(1)    
            IDX = IDX + 4
           ENDDO	
          
           TMPQ.OLN = TMPQ.ILN                           
           DO I=1, TMPQ.OLN
            TMPQ.MSO(I) = TMPQ.MSI(I)
           ENDDO           	
           
          IF(TMPQ.MSO(8) .EQ. 2 .OR.                       ! Sales And Prizes
     *       TMPQ.MSO(8) .EQ. 4) THEN
            IF(TMPQ.MSO(8) .EQ. 4) IDX = IDX + 8       
            I4TEMP = TCNT
            TMPQ.MSO(IDX+0) = I1TEMP(4)
            TMPQ.MSO(IDX+1) = I1TEMP(3)
            TMPQ.MSO(IDX+2) = I1TEMP(2)
            TMPQ.MSO(IDX+3) = I1TEMP(1)    
            IDX = IDX + 4

            I4TEMP = TAMT/100            
            TMPQ.MSO(IDX+0) = I1TEMP(4)
            TMPQ.MSO(IDX+1) = I1TEMP(3)
            TMPQ.MSO(IDX+2) = I1TEMP(2)
            TMPQ.MSO(IDX+3) = I1TEMP(1)
            IDX = IDX + 4

            I4TEMP = TAMT
            TMPQ.MSO(IDX+4) = I1TEMP(4)
            TMPQ.MSO(IDX+5) = I1TEMP(3)
            TMPQ.MSO(IDX+6) = I1TEMP(2)
            TMPQ.MSO(IDX+7) = I1TEMP(1)
            IDX = IDX + 4
           ENDIF	 

          IF(TMPQ.MSO(8) .EQ. 8) THEN                      ! Commisions
            I4TEMP = TCNT
            TMPQ.MSO(IDX+0) = I1TEMP(4)
            TMPQ.MSO(IDX+1) = I1TEMP(3)
            TMPQ.MSO(IDX+2) = I1TEMP(2)
            TMPQ.MSO(IDX+3) = I1TEMP(1)    
            IDX = IDX + 4

            I4TEMP = TAMT                        
            TMPQ.MSO(IDX+0) = I1TEMP(4)
            TMPQ.MSO(IDX+1) = I1TEMP(3)
            TMPQ.MSO(IDX+2) = I1TEMP(2)
            TMPQ.MSO(IDX+3) = I1TEMP(1)
            IDX = IDX + 4
           ENDIF	            
          ENDIF
          
          CALL SETTIME(5, TMPQ.MSO, P(ACTTIM))
         ENDIF 
                  
        ENDIF 
        
C
C 
C
        CALL FILL_CHECKSUM(TMPQ.MSI, TMPQ.MSO, TMPQ.OLN)
        
c        TYPE *,'LEN:',TMPQ.OLN
c        CALL PRINTMSG(TMPQ.MSO, TMPQ.OLN)

        INP_MESSAGEQ(1) = TMPQ          
                
        END

