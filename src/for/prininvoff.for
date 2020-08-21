C
C PROGRAM PRININVOFF
C
C V01 07-JUL-2001 TRG INITIAL RELEASE FOR NETHERLANDS
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
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM PRININVOFF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C	
	INCLUDE 'INCLIB:HSHCOM.DEF'
C	
        INTEGER*4    ST,OPT,LINI,LEND,RETINI,RETEND,I	
        INTEGER*4    RETM(5000),NUMENTRY,INPLEN
        CHARACTER*30 CXFILE
C
        
        INTEGER*4    LU  /2/                   
        INTEGER*4    LUP /3/                           
C
        CALL COPYRITE
C
        TYPE *,IAM(),'This program prints the checks that'
        TYPE *,IAM(),'are on INVRPTOFF'
        TYPE *,IAM()
        TYPE *,IAM(),'Options' 
        TYPE *,IAM()                               
        TYPE *,IAM(),'1.- Print all agents'                        
        TYPE *,IAM(),'2.- Print a range of agents (From X To Y)'                        
        TYPE *,IAM(),'3.- Print some agents (X,Y,Z....)'                        
        TYPE *,IAM()                               
        CALL PRMNUM('Select the option:',OPT,1,3,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
C
C
        CALL PRMTEXT('Name of the file (INVRPTOFF.REP):',CXFILE,INPLEN)        
        IF(INPLEN.EQ.0) CXFILE(1:13) = 'INVRPTOFF.REP'
C        IF(INPLEN.EQ.0) CXFILE(1:12) = 'INVRPTOFF.45'
        IF(INPLEN.EQ.1) THEN
          IF((CXFILE(1:1).EQ.'e') .OR. (CXFILE(1:1).EQ.'E')) CALL GSTOP(GEXIT_OPABORT)	
        ENDIF	
C        CALL PRMTEXT('Name of the queue (PTSYSA_PRINTER):',PXFILE,INPLEN)        
C        IF(INPLEN.EQ.0) PXFILE(1:14) = 'PTSYSA_PRINTER'
C        IF(INPLEN.EQ.1) THEN
C          IF((PXFILE(1:1).EQ.'e') .OR. (PXFILE(1:1).EQ.'E')) CALL GSTOP(GEXIT_OPABORT)	
C        ENDIF	
        
        
        OPEN(UNIT=LU,
     *     FILE=CXFILE,
     *     IOSTAT=ST,
     *     STATUS='OLD',
     *     ORGANIZATION='SEQUENTIAL',
     *     ACCESS='SEQUENTIAL')
     
        IF(ST.NE.0) THEN
         TYPE *,IAM(),'ERROR OPENNING THE FILE'        	 
         CALL GSTOP(GEXIT_FATAL)
        ENDIF	
        
        OPEN(UNIT=LUP,
     *     FILE='SYSX:PRININVOFF.REP',
     *     IOSTAT=ST,
     *     STATUS='NEW')

        IF(ST.NE.0) THEN
         TYPE *,IAM(),'ERROR OPENNING THE PRINTER'        	 
         CALL GSTOP(GEXIT_FATAL)
        ENDIF	
                   
        GOTO(1000,2000,3000) OPT
C
C
1000    CONTINUE
        CALL GETLINES(LU,0,0,LINI,LEND)
        CALL PRINTLINES(LU,LUP,LINI,LEND)
        GOTO 5000        
C
C
2000    CONTINUE
        CALL PRMNUM('Enter First Retailer',RETINI,1,9999999,ST)
        IF(ST.NE.0) CALL OPABORT(LU,LUP)
        CALL PRMNUM('Enter  Last Retailer',RETEND,RETINI,9999999,ST)        
        IF(ST.NE.0) CALL OPABORT(LU,LUP)
        CALL GETLINES(LU,RETINI,RETEND,LINI,LEND)
        CALL PRINTLINES(LU,LUP,LINI,LEND)        
        GOTO 5000
C
C
3000    CONTINUE 
        NUMENTRY=0
3100    CONTINUE        
        CALL PRMNUM('Enter a Retailer E ends entry',RETM(NUMENTRY+1),1,9999999,ST) 
        IF(ST.NE.0) GOTO 3300
        NUMENTRY=NUMENTRY+1        
        GOTO 3100
3300    CONTINUE
        
        DO I=1,NUMENTRY
         CALL GETLINES(LU,RETM(I),RETM(I),LINI,LEND)
         CALL PRINTLINES(LU,LUP,LINI,LEND)        
        ENDDO 
        GOTO 5000        
C
C
C ALL ENDS COME HERE
C

5000    CONTINUE
        CLOSE(LU)
        CLOSE(LUP)
        CALL GSTOP(GEXIT_SUCCESS)
C
	END
C
C  SELECTS INITIAL AND ENDING LINE TO PRINT BASED ON RETAILER NUMBER
C
       SUBROUTINE GETLINES(LU,RETINI,RETEND,LINI,LEND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4  LU,RETINI,RETEND,LINI,LEND
        INTEGER*4  INPUT_REC_LEN,ST,A,B
        PARAMETER (INPUT_REC_LEN=150) 
        CHARACTER  BUF*150 
        
        REWIND(LU)
        LINI=0
        LEND=0
        
        IF(RETINI.EQ.0 .AND. RETEND.EQ.0) THEN
         LINI=1	 
100      CONTINUE        	
           READ(LU,END=200,ERR=200,IOSTAT=ST,FMT='(<INPUT_REC_LEN>A)') BUF                    	  
           IF(ST.NE.0) GOTO 200
           LEND=LEND+1
           GOTO 100
200      CONTINUE          
         RETURN
        ENDIF	

        IF(RETINI.EQ.RETEND) THEN
         CALL RETLINS(LU,RETINI,LINI,LEND)    	 
         RETURN
        ENDIF	
       
        CALL RETLINS(LU,RETINI,LINI,A)    	 
        CALL RETLINS(LU,RETEND,B,LEND)    	         
        RETURN

        END       
C
C  PRINTS THE SPECIFIED LINES TO THE PRINTER
C
        SUBROUTINE PRINTLINES(LU,LUP,LINI,LEND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4    LU,LUP,LINI,LEND               
        INTEGER*4    INPUT_REC_LEN,ST,LINE
        PARAMETER    (INPUT_REC_LEN=80) 
        CHARACTER    BUF*80 
        
        IF(LINI.EQ.0 .OR. LEND.EQ.0) RETURN
        IF(LINI.GT.LEND) RETURN
        
        REWIND(LU)               
       
        LINE = 0
100     CONTINUE        	
         READ(LU,END=200,ERR=200,IOSTAT=ST,FMT='(<INPUT_REC_LEN>A)') BUF                    	  
         IF(ST.NE.0) RETURN
         LINE = LINE + 1
         IF(LINE.GE.LINI) THEN
           WRITE(LUP,150) BUF          	
150        FORMAT(X,A80)           
         ENDIF	
         IF(LINE.GE.LEND) RETURN
        GOTO 100
200     CONTINUE        
C
        RETURN
        END               
C
C ROUTINE TO CLOSE FILE & EXIT
C        
        SUBROUTINE OPABORT(LU,LUP)        
        IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 LU,LUP
        
        CLOSE(LU)
        CLOSE(LUP)        
        
        CALL GSTOP(GEXIT_OPABORT)
        END        

C
C ROUTINE TO CLOSE FILE & EXIT
C        
        SUBROUTINE RETLINS(LU,RET,LINI,LEND)
        IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        CHARACTER*10 CXAGTP,CXAGT
        INTEGER*4    LU,RET,LINI,LEND,LINE               
        INTEGER*4    INPUT_REC_LEN,ST
        PARAMETER    (INPUT_REC_LEN=150) 
        CHARACTER    BUF*150 
        
        LINI = 0
        LEND = 0        
        
        WRITE(CXAGTP,50) RET
50      FORMAT(I7.7)        
        CXAGT(1:2) = CXAGTP(1:2)
        CXAGT(3:3) = '-'
        CXAGT(4:8) = CXAGTP(3:7)        
                
        REWIND(LU)

        LINE = 0
100     CONTINUE        	
         READ(LU,END=200,ERR=200,IOSTAT=ST,FMT='(<INPUT_REC_LEN>A)') BUF                    	  
         IF(ST.NE.0) GOTO 200
         LINE = LINE + 1
         IF(CXAGT(1:8).EQ.BUF(12:19)) THEN
          LINI=LINE-10
          LEND=LINE+13
         ENDIF	                   
         GOTO 100
200     CONTINUE          
        
        
        END
        
