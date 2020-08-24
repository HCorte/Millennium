C V01 19-DEZ-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C ALLOWS TO TAKE OFFLINE ORDERS DOUBLED IN THE OPS FILE FOR WEEK 200127
C THE LOW ORDERS DON'T HAVE BANK AND THE HI ORDERS WERE CHOOSEN
C RANDOMLY TO BE SENT 
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
        PROGRAM FIXOPS
        IMPLICIT NONE                                                  
	                                                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'
C                                                                       
        INTEGER*4   MSG_LUN /6/
	INTEGER*4   ST, SZ, I
C
	CHARACTER*14 CHAVE
C
	INTEGER*4 MAXJOGOS 
	PARAMETER (MAXJOGOS=4)

	INTEGER*4 JOGO(MAXJOGOS) /1,2,4,6/   

	INTEGER*4 INIORD(MAXJOGOS) /5983,406886,405191,3217/
	INTEGER*4 FIMORD(MAXJOGOS) /6006,408307,406304,3230/
     
	CHARACTER*14  BILHETEDEL(1000)

	INTEGER*4 BDEL, BLOWDEL, JG, K, J, ORD

	LOGICAL FOUND

	CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-------------------------------------------------'   
        TYPE*,IAM(),'<<<<< SELECCAO E ELIMINACAO DAS ORDENS DUPLICADAS'
        TYPE*,IAM(),'      LEVANDO EM CONTA OS PAGAMENTOS        >>>>>'   
        TYPE*,IAM(),'-------------------------------------------------'
        TYPE*,IAM(),' '
	
C	OPEN ORDENS DE PAGAMENTO FILE
C     	=============================
     	CALL OPEN_OPS('KEYED',ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error Opening Order file', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
	ENDIF

        DO J = 1,MAXJOGOS

           JG = JOGO(J)

           DO I=1,1000
              BILHETEDEL(I) = '              '
           ENDDO

           BDEL    = 0
           BLOWDEL = 0

           DO ORD = INIORD(J), FIMORD(J)

              WRITE(CHAVE,FMT='(I2.2,A6,I6.6)') JG, '200127', ORD
	      READ(OPS_LUN, KEYID=0, KEYEQ=CHAVE, IOSTAT=ST) OPS_REC
              IF (ST.NE.0) THEN
                 CALL DISPERR (MSG_LUN, 'Error READING OPS FILE RECORD', 0, 'STATUS = ', ST, ' ', 0)
                 STOP
              ENDIF

              IF ( (.NOT. OPS_REC.HI_PRIZE) .AND. OPS_REC.PAID_CDC .EQ.0 .AND. CTOI(OPS_REC.BANK,SZ).EQ.0) THEN
                 BLOWDEL = BLOWDEL + 1

                 DELETE(OPS_LUN,IOSTAT=ST)
                 IF (ST.NE.0) THEN
                    CALL DISPERR (MSG_LUN, 'Error DELETING OPS FILE RECORD', 0, 'STATUS = ', ST, ' ', 0)
                    STOP
                 ENDIF
              ENDIF	         
               
              IF (OPS_REC.HI_PRIZE .AND. OPS_REC.PAID_CDC .EQ.0) THEN
                 FOUND = .FALSE.
                 DO K=1,BDEL
                    IF (BILHETEDEL(K).EQ.OPS_REC.BILHETE) THEN
                       FOUND = .TRUE.
                    ENDIF
                 ENDDO
              
                 IF (.NOT.FOUND) THEN
                    DELETE(OPS_LUN,IOSTAT=ST)
                    IF (ST.NE.0) THEN
                       CALL DISPERR (MSG_LUN, 'Error DELETING OPS FILE RECORD', 0, 'STATUS = ', ST, ' ', 0)
                       STOP
                    ENDIF
                    BDEL = BDEL + 1
                    BILHETEDEL(BDEL) = OPS_REC.BILHETE
                 ENDIF
              ENDIF

           ENDDO

	   TYPE*,'JOGO =',JG, '         LOW ORDERS =', BLOWDEL, '         HI  ORDERS =', BDEL 

        ENDDO

 	close (OPS_LUN)
          
 	STOP
	END


	  
