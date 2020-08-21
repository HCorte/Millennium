C AGTCPFLG.FOR
C
C Program to Enable / Disable Agent Game Wagering,Cancellations,Validations.
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C       OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM AGTCPFLG
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C
	INTEGER*4	NTER		!Loop Variable
	INTEGER*4	ERRO		!Subroutine Return Status.
	INTEGER*4	FFDB(7)		!File Descriptor block for ASF.
	INTEGER*4       BEGAAM          !BEG ACTIVE AM
	INTEGER*4       ENDAAM          !END ACTIVE AM
	INTEGER*4       BEGSAM          !BEG SUSPENSION AM
	INTEGER*4       ENDSAM          !END SUSPENSION AM	
	INTEGER*4       BEGAPA          !BEG ACTIVE Passive
	INTEGER*4       ENDAPA          !END ACTIVE Passive
	INTEGER*4       BEGSPA          !BEG SUSPENSION Passive
	INTEGER*4       ENDSPA          !END SUSPENSION Passive	
	CHARACTER*6     GET_DATE        !Function
C
        CHARACTER    CASFBYT*760
        EQUIVALENCE (CASFBYT,ASFBYT)    !ASFBYT is equivalent to ASFINF
	
C
	TYPE*,IAM()
	TYPE*,IAM(),'<<<<< AGTCPFLG Convert YYYYWW V01 >>>>>'
	TYPE*,IAM()
C
C GET SYSTEM CONFIGURATION INFO.
C
	CALL GETSCONF(SCFREC,ERRO)
	IF(ERRO.NE.0) THEN
	   TYPE*,IAM(),'Unable to get System Configuration info.'
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C OPEN THE AGENT SALES FILE
C
	CALL OPENW(ASF,SFNAMES(1,ASF),4,0,0,ERRO)
	CALL IOINIT(FFDB,ASF,ASFSEC*256)
	IF(ERRO.NE.0) CALL FILERR(SFNAMES(1,ASF),1,ERRO,0)
C
C LOOP FOR SELECTED RANGE OF TERMINALS
C
	DO NTER = 1,NUMAGT
	  CALL READW(FFDB,NTER,ASFREC,ERRO)
	  IF(ERRO.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ERRO,NTER)
C
C CONVERT OLD YYYYWW INTO DDMMYY FORMAT
C
          CALL ASCBIN(ASFINF,SWBSA,LWBSA,BEGAAM,ERRO)  
          CALL ASCBIN(ASFINF,SWESA,LWESA,ENDAAM,ERRO) 
          CALL ASCBIN(ASFINF,SWBSU,LWBSU,BEGSAM,ERRO)  
          CALL ASCBIN(ASFINF,SWESU,LWESU,ENDSAM,ERRO)  
          CALL ASCBIN(ASFINF,SPBSA,LPBSA,BEGAPA,ERRO)  
          CALL ASCBIN(ASFINF,SPESA,LPESA,ENDAPA,ERRO)                                   
          CALL ASCBIN(ASFINF,SPBSU,LPBSU,BEGSPA,ERRO)  
          CALL ASCBIN(ASFINF,SPESU,LPESU,ENDSPA,ERRO) 
C
          CASFBYT(SWBSA:EWBSA) = GET_DATE(BEGAAM)
          CASFBYT(SWESA:EWESA) = GET_DATE(ENDAAM)
          CASFBYT(SWBSU:EWBSU) = GET_DATE(BEGSAM)
          CASFBYT(SWESU:EWESU) = GET_DATE(ENDSAM)
          CASFBYT(SPBSA:EPBSA) = GET_DATE(BEGAPA)
          CASFBYT(SPESA:EPESA) = GET_DATE(ENDAPA)
          CASFBYT(SPBSU:EPBSU) = GET_DATE(BEGSPA)
          CASFBYT(SPESU:EPESU) = GET_DATE(ENDSPA)  
C
C         TYPE*,IAM(),NTER
C         TYPE*,IAM(),'BEGAAM: ',BEGAAM,' > ',CASFBYT(SWBSA:EWBSA)
C         TYPE*,IAM(),'ENDAAM: ',ENDAAM,' > ',CASFBYT(SWESA:EWESA)
C         TYPE*,IAM(),'BEGSAM: ',BEGSAM,' > ',CASFBYT(SWBSU:EWBSU)
C         TYPE*,IAM(),'ENDSAM: ',ENDSAM,' > ',CASFBYT(SWESU:EWESU)
C         TYPE*,IAM(),'BEGAPA: ',BEGAPA,' > ',CASFBYT(SPBSA:EPBSA)
C         TYPE*,IAM(),'ENDAPA: ',ENDAPA,' > ',CASFBYT(SPESA:EPESA)
C         TYPE*,IAM(),'BEGSPA: ',BEGSPA,' > ',CASFBYT(SPBSU:EPBSU)
C         TYPE*,IAM(),'ENDSPA: ',ENDSPA,' > ',CASFBYT(SPESU:EPESU)   
C                                
C WRITE RECORD BACK
C
	  CALL WRITEW(FFDB,NTER,ASFREC,ERRO)
	  IF(ERRO.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ERRO,NTER)
C
	ENDDO
C
        CALL CLOSEFIL(FFDB)
C        
        TYPE*,iam()
	TYPE*,IAM(),'Update Completed for Terminals '
        TYPE*,iam()
C        	
        CALL GSTOP(GEXIT_SUCCESS)
	END
C
C       FUNCTION THAT RETURNS A STRING DATE BASED ON WEEK
C
        CHARACTER*6 FUNCTION GET_DATE(YYYYWW)
C        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 YYYYWW
        INTEGER*4 YYYY
        INTEGER*4 WW
        INTEGER*4 JUL
        INTEGER*2 DATE(LDATE_LEN)   
C        
        GET_DATE = '000000'
        IF(YYYYWW.LE.0) RETURN
C
        WW   = MOD(YYYYWW,100)
        YYYY = YYYYWW/100
        IF(WW.LE.0) THEN
          JUL = 1
        ELSE
          JUL = (WW*7)-6
        ENDIF
C
        DATE(VJUL)=JUL
        DATE(VYEAR)=YYYY
        CALL JDATE(DATE)
C        
        WRITE(GET_DATE,'(I2.2,I2.2,I2.2)') DATE(VDAY),DATE(VMON),MOD(DATE(VYEAR),100)
C       
        RETURN           
C        
        END
