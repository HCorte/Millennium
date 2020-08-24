C
C NRM_SHOW_OP.FOR                                                                    
C
C
C V03 24-DEC-2010 FRP Lotto2 Changes
C V02 19-JUN-2001 EPH FIX SOME FORMATS 
C V01 04-APR-2001 EPH INITIAL VERSION FOR SCML
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
      SUBROUTINE SHOW_OP 
      IMPLICIT NONE                                                  
                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF' 
      INCLUDE 'INCLIB:DATBUF.DEF' 
      INCLUDE 'INCLIB:CONCOM.DEF' 
      INCLUDE 'INCLIB:OPS_REC.DEF'

	CHARACTER*10 DATE1, DATE2, DATE3
	CHARACTER*8  IAGT_NO      !FUNCTION
	INTEGER*4    SZ
	CHARACTER*4  GAM_NAME
	INTEGER*2    DATE(12)

	CHARACTER*3  SIMNAO   !FUNCTION	

	CHARACTER*14 MANUALPAY

	WRITE(GAM_NAME,FMT='(A4)') GSNAMES(CTOI(OPS_REC.GAME,SZ))

        IF (OPS_REC.PROC_PAID_CDC.GT.0) THEN
	   DATE(VCDC) = OPS_REC.PROC_PAID_CDC
	   CALL LCDATE(DATE)
           WRITE(DATE1,FMT='(I2.2,A1,I2.2,A1,I4.4)') DATE(VDAY), '/', DATE(VMON), '/', 2000+DATE(VYEAR)
        ELSE
           DATE1 = '00/00/0000'
        ENDIF

        IF (OPS_REC.PAYABLE_CDC.NE.0) THEN
	   DATE(VCDC) = OPS_REC.PAYABLE_CDC
	   CALL LCDATE(DATE)
           WRITE(DATE2,FMT='(I2.2,A1,I2.2,A1,I4.4)') DATE(VDAY), '/', DATE(VMON), '/', 2000+DATE(VYEAR)
        ELSE
           DATE2 = '00/00/0000'
        ENDIF

        IF (OPS_REC.PAID_CDC.NE.0) THEN
	   DATE(VCDC) = OPS_REC.PAID_CDC
	   CALL LCDATE(DATE)
           WRITE(DATE3,FMT='(I2.2,A1,I2.2,A1,I4.4)') DATE(VDAY), '/', DATE(VMON), '/', 2000+DATE(VYEAR)
        ELSE
           DATE3 = '00/00/0000'
        ENDIF

	IF (OPS_REC.PAID_CDC.NE.0 .AND. OPS_REC.PAID_MANUALLY) THEN
           MANUALPAY = ' -> MANUAL PAY'
        ELSE
           MANUALPAY = '              '
        ENDIF

	TYPE*,'================================================================================'
        TYPE*,'GAME             = ',OPS_REC.GAME, '  (',GAM_NAME,')'
        TYPE*,'DRAW             = ',OPS_REC.YEARWEEK(5:7),'/',OPS_REC.YEARWEEK(3:4)
        TYPE*,'ORDER            = ',OPS_REC.ORDER
        TYPE*,'BILETE           = ',OPS_REC.BILHETE
        TYPE*,'SPLIT FLAG       = ',SIMNAO(OPS_REC.SPLITTED)
        TYPE*,'AGENT            = ',IAGT_NO(CTOI(OPS_REC.AGENT,SZ))        !OPS_REC.AGENT
        TYPE*,'PRINTED_BY_OFF   = ',SIMNAO(OPS_REC.PRINTED_BY_OFF)
	TYPE*,'CLAIM            = ',SIMNAO(OPS_REC.CLAIM)
        TYPE*,'PAID OP PROC CDC = ',DATE1, '  ( CDC =', OPS_REC.PROC_PAID_CDC, ' )'
        TYPE*,'GENERATED        = ',SIMNAO(OPS_REC.GENERATED)
        TYPE*,'PAYABLE_CDC      = ',DATE2, '  ( CDC =', OPS_REC.PAYABLE_CDC, ' )'
        TYPE*,'PAID_CDC         = ',DATE3, '  ( CDC =', OPS_REC.PAID_CDC,' ) ' // MANUALPAY
        TYPE*,'PAID_SENT_SAP    = ',SIMNAO(OPS_REC.PAID_SENT_SAP)
        TYPE*,'ONLINE_ORDER     = ',SIMNAO(OPS_REC.ONLINE_ORDER)
        TYPE*,'HI_PRIZE         = ',SIMNAO(OPS_REC.HI_PRIZE)
	WRITE(6,19) OPS_REC.WINS(1), 
     *              OPS_REC.WINS(2), 
     *              OPS_REC.WINS(3), 
     *              OPS_REC.WINS(4), 
     *              OPS_REC.WINS(5), 
     *              OPS_REC.WINS(6), 
     *              OPS_REC.JOKER_DIV
19	FORMAT(1X, 'DIV 1    DIV 2    DIV 3    DIV 4    DIV 5    DIV 6      JOKER DIV',/,
     *         1X, I5, 4X, I5, 4X, I5, 4X, I5, 4X, I5, 4X, I5, 10X, I1)
        TYPE*,'TOTAL_GAME       = ',CMONY (OPS_REC.TOTAL_GAME, 12, VALUNIT)
        TYPE*,'TOTAL_JOKER      = ',CMONY (OPS_REC.TOTAL_JOKER, 12, VALUNIT)
        WRITE(6,18) OPS_REC.BANK, OPS_REC.BRANCH
18	FORMAT(1X,'BANK  =  ', A4, '    /    BRANCH  =  ', A4)
	TYPE*,'================================================================================'

	RETURN
	END


	CHARACTER*3 FUNCTION SIMNAO (LOGVAR)
	LOGICAL LOGVAR
	IF (LOGVAR) THEN
           SIMNAO = 'SIM'
        ELSE
           SIMNAO = 'NÃO'
        ENDIF
        RETURN
	END
