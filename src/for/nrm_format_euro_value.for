C
C FORMAT_EURO_VALUE.FOR                                                                    
C
C V02 25-SEP-2001 EPH OPERATE WITH DOUBLE PRECISION DURING DIVISIONS BY FIXED VALUES
C V01 01-MAY-2001 EPH INITIAL VERSION FOR SCML
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
C Copyright 2001 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                  
C	RECEIVES VALUE IN EUROS AND FORMATS IN EUROS OR CONVERTS AND FORMAT IT IN ESCUDOS
C                                                             
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
C       ******************************************************************
        CHARACTER*18 FUNCTION FORMAT_EURO_VALUE (VALOR_EM_EUROS_AUX, CURRENCY)
C       ******************************************************************
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

        INTEGER*4   VALOR_EM_EUROS, VALOR_EM_EUROS_AUX
        CHARACTER*3 CURRENCY        !CURRENCY TO FORMAT IN ('Esc'=ESCUDOS / 'Eur'=EUROS)

        INTEGER*4   INT_PART, FRAC_PART
        INTEGER*4   GRP3DIG_1, GRP3DIG_2, GRP3DIG_3
        CHARACTER*1 FRAC_SEPARATOR
        INTEGER*4   I,J
	LOGICAL     NEG

	NEG = .FALSE.

	VALOR_EM_EUROS = VALOR_EM_EUROS_AUX
	IF (VALOR_EM_EUROS.LT.0) THEN
	   VALOR_EM_EUROS = VALOR_EM_EUROS*-1
	   NEG = .TRUE.  
	ENDIF

        IF (CURRENCY.EQ.'Eur' .OR. CURRENCY.EQ.'EUR') THEN
           INT_PART  = INT (VALOR_EM_EUROS/100.0D0)
           FRAC_PART = VALOR_EM_EUROS - (INT_PART*100)
           FRAC_SEPARATOR = ','
        ELSE
           INT_PART  = IDNINT((DFLOAT(VALOR_EM_EUROS)/100.00D00)*200.482)     
           FRAC_PART = 0
           FRAC_SEPARATOR = '$'
        ENDIF

        GRP3DIG_1 = INT (INT_PART/1000000.0D00)
        GRP3DIG_2 = INT (INT_PART/1000.0D00) - (GRP3DIG_1*1000)
        GRP3DIG_3 = INT_PART - (INT (INT_PART/1000.0D00) * 1000)

        WRITE(FORMAT_EURO_VALUE,20) GRP3DIG_1, '.', GRP3DIG_2, '.', GRP3DIG_3, FRAC_SEPARATOR, FRAC_PART, ' ', CURRENCY
20      FORMAT(I3.3,A1,I3.3,A1,I3.3,A1,I2.2,A1,A3)

        I = 1
        DO WHILE ( (FORMAT_EURO_VALUE(I:I).LT.'1' .OR. FORMAT_EURO_VALUE(I:I).GT.'9') .AND. I.LE.11)
           FORMAT_EURO_VALUE(I:I) = '*'
           I = I + 1
        ENDDO        

	IF (NEG) THEN
	    DO J=18,1,-1
		IF (FORMAT_EURO_VALUE(J:J).EQ.'*') THEN
		   FORMAT_EURO_VALUE(J:J) = '-'
		   EXIT
		ENDIF
	    ENDDO
	ENDIF

        RETURN
        END
