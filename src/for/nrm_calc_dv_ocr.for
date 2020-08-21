C
C NRM_CAL_DV_OCR.FOR
C
C V01 04-APR-2001 EPH INITIAL RELEASE FOR PORTUGAL
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C       ************************************************************
	SUBROUTINE CALC_DV_OCR (OCR_LINE, DV)
C       ************************************************************
C       Routine to calculate 2 DIGIT DV from Payment Order OCR LINE
C       ************************************************************
	IMPLICIT NONE

        CHARACTER*29 OCR_LINE      !(INPUT)  Z.INTERBANCARIA + NUMERO CONTA + NUMERO DE ORDEM (8 DIG DIREITA) + TIPO
				   !             (8 DIG)         (11 DIG)                (8 DIG)                (2 DIG)
        INTEGER*4    DV            !(OUTPUT) STATUS FOR OPEN

	INTEGER*4    TAB(31)  /85, 57, 93, 19, 31, 71, 75, 56, 25, 51, 
     *                         73, 17, 89, 38, 62, 45, 53, 15, 50, 05, 
     *                         49, 34, 81, 76, 27, 90, 09, 30, 03, 10, 01/

	INTEGER*4    I, DIGIT, ASCVAL, RESTO, SOMA

	DV = 0
	SOMA = 0

	DO I = 1,29
	   ASCVAL = ICHAR(OCR_LINE(I:I))
           IF (ASCVAL.GE.48 .AND. ASCVAL.LE.57) THEN
              DIGIT = ASCVAL-48
           ELSE
              RETURN
	   ENDIF
	   SOMA = SOMA + (DIGIT * TAB(I))	   
	ENDDO

	RESTO = MOD (SOMA, 97)
	DV = 98 - RESTO 

        RETURN
        END
