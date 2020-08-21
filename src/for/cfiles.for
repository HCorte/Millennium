C
C V01 11-FEB-2000 OXK Released for Vakio batch
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM CFILES
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'

C
	INTEGER*4   YESNO,I,STATUS,LEN
	INTEGER*4   I4NAME(5)
	CHARACTER*20 CXNAME
	EQUIVALENCE  (CXNAME,I4NAME(1))
	INTEGER*4   SIZE
	INTEGER*4   NOFTLSIG
	EXTERNAL    NOFTLSIG
C
	CALL COPYRITE
C
	CALL LIB$ESTABLISH(NOFTLSIG)
C
	TYPE*,IAM(),'This program creates the following files -'
	TYPE*,IAM()
	TYPE*,IAM(),' 1. Game files for Vakio                  - S%F.FIL'
	TYPE*,IAM(),' 2. Checkpoint files                      - CHK%.FIL'
	TYPE*,IAM(),' 3. Pool files (POOL1.FIL,LTOPOOL1.FIL,POOLOVR1.FIL)'
	TYPE*,IAM(),' 4. Promo File                            - PROMO.FIL'
	TYPE*,IAM()
	CALL PRMYESNO('Do you want to continue [Y/N] ?',YESNO)
	IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
C CREATE GAME FILES 
C	
	DO I = 2, 6
	  WRITE(CXNAME,900) 'S',I
	  SIZE = 13400
10	  CONTINUE
	  CALL CRTFIL(I4NAME,SIZE,STATUS)
	  IF(STATUS.NE.0) THEN
	    CALL INPTEXT('Enter new file name ',CXNAME,LEN)
	    GOTO 10
	  ENDIF
	ENDDO
C
C CREATE CHKPOINT FILES
C	
	CXNAME = 'WORK:CHK0.FIL'
	SIZE   = 219400
	CALL CREATE_FILE(I4NAME,SIZE,STATUS)
	IF(STATUS.NE.0) CALL GSTOP(GEXIT_FATAL)

	CXNAME = 'WORK:CHK1.FIL'
	SIZE   = 136800
	CALL CREATE_FILE(I4NAME,SIZE,STATUS)
	IF(STATUS.NE.0) CALL GSTOP(GEXIT_FATAL)

	CXNAME = 'WORK:CHK3.FIL'
	SIZE   = 136800
	CALL CREATE_FILE(I4NAME,SIZE,STATUS)
	IF(STATUS.NE.0) CALL GSTOP(GEXIT_FATAL)

	CXNAME = 'WORK:CHK2.FIL'
	SIZE   = 82700
	CALL CREATE_FILE(I4NAME,SIZE,STATUS)
	IF(STATUS.NE.0) CALL GSTOP(GEXIT_FATAL)

	CXNAME = 'WORK:CHK4.FIL'
	SIZE   = 82700
	CALL CREATE_FILE(I4NAME,SIZE,STATUS)
	IF(STATUS.NE.0) CALL GSTOP(GEXIT_FATAL)
C
C Create POOL1.FIL
C
	CXNAME = 'FILE:POOL1.FIL'
	SIZE   = LTOSEC/2 * (LTNUMPAG+1)
	CALL CREATE_FILE(I4NAME,SIZE,STATUS)
	IF(STATUS.NE.0) CALL GSTOP(GEXIT_FATAL)
C
C Create LTOPOOL.FIL
C
	CXNAME = 'FILE:LTOPOOL.FIL'
	SIZE   = LTOSEC/2 * LTNUMPAG
	CALL CREATE_FILE(I4NAME,SIZE,STATUS)
	IF(STATUS.NE.0) CALL GSTOP(GEXIT_FATAL)
C
C Create POOLOVR1.FIL
C
	CXNAME = 'FILE:POOLOVR1.FIL'
	SIZE   = 30000
	CALL CREATE_FILE(I4NAME,SIZE,STATUS)
	IF(STATUS.NE.0) CALL GSTOP(GEXIT_FATAL)

C
C Create PROMO.FIL
C
	CXNAME = 'FILE:PROMO.FIL'
	SIZE   = 200
	CALL CREATE_FILE(I4NAME,SIZE,STATUS)
	IF(STATUS.NE.0) CALL GSTOP(GEXIT_FATAL)

C
C All is done
C
	CALL GSTOP(GEXIT_SUCCESS)
C
900	FORMAT('FILE:',A,I1,'F.FIL')
	END
