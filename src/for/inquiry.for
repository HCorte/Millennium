C  GXSRC:INQUIRY.FOR
C
C SUBROUTINE INQUIRY
C
C INQUIRY.FOR
C
C V02 04-FEB-98 UXN EXP_QLOGTRA used instead of LOGTRA
C V01 17-APR-97 UXN Initial release.
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
C Input   : 
C           START_TIME  - search start time
C           END_TIME    - search end time
C           ACC_NR      - player account number
C           TX_NR       - transaction # from FEP database
C Output  :
C           SERIAL_NR   - serial # in GOLS if found, otherwise = 0
C           STATUS_CODE -  0 - no errors occured during reading TMF
C                          1 - transaction found, but
C			       status not GOOD
C                         -1 - error opening TMF file
C                         -2 - error closing TMF file

C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE INQUIRY( START_TIME,
     *                      END_TIME,
     *                      ACC_NR,
     *                      TX_NR,
     *                      SERIAL_NR,
     *                      STATUS )
        IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
C
C Input parameters
C
        INTEGER*4 START_TIME,END_TIME
        INTEGER*4 ACC_NR, TX_NR
C
C Output parameters
C
        INTEGER*4 SERIAL_NR, STATUS
C
C Local variables
C       
        INTEGER*4 FIRST_NUM,LAST_NUM,AVG_NUM
        INTEGER*4 LAST_NUM_SEARCH
        INTEGER*4 BUF(LMUREC)
        INTEGER*4 DUMMY, ST
C
C End of local variables...
C
        STATUS = 0
        SERIAL_NR = 0

C
C  OPEN TMF FILE
C
        CALL OPENW(PTMF,SFNAMES(1,PTMF),4,0,0,ST)
        IF(ST.NE.0) THEN
           CALL USRCLOS1(   PTMF)
           STATUS = -1
           RETURN
        ENDIF
	CALL TOPEN(PTMF)
C
C  DO BINARY SEARCH 
C
        FIRST_NUM = 1
        LAST_NUM = NXTSER       ! NEXT AVAILABLE SERIAL NUMBER
        LAST_NUM_SEARCH = NXTSER-1    ! To detect end of TM
        AVG_NUM = FIRST_NUM + (LAST_NUM - FIRST_NUM)/2
C
20      CONTINUE
C
        IF( AVG_NUM.EQ.FIRST_NUM ) GOTO 100

        CALL RLOG_NH(AVG_NUM,BUF,DUMMY,ST)
  
	IF(ST.NE.0) THEN
	   AVG_NUM = AVG_NUM - 1
	   GOTO 20
	ENDIF      

        CALL EXP_QLOGTRA(TRABUF,BUF)
C
        IF(TRABUF(TTYP).EQ.NUSD) THEN
           AVG_NUM = AVG_NUM - 1        ! THEN TAKE PREVIOUS
           GOTO 20
        ENDIF

        IF(TRABUF(TTIM).LT.START_TIME) THEN
            FIRST_NUM  = AVG_NUM
        ELSE
            LAST_NUM   = AVG_NUM
        ENDIF
C       
        AVG_NUM = FIRST_NUM + (LAST_NUM - FIRST_NUM)/2
        GOTO 20   
C
C  END OF BINARY SEARCH 
C
100     CONTINUE        
C
C Starting sequential search from FIRST_NUM until END_TIME reached
C       
        IF(FIRST_NUM .GT. LAST_NUM_SEARCH) GOTO 200

        CALL RLOG_NH(FIRST_NUM,BUF,DUMMY,ST)
        FIRST_NUM = FIRST_NUM + 1
      
	IF(ST.NE.0) GOTO 100

        CALL EXP_QLOGTRA(TRABUF,BUF)
C        
        IF(TRABUF(TTYP).NE.TWAG)  GOTO 100
C
        IF(TRABUF(TTIM).GT.END_TIME) GOTO 200        
C
        IF( TRABUF(TWBNKID).EQ.TX_NR .AND. 
     *      TRABUF(TWBNKNM).EQ.ACC_NR )  THEN  ! Transaction found !!!  
          SERIAL_NR = TRABUF(TSER)
	  IF(TRABUF(TSTAT).NE.GOOD) STATUS = 1
          GOTO 200
        ELSE    
          GOTO 100
        ENDIF      
C
C       END OF SEQUENTIAL SEARCH
C
200     CONTINUE
C
C CLOSE TMF FILE
C
        CALL USRCLOS1(   PTMF)
        IF(ST.LT.0) THEN
           STATUS = -2
           RETURN
        ENDIF
        RETURN
        END
