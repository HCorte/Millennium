C
C SUBROUTINE REPROC
C
C V06 16-MAR-2010 RXK Claim transaction replaced with return transaction
C V05 15-JUN-2000 UXN HBLKRDY added.
C V04 12-JAN-1997 HXK Added Instant games 
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 12-NOV-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C Copyright 1991,1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE REPROC(RTASK)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
C
	LOGICAL RTASK
C
        character*132 message
C
	INTEGER*4 BUF(LREC*3), OFFSET, INDEX, BLOCK, S, M, H, LSTTYP
	INTEGER*4 LSTTIM, STATUS, DUMMY, SER, EOF, ST, MAX
	CHARACTER*12 ATYPE(10)
	DATA ATYPE/'wager       ','cancellation','deletion    ',
     *	           'validation  ','return      ','refund      ',
     *             'special fun ','command     ','instant     ',
     *             'path thru   '/
C
C SET MULTIWIN STATUS FOR STOPSYS PROCEDURES ( BEFORE REPROCESSING TMF )
C
        CALL CHKWINS
C
C SET REPROCESSING LIMIT
C
	MAX=MAX0(P(COMSER),P(POLSER))
	IF(RTASK) MAX=999999999         !PROCESS ALL SERIAL NRS
C
C OPEN TMF FILE
C
	CALL OPENW(2,SFNAMES(1,PTMF),4,0,0,ST)
	IF(ST.NE.0) THEN
	  write(message,'(A,I8)') 'TMF open error: ',ST
          call printany(message)
          call gstop(gexit_fatal)
	ENDIF
	CALL TOPEN(2)
C	WRITE(5,900) IAM()
        call printany('Reprocessing of TMF started ....')
C
C READ NEXT TRANSACTION FROM LOG FILE
C
	EOF=0
	SER=NXTSER
20	CONTINUE
	IF(NXTSER.GE.MAX) THEN    !IF REPROCESSED ALL TRANSACTIONS
C	  WRITE(5,910) IAM()
          call printany('Reprocessing of TMF complete ...')
	  CALL USRCLOS1(     2)
	  RETURN
	ENDIF
	CALL RLOG(SER,BUF,DUMMY,STATUS)
	IF(STATUS.LT.0) THEN
	  write(message,'(A,I8)') 'TMF read error: ',ST
          call printany(message)
          call gstop(gexit_fatal)	  
	ENDIF
	IF(STATUS.GT.0) THEN
	  SER=SER+1
	  NXTSER=SER
	  GOTO 20
	ENDIF
C
C REPROCESS TRANSACTION (UPDATES MEMORY ONLY)
C TRANSACTION WILL BE QUEUED TO REPCAN OR VALPRO
C FOR CANCELLATION OR VALIDATION FILE UPDATES
C
	CALL REPROTRA(BUF,LSTTIM,LSTTYP,EOF)
	IF(EOF.NE.0) THEN
	  IF(EOF.GT.5000) THEN
	    P(REPFLG)=1
	    IF(LSTTIM.NE.0) THEN
	      IF(LSTTYP.LT.1.OR.LSTTYP.GT.10) LSTTYP=10
	      H=LSTTIM/3600
	      M=(LSTTIM-H*3600)/60
	      S=LSTTIM-H*3600-M*60
C             WRITE(5,930) IAM(),ATYPE(LSTTYP),H,M,S
	      WRITE(message,930) ATYPE(LSTTYP),H,M,S
              call printany(message)	      	      
	    ENDIF
C           WRITE(5,910) IAM()
            call printany('Reprocessing of TMF complete ...')	    
	    CALL USRCLOS1(     2)
	    RETURN
	  ENDIF
	  SER=SER+1
	ELSE
	  NXTSER=SER
	  CALL GETBI(NXTSER,BLOCK,INDEX,OFFSET)
	  HRSER=NXTSER
	  HSER=NXTSER
	  HBLOCK=BLOCK
	  HBLKRDY=BLOCK-1
	  SER=SER+1
	  NXTSER=SER
	ENDIF
	GOTO 20
C
C
C900	FORMAT(1X,A18,'Reprocessing of TMF started')
C910	FORMAT(1X,A18,'Reprocessing of TMF complete')
C920	FORMAT(1X,A18,'TMF read error ',I4)
C930	FORMAT(1X,A18,'Last transaction reprocessed was a ',A12,' at ',I2.2,':',I2.2,':',I2.2)
930	FORMAT('Last transaction reprocessed was a ',A12,' at ',I2.2,':',I2.2,':',I2.2)
	END
