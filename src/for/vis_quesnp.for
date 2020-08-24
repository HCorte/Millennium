C  GXSRC:VIS_QUESNP.FOR
C  
C  $Log:   GXAFXT:[GOLS]QUESNP.FOV  $
C  
C V14 24-FEB-2014 SCML   Added support for IGS
C V13 24-JUN-2011 FJG Reprocessing OOB QUE
C V12 17-FEB-2001 CS  ADDED PASSIVE REPROCESSING QUEUE'S (RETURN AND VALIDATION)
C V11 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V10 30-NOV-1994 DJO Modified to display the REPVAL queue's. 
C V09 03-JAN-1994 SYSTEM Applying PVCS header for automatic revision history
C V08 21-DEC-1993 SYSTEM Initial revision.
C V07 04-MAR-1992 TKO Handle more log buffers
C V06 07-FEB-1992 MTK CHANGED TO WORK WITH NEW TASKID.DEF
C V05 30-MAY-1991 TKO USE LISTSIZE TO GET SIZE - NOT ACTTSK
C V04 02-APR-1991 WS  REMOVED ENCQUE AND DECDONE QUEUES
C V03 30-JAN-1991 KWP REMOVE OLD OUTQU*'S
C V02 26-JAN-1991 TKO HANDLE 'NOT USED' TASKS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C VIS_QUESNP.FOR
C
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
C Copyright 1992 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE QUESNP(QUE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:ENCCOM.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:X2XQUE.DEF'
CPXN	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4 FRELST(3),INPLST(3),OUTLST(3)
C****	INTEGER*4 DECLST(3),DECDLST(3),DISLST(3),ENCLST(3)
	INTEGER*4 DECLST(3),DISLST(3)
	INTEGER*4 TASKS(NUMAPPQUE)
	INTEGER*4 ACTIVE(NUMTSK)
C
	INTEGER*4 QUE, OFF, INDEX, I
C
	LOGICAL*4 FIRSTCALL/.TRUE./
C
	INTEGER*4 RPVQ_LIST(6)
C----+------------------------------------------------------------------
C V14| Adding support for IGS
C----+------------------------------------------------------------------
        INTEGER*4 COMIGSQ_LIST(2)
C----+------------------------------------------------------------------
C V14| Adding support for IGS
C----+------------------------------------------------------------------
C
	INTEGER*4 LSTSTAT(9)
	DATA      LSTSTAT/'rqin','inpr','redy','rewr','rout','oinp','free',
     *	                'tape','tout'/
C
C
C
C ON FIRST CALL, REPLACE NAMES IN TASKID
C
	IF( FIRSTCALL )THEN
	  FIRSTCALL = .FALSE.
	  DO 5 I = 1, NUMTSK
	    IF(CXTSKNAM(I).EQ.' ') CXTSKNAM(I)='notusd  '
5	  CONTINUE
	ENDIF
C
C BUILD QUEUE IMAGES
C
	CALL QIMAGE(FREEQ,FRELST,3)
	CALL QIMAGE(INQUE,INPLST,3)
	CALL QIMAGE(X2X_OUTPUT,OUTLST,3)
	CALL NQIMAGE(GAME_OUTQUE,DECLST,3)
C****	CALL NQIMAGE(DECDONE,DECDLST,3)
	IF(QUE.LE.0.OR.QUE.GT.NUMAPPQUE) QUE=DIS
	CALL QIMAGE(QUETAB(1,QUE),DISLST,3)
C****	CALL NQIMAGE(ENCQUE,ENCLST,3)
C
	DO 10 OFF=1,NUMAPPQUE
	  CALL LISTSIZE(QUETAB(1,OFF),ACTIVE(OFF))
10	CONTINUE
C
C DJO - GET THE VALUES FOR THE REPROCESS VALIDATION QUEUES
C
	DO  I =1,3
	    IF	(I.EQ.1) THEN
		CALL LISTSIZE(REPVQUE(1),RPVQ_LIST(I))
	    ELSEIF(I.EQ.2) THEN
		CALL LISTSIZE(REPQUEPAS(1,RQPASPRO),RPVQ_LIST(I))
	    ELSE
		CALL LISTSIZE(REPQUEPAS(1,RQPASVAL),RPVQ_LIST(I))
	    ENDIF
	ENDDO
C----+------------------------------------------------------------------
C V14| Adding support for IGS
C----+------------------------------------------------------------------
        CALL LISTSIZE(COMIGSQUE(1),COMIGSQ_LIST(1))
C----+------------------------------------------------------------------
C V14| Adding support for IGS
C----+------------------------------------------------------------------
C
C ENCODE SCREEN IMAGE
C
	WRITE(CLIN1,901)
	WRITE(CLIN3,903)   TSKNAM(QUE)
	WRITE(CLIN4,904)FRELST(1),INPLST(1),DECLST(1),
     *               DISLST(1),OUTLST(1)
	WRITE(CLIN5,905)FRELST(2),INPLST(2),DECLST(2),
     *               DISLST(2),OUTLST(2)
	WRITE(CLIN6,905)FRELST(3),INPLST(3),DECLST(3),
     *               DISLST(3),OUTLST(3)
	WRITE(CLIN7,908)
C
	INDEX=0
	DO 100 OFF=1,NUMAPPQUE
	INDEX=INDEX+1
	TASKS(INDEX)=OFF
100	CONTINUE
C
C Display the first 8 queues
C
	WRITE(CLIN8, 910) (TASKS(I),TSKNAM(TASKS(I)),I=1,8)
	WRITE(CLIN9,911) (ACTIVE(TASKS(I)),I=1,8)
	WRITE(CLIN10,910) (TASKS(I),TSKNAM(TASKS(I)),I=9,MIN(NUMAPPQUE,16))
	WRITE(CLIN11,911) (ACTIVE(TASKS(I)),I=9,MIN(NUMAPPQUE,16))
	IF(NUMAPPQUE.GE.17)THEN
	WRITE(CLIN12,910) (TASKS(I),TSKNAM(TASKS(I)),I=17,MIN(NUMAPPQUE,24))
	WRITE(CLIN13,911) (ACTIVE(TASKS(I)),I=17,MIN(NUMAPPQUE,24))
	ENDIF
	IF(NUMAPPQUE.GE.25)THEN
	  WRITE(CLIN14,910) (TASKS(I),TSKNAM(TASKS(I)),I=25,MIN(NUMAPPQUE,32))
	  WRITE(CLIN15,911) (ACTIVE(TASKS(I)),I=25,MIN(NUMAPPQUE,32))
	ENDIF
	WRITE(CLIN16,916)
C----+------------------------------------------------------------------
C V14| Adding support for IGS
C----+------------------------------------------------------------------
C        WRITE(CLIN17,917) (RPVQ_LIST(I),I=1,3)
        WRITE(CLIN17,917) RPVQ_LIST(1),RPVQ_LIST(2),RPVQ_LIST(3),COMIGSQ_LIST(1)
C----+------------------------------------------------------------------
C V14| Adding support for IGS
C----+------------------------------------------------------------------
C
	WRITE(CLIN18,918)
	WRITE(CLIN19,920) (LSTSTAT(LOGBUF(BSTATE,I)+1),I,I=1,8)
	WRITE(CLIN20,920) (LSTSTAT(LOGBUF(BSTATE,I)+1),I,I=9,16)
	RETURN
C
C FORMAT STATEMENTS
C
901	FORMAT('**** System queue snapshot ****')
903	FORMAT(1X,' Free ',3X,'Input ',3X,' Outq ',3X,
     *	 A8,1X,' X2XQ ')
904	FORMAT(1X,'<',I4.0,'>',3X,'<',I4.0,'>',3X,'<',I4.0,'>',
     *	       3X,'<',I4.0,'>',3X,'<',I4.0,'>')
905	FORMAT(2X,I4.0,5X,I4.0,5X,I4.0,5X,I4.0,5X,I4.0)
908	FORMAT(16('-'),1X,'T R A N S A C T I O N S   I N   ',
     *	 'P R O C E S S ',16('-'))
910	FORMAT(8(I3.2,'-',A6))
911	FORMAT(8(I7,3X))
C
C QUESNP FOR PASSIVE GAMES: 33-RPVQPV 34-RPASPRO 35-RPASVAL
C

C----+------------------------------------------------------------------
C V14| Adding support for IGS
C----+------------------------------------------------------------------
C916     FORMAT(X, '33-RPVQPV 34-RPASPR 35-RPASVA')
C917     FORMAT(1X, I9, 1X, I9, 1X, I9)
916     FORMAT(X, '33-RPVQPV 34-RPASPR 35-RPASVA 36-COMIGS')
917     FORMAT(1X, I9, 1X, I9, 1X, I9, 1X, I9)
C----+------------------------------------------------------------------
C V14| Adding support for IGS
C----+------------------------------------------------------------------
C
918	FORMAT(14('-'),' L O G G E R   B U F F E R   ',
     *	 'U T I L I Z A T I O N ',14('-'))
920	FORMAT(8(2X,A4,I2,2X))
	END
