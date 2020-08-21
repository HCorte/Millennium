C
C SUBROUTINE X2ALLSAP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2ALLSAP.FOV                                 $
C  $Date::   17 Apr 1996 16:07:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2allsap.for;1 **
C
C X2ALLSAP.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V02 20-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes into 
C		   X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will produce the all SAP snapshot
C for the X2X network subsystem.
C NOTE: all necessary input information has been stored
C in X2VIS by X2LINTER.
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2ALLSAP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
        INCLUDE 'INCLIB:X2TDBH.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4   DISDIM                          !Dimension of table
	PARAMETER  (DISDIM  = 5)		    !V02
C
	INTEGER*4   I, ACTPRT, NUMPRT, LSTTIM, LSTSRT
	INTEGER*4   LSTSEC, LSTHR, LSTMIN, LSTSAP
C
	INTEGER*4   DISCNT                          !Display counter
	INTEGER*4   DISTBL(DISDIM,X2X_STATIONS)     !Display table
	INTEGER*4   SAPIDX,STSIDX,TIMIDX,ERRIDX     !Indices into DISTBL
	INTEGER*4   SUBNETIDX			    !DISTBL index - V02
	INTEGER*4   MAXDISP                         !Max lines displayed
	INTEGER*4   BEGIDX,CURIDX                   !Display indices
	INTEGER*4   SAP,PRT                         !Array indices
	INTEGER*4   SAPSRT,STSSRT,TIMSRT,ERRSRT     !Sort keys
	INTEGER*4   KEY1,KEY2,KEY3                  !Sort keys
	INTEGER*4   LINIDX                          !Print line index
	INTEGER*4   SEC,MIN,HR                      !Time display
	INTEGER*4   STATE                           !SAP state
	INTEGER*4   DELAY                           !Refresh delay
	INTEGER*4   CONNECT                         !SAP connection #
	INTEGER*4   LAN                             !LAN number
        INTEGER*4   CAPACITY                        !FE CAPACITY
	CHARACTER   SAPSTATE(0:2)*7                 !SAP state
	LOGICAL     LOADED      /.FALSE./           !Array loaded flag
	LOGICAL     DISPLAY     /.FALSE./           !Display info flag
C
	PARAMETER  (MAXDISP = 14)
	PARAMETER  (SAPIDX  = 1)
	PARAMETER  (STSIDX  = 2)
	PARAMETER  (TIMIDX  = 3)
	PARAMETER  (ERRIDX  = 4)
	PARAMETER  (SUBNETIDX=5)		    !V02
	PARAMETER  (SAPSRT  = 1)
	PARAMETER  (STSSRT  = 2)
	PARAMETER  (TIMSRT  = 3)
	PARAMETER  (ERRSRT  = 4)
	PARAMETER  (DELAY   = 60)
C
	DATA        SAPSTATE    /'notup  ','online ','idle   '/
	DATA        LSTSAP  / 0 /
	DATA        LSTMIN  / 0 /
	DATA        LSTHR   / 0 /
	DATA        LSTSEC  / 0 /
	DATA        LSTSRT  / 0 /
	DATA        LSTTIM  / 0 /
	DATA        CURIDX  / 0 /
	DATA        BEGIDX  / 0 /
	DATA        DISCNT  / 0 /
C
C INITIALIZE VARIABLES.
C
	NUMPRT=0
	ACTPRT=0
C
C IF IT IS TIME TO REFRESH THE SCREEN, SET THE LOADED
C FLAG.
C
	IF(LSTTIM+DELAY.LT.P(ACTTIM) .OR.
     *	   X2FLDINF(XUPDIDX).NE.0) THEN
	  LOADED=.FALSE.
	  X2FLDINF(XSRTIDX)=LSTSRT
	  X2FLDINF(XUPDIDX)=0
	ENDIF
C
C LOAD THE DISPLAY TABLE.
C
	IF(.NOT.LOADED) THEN
	  DISCNT=0
	  DO 100 SAP=1,X2X_SAP
	    STATE=X2XE_ACT_STATUS(SAP)
	    IF(STATE.NE.0) THEN
	      DISCNT=DISCNT+1
	      DISTBL(SAPIDX,DISCNT)=SAP
	      DISTBL(STSIDX,DISCNT)=STATE
	      DISTBL(TIMIDX,DISCNT)=X2XE_TIME(SAP)
	      DISTBL(ERRIDX,DISCNT)=X2XE_CNT_ERR(SAP)
	      DISTBL(SUBNETIDX,DISCNT)=X2XE_SUBNETWORK(SAP)	!V02
	    ENDIF
100	  CONTINUE
	  LOADED=.TRUE.
	  DISPLAY=.TRUE.
C
C CALCULATE TIME LAST UPDATED.
C
	  LSTTIM=P(ACTTIM)
	  LSTSEC=LSTTIM
	  LSTHR=LSTSEC/3600
          IF(LSTHR.GT.99.OR.LSTHR.LT.0) LSTHR = 0
	  LSTMIN=(LSTSEC-LSTHR*3600)/60
	  LSTSEC=LSTSEC-(LSTHR*3600+LSTMIN*60)
	ENDIF
C
C SORT THE DISPLAY TABLE ACCORDING TO DESIRED INFORMATION.
C DEFAULT TO SAP SORT.
C
	IF(X2FLDINF(XSRTIDX).NE.0) THEN
	  IF(X2FLDINF(XSRTIDX).EQ.STSSRT) THEN
	    KEY1=STSIDX
	    KEY2=SAPIDX
	    KEY3=0
	  ELSE IF(X2FLDINF(XSRTIDX).EQ.TIMSRT) THEN
	    KEY1=TIMIDX
	    KEY2=SAPIDX
	    KEY3=0
	  ELSE IF(X2FLDINF(XSRTIDX).EQ.ERRSRT) THEN
	    KEY1=ERRIDX
	    KEY2=SAPIDX
	    KEY3=0
	  ELSE
	    KEY1=SAPIDX
	    KEY2=0
	    KEY3=0
	  ENDIF
	  CALL I4XSORT(DISTBL,DISDIM,DISCNT,KEY1,KEY2,KEY3)
	  LSTSRT=X2FLDINF(XSRTIDX)
	  X2FLDINF(XSRTIDX)=0
	  DISPLAY=.TRUE.
	ENDIF
C
C DISPLAY STARTING AT THE BOTTOM INDEX.
C
	IF(X2FLDINF(XBOTIDX).NE.0) THEN
	  BEGIDX=1
	  X2FLDINF(XBOTIDX)=0
	  DISPLAY=.TRUE.
C
C DISPLAY STARTING AT THE TOP.
C
	ELSE IF(X2FLDINF(XTOPIDX).NE.0) THEN
	  BEGIDX=MAX0(1,DISCNT-MAXDISP)
	  X2FLDINF(XTOPIDX)=0
	  DISPLAY=.TRUE.
C
C DISPLAY FORWARD THE SPECIFIED NUMBER OF SCREENS.
C
	ELSE IF(X2FLDINF(XFORIDX).NE.0) THEN
	  BEGIDX=MIN0(DISCNT,CURIDX+(MAXDISP*X2FLDINF(XFORIDX)))
	  X2FLDINF(XFORIDX)=0
	  DISPLAY=.TRUE.
C
C DISPLAY BACKWARDS THE SPECIFIED NUMBER OF SCREENS.
C
	ELSE IF(X2FLDINF(XBAKIDX).NE.0) THEN
	  BEGIDX=MAX0(1,CURIDX-(MAXDISP*X2FLDINF(XBAKIDX)))
	  X2FLDINF(XBAKIDX)=0
	  DISPLAY=.TRUE.
C
C DISPLAY STARTING AT THE INPUT SAP NUMBER.
C
	ELSE IF(X2FLDINF(XSAPIDX).NE.LSTSAP) THEN
	  DO 200 I=1,DISCNT
	    IF(DISTBL(SAPIDX,I).EQ.X2FLDINF(XSAPIDX)) THEN
	      BEGIDX=I
	      GOTO 210
	    ENDIF
200	  CONTINUE
	  BEGIDX=CURIDX
	  WRITE(CLIN24,9050) X2FLDINF(XSAPIDX)
210	  CONTINUE
	  DISPLAY=.TRUE.
	  LSTSAP=X2FLDINF(XSAPIDX)
	ENDIF
C
C IF THE DISPLAY FLAG HAS BEEN SET, DISPLAY THE SCREEN
C STARTING AT THE CURRENT BEGINNING POSITION.
C
	IF(DISPLAY) THEN
	  WRITE(CLIN1,9000)
	  WRITE(CLIN2,9005) LSTHR, LSTMIN, LSTSEC
	  WRITE(CLIN3,9010)
	  WRITE(CLIN4,9020)
	  LINIDX=5
C
	  DO 1000 I=MAX0(1,BEGIDX),MIN0(DISCNT,BEGIDX+MAXDISP)
	    SAP=DISTBL(SAPIDX,I)
	    IF(SAP.LT.X2X_ACTUAL_SAP) GOTO 1000
C
C CALCULATE THE NUMBER OF PORTS.
C
	    NUMPRT=0
	    ACTPRT=0
	    DO 1010 PRT=1,X2X_SAP_PORTS
	      IF(X2XE_LOCAL_PORT_STATE(PRT,SAP).NE.0) NUMPRT=NUMPRT+1
	      IF(X2XE_LOCAL_PORT_STATE(PRT,SAP).EQ.X2XPS_ON_LINE)
     *	        ACTPRT=ACTPRT+1
1010	     CONTINUE
C
C CALCULATE TIME AND GET SAP STATE.
C
	    SEC=X2X_SYSTIM-X2XE_TIME(SAP)
	    HR=SEC/3600
            IF(HR.LT.0.OR.HR.GT.99) HR=99
	    MIN=(SEC-HR*3600)/60
            IF(MIN.LT.0.OR.MIN.GT.99) MIN=99
	    SEC=SEC-(HR*3600+MIN*60)
            IF(SEC.LT.0.OR.SEC.GT.99) SEC=99
	    STATE=X2XE_ACT_STATUS(SAP)
	    CONNECT=CONNECTION(X2X_GAME_SAP,SAP)
	    LAN=CURLAN(CONNECT)
C
C DETERMINE CAPACITY BASED ON FRONTEND TYPE.
C
            IF(X2XE_FE_TYPE(SAP).EQ.X2TDBH_FE_TYPE_DIAL_UP) THEN
              CAPACITY=X2XE_MAX_CAPACITY(SAP)
            ELSE
              CAPACITY=X2XE_CAPACITY(SAP)
            ENDIF
C
C OUTPUT INFORMATION.
C
	    WRITE(XNEW(  LINIDX),9040)
     *	         SAP,                X2XE_FE_ID(SAP),
     *		 X2XE_SUBNETWORK(SAP),		!V02
     *	         SAPSTATE(STATE),    HR,MIN,SEC,
     *	         CAPACITY          , X2XE_SEQ_RECV(SAP),
     *	         X2XE_SEQ_XMIT(SAP), X2XE_CNT_ERR(SAP),
     *	         CONNECT,            LAN,
     *	         NUMPRT,             ACTPRT,
     *	         X2XE_DELAY(SAP)
	    LINIDX=LINIDX+1
1000	  CONTINUE
C
C CLEAR ANY UNUSED SLOTS.
C
	  DO 1020 I=LINIDX,24
	    WRITE(XNEW(  I),9060)
1020	  CONTINUE
	  CURIDX=BEGIDX
	ENDIF
C
C PROGRAM EXIT.
C
	RETURN
C
C     ===================== Format Statements ===================
C
9000	FORMAT('All SAP Snapshot ')
9005	FORMAT(T21,'(3)',T49,'(4)',T65,'Update:',
     *	       I2.2,':',I2.2,':',I2.2)
9010	FORMAT(T2,'(1)',T14,'(2)',T21,'Last',T37,'Seq',
     *	       T43,'Seq',T49,'Seq',
     *	       T63,'Conf',T70,'Act',T75,'Ethr')
9020	FORMAT(T1,'SAP',T5,'ID',T8,'Snet',T12,' Status',T20,'Active',	!V02
     *	       T28,'Capcty',T36,'Recv',T42,'Xmit',T48,'Errs',		!V02
     *	       T53,'Conn',T58,'LAN',T63,'Port',T69,'Port',T74,'Delay')	!V02
9040	FORMAT(T1,I3,T4,I3,T7,I3,T12,A7,T19,I2.2,':',I2.2,':',I2.2,	!V02
     *	       T28,I6,T36,I5,T42,I5,T48,I5,T54,I4,T58,I2,
     *	       T62,I5,T68,I5,T74,I5)
9050	FORMAT('SAP ',I6,' not found. ')
9060	FORMAT(80(' '))
	END
