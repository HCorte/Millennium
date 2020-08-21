C
C SUBROUTINE X2NETSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2NETSNP.FOV                                 $
C  $Date::   17 Apr 1996 16:25:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - vis_x2netsnp.for **
C
C X2NETSNP.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, moved VISCOM.DEF for Finland.
C
C V03  18-AUG-94 GPR HANDLE 12 ADDRESS CHARS FOR UK - Integrate UK changes 
C		     into X2X Baseline
C V02  7-MAR-94 JWE Add broadcast server ports
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will display all network ports defined
C in the X2X network subsystem.
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
	SUBROUTINE X2NETSNP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'		    !V03
	INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:X2MAINT.DEF'
C
	INTEGER*4   DISDIM                          !Dimension of table
	PARAMETER  (DISDIM  = 4)
C
	INTEGER*4   DISCNT                          !Display counter
	INTEGER*4   DISTBL(DISDIM,X2X_NETWORK_PORTS)!Display table
	INTEGER*4   NETIDX,LOCIDX,SAPIDX,TYPIDX     !Indices into DISTBL
	INTEGER*4   MAXDISP                         !Max lines displayed
	INTEGER*4   BEGIDX,CURIDX                   !Display indices
	INTEGER*4   NET,LOC,SAP                     !Array indices
	INTEGER*4   NETSRT,LOCSRT,SAPSRT,TYPSRT     !Sort keys
	INTEGER*4   KEY1,KEY2,KEY3                  !Sort keys
	INTEGER*4   LINIDX                          !Print line index
	INTEGER*4   SEC,MIN,HR                      !Time display
	INTEGER*4   DELAY                           !Refresh delay
	INTEGER*4   J, ERR, SAPPRT, I, LSTTIM, LSTSRT
	INTEGER*4   LSTSEC, LSTHR, LSTMIN, LSTNET
	INTEGER*4   ADR_LEN			    !Address length - V03
	CHARACTER   PRTSTATE(0:4)*7                 !Terminal state
	CHARACTER   CHRSTR(LXADR)*1                 !BCD address	!V03
	CHARACTER   YESNO(0:1)*3                    !Yes/no enable
	CHARACTER   PRTTYPE(0:7)*7                  !Port type
	LOGICAL     LOADED  /.FALSE./               !Array loaded flag
	LOGICAL     DISPLAY /.FALSE./               !Display info flag
C
	PARAMETER  (MAXDISP = 15)
	PARAMETER  (NETIDX  = 1)
	PARAMETER  (LOCIDX  = 2)
	PARAMETER  (SAPIDX  = 3)
	PARAMETER  (TYPIDX  = 4)
	PARAMETER  (NETSRT  = 1)
	PARAMETER  (LOCSRT  = 2)
	PARAMETER  (SAPSRT  = 3)
	PARAMETER  (TYPSRT  = 4)
	PARAMETER  (DELAY   = 100)
C
	DATA        PRTSTATE    /'not def','   idle',' online',
     *	                         '   down','conpend'/
	DATA        PRTTYPE     /'invalid','   X.21','   X.25',
     *	                         ' Dialup','  Async','   Usat',
     *				 'BCST 1 ','BCST 2'/
	DATA        YESNO       /'yes',' no'/
	DATA        LSTNET  / 0 /
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
	LOADED=.FALSE.
	NET=X2FLDINF(XNETIDX)
	IF(NET.LE.0 .OR. NET.GT.X2X_NETWORK_PORTS) NET=1
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
	  DO 100 NET=1,X2X_NETWORK_PORTS
	    LOC=X2XPN_NETWORK_TO_LOCAL(NET)
	    IF(LOC.NE.0) THEN
	      DISCNT=DISCNT+1
	      DISTBL(NETIDX,DISCNT)=NET
	      DISTBL(LOCIDX,DISCNT)=LOC
	      DISTBL(SAPIDX,DISCNT)=X2XPL_SAP(LOC)
	      DISTBL(TYPIDX,DISCNT)=X2XPN_TYPE(NET)
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
	  LSTMIN=(LSTSEC-LSTHR*3600)/60
	  LSTSEC=LSTSEC-(LSTHR*3600+LSTMIN*60)
	ENDIF
C
C SORT THE DISPLAY TABLE ACCORDING TO DESIRED INFORMATION.
C DEFAULT TO SAP SORT.
C
	IF(X2FLDINF(XSRTIDX).NE.0) THEN
	  IF(X2FLDINF(XSRTIDX).EQ.LOCSRT) THEN
	    KEY1=LOCIDX
	    KEY2=0
	    KEY3=0
	  ELSE IF(X2FLDINF(XSRTIDX).EQ.SAPSRT) THEN
	    KEY1=SAPIDX
	    KEY2=0
	    KEY3=0
	  ELSE IF(X2FLDINF(XSRTIDX).EQ.TYPSRT) THEN
	    KEY1=TYPIDX
	    KEY2=NETIDX
	    KEY3=0
	  ELSE
	    KEY1=NETIDX
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
	  BEGIDX=MAX0(1,DISCNT-MAXDISP)
	  X2FLDINF(XBOTIDX)=0
	  DISPLAY=.TRUE.
C
C DISPLAY STARTING AT THE TOP.
C
	ELSE IF(X2FLDINF(XTOPIDX).NE.0) THEN
	  BEGIDX=1
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
C DISPLAY STARTING AT THE INPUT NETWORK PORT NUMBER.
C
	ELSE IF(X2FLDINF(XNETIDX).NE.LSTNET) THEN
	  DO 300 I=1,DISCNT
	    IF(DISTBL(NETIDX,I).EQ.X2FLDINF(XNETIDX)) THEN
	      BEGIDX=I
	      GOTO 310
	    ENDIF
300	  CONTINUE
	  BEGIDX=CURIDX
310	  CONTINUE
	  DISPLAY=.TRUE.
	  LSTNET=X2FLDINF(XNETIDX)
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
	  WRITE(CLIN5,9030)
	  LINIDX=6
C
	  DO 1000 I=MAX0(1,BEGIDX),MIN0(DISCNT,BEGIDX+MAXDISP)
	    NET=DISTBL(NETIDX,I)
	    LOC=DISTBL(LOCIDX,I)
	    SAP=DISTBL(SAPIDX,I)
	    SAPPRT=X2XPL_SAP_PORT(LOC)
C
C CALCULATE TIME AND GET TERMINAL STATE.
C
CV03	    CALL HTOA(CHRSTR,1,10,X2XPN_ADRESS(1,NET),ERR)
C 	    ***** Start V03 changes *****
C
	    ADR_LEN=X2XPN_ADDLEN(NET)
	    IF(ADR_LEN.NE.0) THEN
	      CALL HTOA(CHRSTR,1,ADR_LEN,X2XPN_ADRESS(1,NET),ERR)
	    ELSE
	      ADR_LEN=1
	      CHRSTR(1)=' '
	    ENDIF
C
C 	    ***** End V03 changes *****
C
	    SEC=X2XPN_TIME(NET)
	    HR=SEC/3600
	    MIN=(SEC-HR*3600)/60
	    SEC=SEC-(HR*3600+MIN*60)
C
C GET THE CURRENT CAPACITY.
C
            DO 1010 J=1,X2X_SAP_PORTS
              CALL X2MNT1TRA(X2XE_LOCAL_PORT_MAINTENANCE(1,1,J,SAP))
              IF(X2MNT1_LINE.EQ.SAPPRT) GOTO 1015
1010        CONTINUE
            X2MNT1_NUMSVC=0
C
C OUTPUT INFORMATION.
C
1015        CONTINUE     
	    WRITE(XNEW(  LINIDX),9040)
     *	      NET, PRTSTATE(X2XPN_STATE(NET)),
     *	      LOC, PRTSTATE(X2XPL_STATE(LOC)),
     *	      SAP, SAPPRT,
     *	      PRTSTATE(X2XE_LOCAL_PORT_STATE(SAPPRT,SAP)),
     *        X2MNT1_NUMSVC, PRTTYPE(X2XPN_TYPE(NET)),
     *	     (CHRSTR(J),J=MAX0(ADR_LEN-DADRL+1,1),ADR_LEN),	      !V03
     *	      YESNO(X2XPN_OUTCALL(NET))				      !V03
	    LINIDX=LINIDX+1
1000	  CONTINUE
C
C........ 
         LINIDX = LINIDX + 1
C
C CLEAR ANY UNUSED SLOTS.
C
	  DO 1020 I=LINIDX,23
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
9000	FORMAT('Network Port Snapshot ')
9005	FORMAT(T65,'Update:',I2.2,':',I2.2,':',I2.2)
9010	FORMAT(T2,'(1)',T15,'(2)',T28,'(3)')
9020	FORMAT(T2,'==Network==',T15,'===Local===',
     *	       T28,'======SAP======',T53,'(4)')
9030	FORMAT(T2,'Prt  Status',T15,'Prt  Status',
     *	       T28,'SAP Prt  Status',T44,'Capcty',T53,'Type',
     *	       T64,'Address',T73,'Outcall')			        !V03
9040	FORMAT(T1,I4,1X,A7,T14,I5,1X,A7,T28,I3,1X,I3,1X,A7,T43,I6,
     *	       T51,A7,T<60+DADRL-MIN0(DADRL,ADR_LEN)>,			!V03
     *	       <MIN0(DADRL,ADR_LEN)>A,T73,A3)				!V03
9060	FORMAT(80(' '))
	END
