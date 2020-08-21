C
C SUBROUTINE X2ALLSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2ALLSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:07:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - vis_x2allstn.for **
C
C X2ALLSTN.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF for Finland.
C
C V12 16-MAR-11 GPW NUMAGT=12288
C V11 16-FEB-11 RXK Use IMIN0 for integer*2 values
C V10 17-MAR-96 wsm Display CREV.
C V09 22-DEC-94 GPR Do not display any unassigned dummy stations for any
C                   classes.  Need to force the display table to be reloaded
C                   so we will no longer have the unused dummy stations in
C                   DISTBL. Integrate UK changes into X2X Baseline
C V08 19-OCT-94 GPR Display XID/GVT ID for USAT and GVT
C V07 18-OCT-94 GPR Display last error code instead of group
C V06 13-OCT-94 GPR Fix loading of display table
C V05 24-AUG-94 GPR Display by subnetwork (X2X_MAX_SUBNETWORK for all)
C V04 18-AUG-94 GPR Change to handle 12 char address for UK
C V03 10-MAY-94 GPR Fix address sort
C V02  7-MAR-94 JWE Prevent activity & error counts from overflowing
C		    fields
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will produce the all station snapshot
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
	SUBROUTINE X2ALLSTN(CMDLIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'					   
	INCLUDE 'INCLIB:AGTINF.DEF'					    !V04
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:X2XREL.DEF'
C
	INTEGER*4   DISDIM                          !Dimension of table
	PARAMETER  (DISDIM  = 7)					    !V05
C
	INTEGER*4   J, SAP, ERR, ADR_LEN, I, TER 
	INTEGER*4   TOTTER(0:ALLIDX)					!V05
	INTEGER*4   TOTSTN(0:ALLIDX)					!V05
	INTEGER*4   DWNTER(0:ALLIDX)					!V05
	INTEGER*4   DWNSTN(0:ALLIDX)					!V05
	INTEGER*4   LSTTIM, LSTSRT,RVAL
	INTEGER*4   LSTSTN, LSTSEC, LSTHR, LSTMIN
	INTEGER*4   LSTSUB						!V05
C
        REAL*8      CMDOPT(3)						    !V05
        INTEGER*4   CMDLIN(2), POS, KEYNUM
	INTEGER*4   DISCNT			    !Display counter    !V05
	INTEGER*4   DISTBL(DISDIM,X2X_STATIONS)     !Display table
	INTEGER*4   STNIDX,TIMIDX,GRPIDX            !Indices into DISTBL
	INTEGER*4   DWNIDX,ADRIDX,AD2IDX            !Indices into DISTBL    !V03
	INTEGER*4   SUBIDX			    !Indices into DISTBL    !V05
	INTEGER*4   MAXDISP                         !Max lines displayed
	INTEGER*4   BEGIDX,CURIDX                   !Display indices
	INTEGER*4   DWN                             !Down term count
	INTEGER*4   STN,PRT,INDX                    !Array indices
	INTEGER*4   STNSRT,DWNSRT,TIMSRT,GRPSRT     !Sort keys
	INTEGER*4   ADRSRT                          !Sort keys
	INTEGER*4   SUBSRT			    !Sort keys
	INTEGER*4   KEY1,KEY2,KEY3                  !Sort keys
	INTEGER*4   LINIDX                          !Print line index
	INTEGER*4   NUMTER                          !Number terms
	INTEGER*4   SEC,MIN,HR                      !Time display
	INTEGER*4   STATE                           !Station state
	INTEGER*4   DELAY                           !Time to reload
	INTEGER*4   CLASS			    !Station Class	    !V05
	INTEGER*4   SUBNET			    !Subnetwork		    !V05
	INTEGER*4   SELECTED_SUBNET		    !Selected Subnetwork    !V05
        INTEGER*2   I2REV
        INTEGER*4   OFF							    !V09
	CHARACTER   CHRSTR(LXADR)*1                 !Char address	    !V04
	CHARACTER   STNSTATE(0:3)*6                 !Station state	    !V07
        CHARACTER   RELST(0:4)*5                    !Relay state
        CHARACTER*1 CREV(2)
	CHARACTER*5 TEMPSTR			    !String for write	    !V05
	CHARACTER*5 POUNDSIGN			    !# Literal		    !V05
	CHARACTER*5 STATESTR			    !STATE literal	    !V05
	CHARACTER*5 ALLSTR			    !ALL literal	    !V05
	CHARACTER*5 SIXSTR			    !SIX literal	    !V05
	CHARACTER*5 BLANKSTR			    !BLANK literal	    !V05
	CHARACTER*4 REVSTR			    !REV literal	    !V05
	CHARACTER*4 RELSTR			    !REL literal	    !V05
	CHARACTER*4 SNETSTR			    !SNET literal	    !V05
	LOGICAL     LOADED  /.FALSE./               !Array loaded flag
	LOGICAL     DISPLAY /.FALSE./		    !Display info flag
        LOGICAL     DISPREV /.FALSE./		    !Display software rev #
        LOGICAL     DISPREL /.TRUE./		    !Display Relay	    !V05
        LOGICAL     DISPSUB /.FALSE./               !Display subnetwork	    !V05
C
	PARAMETER  (POUNDSIGN='  #  ')					    !V05
	PARAMETER  (STATESTR='STATE')					    !V05
	PARAMETER  (ALLSTR=' ALL ')					    !V05
	PARAMETER  (SIXSTR=' (6) ')					    !V05
	PARAMETER  (BLANKSTR='     ')					    !V05
	PARAMETER  (REVSTR='REV ')					    !V05
	PARAMETER  (RELSTR='REL ')					    !V05
	PARAMETER  (SNETSTR='SNET')					    !V05
	PARAMETER  (MAXDISP = 14)
	PARAMETER  (STNIDX  = 1)
	PARAMETER  (ADRIDX  = 2)
	PARAMETER  (AD2IDX  = 3)					    !V03
	PARAMETER  (TIMIDX  = 4)					    !V03
	PARAMETER  (GRPIDX  = 5)					    !V03
	PARAMETER  (DWNIDX  = 6)					    !V03
	PARAMETER  (SUBIDX  = 7)					    !V05
	PARAMETER  (STNSRT  = 1)
	PARAMETER  (ADRSRT  = 2)
	PARAMETER  (DWNSRT  = 3)
	PARAMETER  (TIMSRT  = 4)
	PARAMETER  (GRPSRT  = 5)
	PARAMETER  (SUBSRT  = 6)					    !V05
	PARAMETER  (DELAY   = 60)
C
	DATA        STNSTATE    /'notact','idle  ','init  ',
     *	                         'disabl'/				    !V07
        DATA        RELST       /'illgl',' idle','on-ln',
     *                           ' rstrt','reset'/
	DATA        CMDOPT      /'XREV    ','XREL    ','XSNE    '/
	DATA        LSTMIN  / 0 /
	DATA        LSTHR   / 0 /
	DATA        LSTSEC  / 0 /
	DATA        LSTSTN  / 0 /
	DATA        LSTSUB  / ALLIDX /					!V05
	DATA        LSTSRT  / 0 /
	DATA        LSTTIM  / 0 /
	DATA        CURIDX  / 0 /
	DATA        BEGIDX  / 0 /
	DATA        DISCNT  / 0 /
C
        EQUIVALENCE(I2REV,CREV(1))
C
C CHECK FOR ANY COMMAND LINE PARAMETERS
C
        POS = 1
	CALL KEY(CMDLIN,CMDOPT,3,POS,KEYNUM)
        GOTO (10,20,30), KEYNUM
          GOTO 99
C
C  DISPLAY REV NUMBERS OR RELAY STATE
C
C  ***** Start V05 chnages *****
C
10      CONTINUE
        DISPREV = .TRUE.
        DISPREL = .FALSE.
        DISPSUB = .FALSE.
        GOTO 99
C
20      CONTINUE
        DISPREV = .FALSE.
        DISPREL = .TRUE.
        DISPSUB = .FALSE.
        GOTO 99
C
30      CONTINUE
        DISPREV = .FALSE.
        DISPREL = .FALSE.
        DISPSUB = .TRUE.
C
C  ***** End V05 chnages *****
C
C IF IT IS TIME TO REFRESH THE SCREEN, SET THE LOADED
C FLAG.
C
99      CONTINUE
	IF(LSTTIM+DELAY.LT.P(ACTTIM) .OR.
     *	   X2FLDINF(XUPDIDX).NE.0) THEN
	  LOADED=.FALSE.
	  X2FLDINF(XSRTIDX)=LSTSRT
	  X2FLDINF(XUPDIDX)=0
	  LSTSTN=0
	ENDIF
C
C       START OF V09 CHANGE BLOCK
C
C       IF A DUMMY STATION HAS TRANSITIONED FROM UNAVAILABLE TO AVAILABLE,
C       THEN FORCE THE DISPLAY TABLE, DISTBL, TO BE RELOADED SO WE WILL
C       NO LONGER DISPLAY DUMMY STATIONS AFTER A STATION HAS BEEN INSTALLED.
        IF (LOADED) THEN
            DO 50 STN=1,X2X_STATIONS
              CLASS = X2XS_STNCLS(STN)
	      IF (CLASS.NE.0) THEN
                IF ( (X2XC_DUMMY_START_STN(CLASS).LE.STN).AND.
     *               (STN.LE.X2XC_DUMMY_END_STN(CLASS)) ) THEN
                      OFF = STN - X2XC_DUMMY_START_STN(CLASS) + 1
                      IF (X2XC_DUMMY_FREE_LIST(OFF,CLASS) .EQ.
     *                    X2XC_DUMMY_AVAILABLE) THEN
                          LOADED=.FALSE.
                          GOTO 55               !NO NEED TO CHECK ANY MORE STNS
                      ENDIF
                 ENDIF
	      ENDIF
  50        CONTINUE
  55        CONTINUE
        ENDIF
C
C       END OF V09 CHANGE BLOCK
C
C	Remember to subtract one from the selected
C	subnetwork which was added by x2linter or x2vissub
C

D	TYPE*,'X2FLDINF(XSUBIDX)= ',X2FLDINF(XSUBIDX)

	IF((X2FLDINF(XSUBIDX).GT.0) .AND.
     *	   (X2FLDINF(XSUBIDX).LE.ALLIDX+1)) THEN
	  SELECTED_SUBNET=X2FLDINF(XSUBIDX)-1
	ELSE
	  SELECTED_SUBNET=LSTSUB
	  X2FLDINF(XSUBIDX)=LSTSUB+1
	ENDIF

D	TYPE*,'SELECTED_SUBNET= ',SELECTED_SUBNET

C
C	***** Start V06 changes *****
C
	IF(X2FLDINF(XSUBIDX).NE.LSTSUB+1) THEN
	  LOADED=.FALSE.
	  LSTSUB=X2FLDINF(XSUBIDX)-1
	  BEGIDX=1
	  CURIDX=1
	ENDIF	
C
C	***** End V06 changes *****
C
C LOAD THE DISPLAY TABLE.
C
	IF(.NOT.LOADED) THEN
C
C  	  ***** Start V05 chnages *****
C
	  DO I=0,ALLIDX
	     TOTTER(I)=0
	     DWNTER(I)=0
	     TOTSTN(I)=0
	     DWNSTN(I)=0
	  ENDDO
	  DISCNT=0
C
C 	  ***** End V05 chnages *****
C
	  X2FLDINF(XUPDIDX)=0
	  DO 100 STN=1,X2X_STATIONS
	    CALL ILBYTE(STATE,IX2XS_STATE,STN-1)
            IF(
     *         (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X21SWC.OR.  !NON ZERO STN_ADDR
     *          BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X25SVC).AND.
     *         ( X2XS_ADRESS(1,STN).NE.0.OR.
     *           X2XS_ADRESS(2,STN).NE.0)
     *            .OR.
     *         (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_ASYPVC.OR. !NON ZERO PORT
     *          BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X25PVC.OR.  
     *          BX2XS_CONN_TYPE(STN).EQ.X2XSCT_USAT_PVC).AND.
     *           X2XS_PHYS(STN).NE.0
     *            .OR.
     *         (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_GTECH_DIAL.OR.  !NON ZERO EVSN
     *          BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X28PAD)  
     *         ) THEN
C
C	      ***** Start V05 changes *****
C
	      CLASS=X2XS_STNCLS(STN)
C
C     	      START OF V09 CHANGE BLOCK
C
C     	      IF A DUMMY STATION HAS TRANSITIONED FROM UNAVAILABLE TO AVAILABLE,
C     	      THEN WE WILL NO LONGER DISPLAY THE DUMMY STATIONS. WE NEED TO DO
C	      THIS BECAUSE WE NO LONGER ZERO OUT THE GVTID (EVSN) IN X2ASSGVT.
C
              IF ( (X2XC_DUMMY_START_STN(CLASS).LE.STN).AND.
     *             (STN.LE.X2XC_DUMMY_END_STN(CLASS)) ) THEN
                    OFF = STN - X2XC_DUMMY_START_STN(CLASS) + 1
                    IF (X2XC_DUMMY_FREE_LIST(OFF,CLASS) .EQ.
     *                  X2XC_DUMMY_AVAILABLE) GOTO 100  !PROCESS NEXT STATION
              ENDIF
C
C     	      END OF V09 CHANGE BLOCK
C
	      SUBNET=X2XC_SUBNETWORK(CLASS)
C
C	      ***** Start V06 changes *****
C
C	      UPDATE THE COUNTERS FOR THE SUBNETWORK		    
C
	      IF((SUBNET.EQ.SELECTED_SUBNET) .OR.
     *		 (SELECTED_SUBNET.EQ.ALLIDX)) THEN
	        DISCNT=DISCNT+1
	        DISTBL(STNIDX,DISCNT)=STN
	        DISTBL(ADRIDX,DISCNT)=X2XS_ADRESS(1,STN)
	        DISTBL(AD2IDX,DISCNT)=X2XS_ADRESS(2,STN)
	        DISTBL(SUBIDX,DISCNT)=SUBNET
	        DISTBL(TIMIDX,DISCNT)=X2X_SYSTIM-X2XS_TIME(STN)
	        DISTBL(GRPIDX,DISCNT)=X2XS_GROUP(STN)
	      ENDIF
C
C	      ***** End V06 changes *****
C
	      TOTSTN(ALLIDX)=TOTSTN(ALLIDX)+1
	      IF(STATE.NE.X2XS_INIT) DWNSTN(ALLIDX)=DWNSTN(ALLIDX)+1
C	      
C	      UPDATE THE COUNTERS FOR THE SELECTED SUBNETWORK
C	      
	      IF(SUBNET.EQ.SELECTED_SUBNET) THEN
		TOTSTN(SUBNET)=TOTSTN(SUBNET)+1
		IF(STATE.NE.X2XS_INIT) DWNSTN(SUBNET)=DWNSTN(SUBNET)+1
	      ENDIF

C	      ***** End V05 changes *****
C
C	      ***** Start V06 changes *****
C
C COUNT THE NUMBER OF DISABLED TERMINALS FOR THE SUBNETWORK.
C
	      IF((SUBNET.EQ.SELECTED_SUBNET) .OR.
     *		 (SELECTED_SUBNET.EQ.ALLIDX)) THEN
	        DWN=0
	        DO 110 PRT=1,X2X_MAXPORT
	          DO 120 TER=1,X2X_MAXTERMS
	            INDX=X2XS_TERMS(TER,PRT,STN)
	            IF(INDX.NE.0) THEN
	              TOTTER(ALLIDX)=TOTTER(ALLIDX)+1
		      IF(SUBNET.EQ.SELECTED_SUBNET) THEN		!V05
		        TOTTER(SUBNET)=TOTTER(SUBNET)+1			!V05
		      ENDIF						!V05
	              CALL ILBYTE(STATE,IX2XT_STATE,INDX-1)
	              IF(STATE.NE.X2XTS_ACTIVE) THEN
	                DWNTER(ALLIDX)=DWNTER(ALLIDX)+1
		        IF(SUBNET.EQ.SELECTED_SUBNET) THEN		!V05
		          DWNTER(SUBNET)=DWNTER(SUBNET)+1		!V05
		        ENDIF						!V05
	                DWN=DWN+1
	              ENDIF
	            ENDIF
120	          CONTINUE
110	        CONTINUE
	        DISTBL(DWNIDX,DISCNT)=DWN
	      ENDIF
C
C	      ***** End V06 changes *****
C
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
C DEFAULT TO STATION SORT.
C
	IF(X2FLDINF(XSRTIDX).NE.0) THEN
	  IF(X2FLDINF(XSRTIDX).EQ.ADRSRT) THEN
	    KEY1=ADRIDX							    !V03
	    KEY2=AD2IDX							    !V03
	    KEY3=0
	  ELSE IF(X2FLDINF(XSRTIDX).EQ.DWNSRT) THEN
	    KEY1=DWNIDX
	    KEY2=TIMIDX
	    KEY3=STNIDX
	  ELSE IF(X2FLDINF(XSRTIDX).EQ.TIMSRT) THEN
	    KEY1=TIMIDX
	    KEY2=DWNIDX
	    KEY3=STNIDX
	  ELSE IF(X2FLDINF(XSRTIDX).EQ.GRPSRT) THEN
	    KEY1=GRPIDX
	    KEY2=STNIDX
	    KEY3=0
	  ELSE IF(X2FLDINF(XSRTIDX).EQ.SUBSRT) THEN
	    KEY1=SUBIDX
	    KEY2=STNIDX
	    KEY3=0
	  ELSE
	    KEY1=STNIDX
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
C DISPLAY STARTING AT THE INPUT STATION NUMBER.
C
	ELSE IF((X2FLDINF(XSTNIDX).NE.LSTSTN) .AND.
     *	        (X2FLDINF(XSTNIDX).NE.0)) THEN
	  DO 200 I=1,DISCNT
	    IF(DISTBL(STNIDX,I).EQ.X2FLDINF(XSTNIDX)) THEN
	      BEGIDX=I
	      GOTO 210
	    ENDIF
200	  CONTINUE
	  BEGIDX=CURIDX
	  WRITE(CLIN23,9050) X2FLDINF(XSTNIDX)
210	  CONTINUE
C
C	  ***** Start V05 changes *****
C
	  IF((DISTBL(SUBIDX,I).NE.SELECTED_SUBNET).AND.
     *	     (SELECTED_SUBNET.NE.ALLIDX)) THEN
            BEGIDX=CURIDX
            WRITE(CLIN23,9055) X2FLDINF(XSTNIDX),SELECTED_SUBNET
	  ENDIF
C
C	  ***** End V05 changes *****
C
	  DISPLAY=.TRUE.
	  LSTSTN=X2FLDINF(XSTNIDX)
	ENDIF
C
C IF THE DISPLAY FLAG HAS BEEN SET, DISPLAY THE SCREEN
C STARTING AT THE CURRENT BEGINNING POSITION.
C
	IF(DISPLAY) THEN
	  WRITE(CLIN1,9000)
C
C	  ***** Start V05 changes *****
C
	  IF(SELECTED_SUBNET.EQ.ALLIDX) THEN
	    TEMPSTR=ALLSTR
	  ELSE
	    WRITE(TEMPSTR,9043)SELECTED_SUBNET
	  ENDIF
C
	  WRITE(CLIN2,9005) TEMPSTR,LSTHR, LSTMIN, LSTSEC
	  WRITE(CLIN3,9010) TOTSTN(SELECTED_SUBNET),
     *	    (TOTSTN(SELECTED_SUBNET)-DWNSTN(SELECTED_SUBNET)),
     *	    DWNSTN(SELECTED_SUBNET)
	  WRITE(CLIN4,9012) TOTTER(SELECTED_SUBNET),
     *	    (TOTTER(SELECTED_SUBNET)-DWNTER(SELECTED_SUBNET)),
     *	     DWNTER(SELECTED_SUBNET)
C
          IF(DISPREV) THEN
	    WRITE(CLIN5,9015)BLANKSTR
	    WRITE(CLIN6,9020)REVSTR
	    WRITE(CLIN7,9030)POUNDSIGN
          ELSEIF(DISPREL) THEN
	    WRITE(CLIN5,9015)BLANKSTR
	    WRITE(CLIN6,9020)RELSTR
	    WRITE(CLIN7,9030)STATESTR
          ELSE
	    WRITE(CLIN5,9015)SIXSTR
	    WRITE(CLIN6,9020)SNETSTR
	    WRITE(CLIN7,9030)POUNDSIGN
          ENDIF
C
	  LINIDX=8
C
	  DO 1000 I=MAX0(1,BEGIDX),MIN0(DISCNT,BEGIDX+MAXDISP)
	    STN=DISTBL(STNIDX,I)
	    IF((DISTBL(SUBIDX,I).EQ.SELECTED_SUBNET) .OR.
     *	       (SELECTED_SUBNET.EQ.ALLIDX)) THEN
	      NUMTER=0
C
C CALCULATE THE NUMBER OF TERMINALS.
C
	      DO 1010 PRT=1,X2X_MAXPORT
	        NUMTER=NUMTER+X2XS_NUM_TERMS(PRT,STN)
1010	      CONTINUE
C
C CONVERT BCD ADDRESS TO ASCII.
C
C	      ***** Start V08 changes *****
C
              IF(.NOT.((BX2XS_CONN_TYPE(STN).EQ.X2XSCT_USAT_PVC) .OR.
     *		       (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X28PAD ) .OR.
     *		       (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_GTECH_DIAL)) ) THEN

	        ADR_LEN=X2XS_ADRESS_LEN(STN)
C
C 	        ***** Start V04 changes *****
C
	        IF(ADR_LEN.NE.0) THEN
	          CALL HTOA(CHRSTR,1,ADR_LEN,X2XS_ADRESS(1,STN),ERR)
	        ELSE
	          ADR_LEN=1
	          CHRSTR(1)=' '
	        ENDIF
C
C 	        ***** End V04 changes *****
C
              ELSE                                          !DIPSLAY EVSN/GVTID
                ADR_LEN=X2XS_EVSN_LEN(STN)
                IF(ADR_LEN.NE.0) THEN
		  CALL HTOA(CHRSTR,1,ADR_LEN,X2XS_EVSN(1,STN),ERR)
                ELSE
                  ADR_LEN=1
                  CHRSTR(1)=' '
                ENDIF
	      ENDIF
C
C	      ***** End V08 changes *****
C
C
C CALCULATE TIME
C
	      SEC=X2X_SYSTIM-X2XS_TIME(STN)
	      HR=SEC/3600
              IF(HR.LT.0.OR.HR.GT.99) HR=99
	      MIN=(SEC-HR*3600)/60
              IF(MIN.LT.0.OR.MIN.GT.99) MIN=99
	      SEC=SEC-(HR*3600+MIN*60)
              IF(SEC.LT.0.OR.SEC.GT.99) SEC=99
	      CALL ILBYTE(STATE,IX2XS_STATE,STN-1)
	      CALL ILBYTE(SAP,IX2XS_SAP,STN-1)
C
C OUTPUT INFORMATION.
C
C
C             TEST FOR INVALID GROUP VALUES
C
              IF(X2XS_GROUP(STN).LE.0) THEN
                RVAL = 1
              ELSE
                RVAL = X2XG_STATE(X2XS_GROUP(STN))
              ENDIF
C
	      WRITE(XNEW(  LINIDX),9060)
              IF(DISPREV) THEN
                I2REV=X2XS_REVISION(STN)
		WRITE(TEMPSTR,9041)CREV(2),CREV(1)
	      ELSEIF(DISPREL) THEN
	        WRITE(TEMPSTR,9042)RELST(X2XR_STATION_STATE(STN))
	      ELSE
	        WRITE(TEMPSTR,9043)DISTBL(SUBIDX,I)
	      ENDIF

c-	      WRITE(XNEW(  LINIDX),9040) STN, STNSTATE(STATE),
	      WRITE(XNEW(  LINIDX),9040) CREV(2),CREV(1),STN,		  !V10
     *                STNSTATE(STATE),					  !V10
     *               (CHRSTR(J),J=MAX0(ADR_LEN-DADRL+1,1),ADR_LEN),
     *                NUMTER, DISTBL(DWNIDX,I),
     *                HR,MIN,SEC, 
     *		      MIN0(999999,X2XS_CNT_ACTIVE(STN)),
     *                IMIN0(9999,X2XS_ERR_CNT(STN)),
     *                X2XS_LAST_ERR_CODE(STN), !V07 X2XS_GROUP(STN),RGRST(RVAL),
     *                SAP,TEMPSTR

	      LINIDX=LINIDX+1

	    ENDIF
C
C	  ***** End V05 changes *****
C
1000	  CONTINUE
C
C CLEAR ANY UNUSED SLOTS.
C
	  DO 1020 I=LINIDX,22
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
9000	FORMAT('All Stations Snapshot ')
9005	FORMAT('Subnetwork:   ',A,T65,'Update:',I2.2,':',I2.2,':',I2.2)	  !V05
9010	FORMAT('Stns Config: ',I6,T26,'Stns Active: ',I6,
     *	       T50,'Stns not active: ',I6)
9012	FORMAT('Term Config: ',I6,T26,'Term Active: ',I6,
     *	       T50,'Term not active: ',I6)
9015	FORMAT(T37,'(3)',T44,'(4)',T65,'(5)',T76,A)			  !V07
9020	FORMAT(T3,'(1)',T22,'(2)',T29,'Num',T34,'Inactive',T44,'Time',
     *	       T54,'Station',T65,'Err',T70,'Last',T76,A)		  !V07
9030	FORMAT(T1,'Station',T9,'State',T19,'Address',T28,'Terms',	  !V07
     *	       T35,'Terms',T42,'Inactive',T53,'Actv/Err',T64,'Code',	  !V07
     *	       T71,'SAP',T75,A)						  !V07
c-9040    FORMAT(T2,I5,T9,A6,T<16+DADRL-MIN0(DADRL,ADR_LEN)>,		  !V07
9040    FORMAT(2A1,T5,I5,T12,A6,T<16+DADRL-MIN0(DADRL,ADR_LEN)>,	  !V10
     *	       <MIN0(DADRL,ADR_LEN)>A1,T29,I3,T36,I3,			  !V04
     *	       T42,I2.2,':',I2.2,':',I2.2,T50,I6,'/',I4,T63,Z6,		  !V07
     *	       T70,I3,T74,A5)						  !V04
9041    FORMAT(' ',2Z2)
9042    FORMAT(A5)
9043    FORMAT(I5)
9050	FORMAT('Station ',I6,' not found. ')
9055	FORMAT('Station ',I6,' not found for Subnetwork ',I6)
9060	FORMAT(80(' '))
	END
