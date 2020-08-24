C
C SUBROUTINE X2RELSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RELSNP.FOV                                 $
C  $Date::   17 Apr 1996 16:30:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2relsnp.for;1 **
C
C X2RELSNP.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V07  2-sep-95	das CHANGES FOR MULTIPLE DOWNLOADS
C V06  2-JUN-95	DXG CHANGES FOR MULTIPLE DOWNLOADS
C V05 20-OCT-94 GPR Change format to I5 for Act, Req, Max
C V04 20-JUL-94 WS MULTINETWORK CHANGES
C V03 08-FEB-92 DAS INCREASE PRECISION OF PERCENTAGES
C V02 08-JAN-92 DAS CHECK ARRAY BOUNDARIES OF X2XG_CNT(GRP)
C V01 01-DEC-91 XXX RELEASED FOR VAX (NETHERLANDS)
C
C This snapshot will display all of the current relay information
C for a given relay application task.
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
	SUBROUTINE X2RELSNP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF' 
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2XGRP.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
        REAL*4      PERALL, PERSEL, PERLOAD     !percentage of loads left
	INTEGER*4   REL,GRP,STN                 !Information indicies
	INTEGER*4   ACTCNT                      !Active group count
	INTEGER*4   ACTNUM(X2X_NUM_GROUPS,3)    !Active groups(GRP/#STN/TIMEOUT)
	INTEGER*4   LOOP,BEGGRP,ENDGRP          !Loop control
	INTEGER*4   PROCESS,LOAD,SEGMENT        !Station statistics
	INTEGER*4   RTYCNT                      !Station retry count
        INTEGER*4   STNACTIVE(8)                !Active stations in broadcast
	INTEGER*4   ACTIVE(X2X_RELAY_APPS)      !Active relays array
	INTEGER*4   REQUEST(X2X_RELAY_APPS)     !Requested relays array
	INTEGER*4   LOOPTIM(X2X_RELAY_APPS)     !Current relay loop timer
	INTEGER*4   MAXDISP / 5 /               !Lines in window
	INTEGER*4   MAXCAP  / 0 /               !Maximum frontend capacity
	INTEGER*4   CURIDX,BEGIDX               !Window index
	INTEGER*4   LSTSTN,LSTREL,LSTGRP        !Last parameters
        INTEGER*4   NLOAD
	INTEGER*4   J, NUMGRP, FSTSTN, LINIDX, QUECNT, ST, MAXLIN
	INTEGER*4   FSTGRP, I, TSEGS
	INTEGER*4   RETRY_OFF
	INTEGER*4   LINCNT
C ****************** START V06 CHANGES **************************
	INTEGER*4   I4
	INTEGER*4   STNCLS
	INTEGER*4   ACT_ROMREV
	INTEGER*4   ROMREV_IDX
	INTEGER*4   APPLICATION_NO	     !APPLICATION NUMBER	    !V03
	INTEGER*4   FORE_BACKGND	     !DENOTES FOREGROUND	    !V03
					     !-BACKGROUND LOAD
	INTEGER*2   I2(2)
	EQUIVALENCE (I4,I2)
	BYTE	    I1(4)
	EQUIVALENCE (I4,I1) 
	CHARACTER*7 FOREGROUND(0:2) /'       ','FOREGND','BACKGND'/
C ****************** END V06 CHANGES **************************
C
	CHARACTER   HEAD(X2X_NUM_GROUPS)*1      !Head station in chain
	CHARACTER   ACTV(X2X_NUM_GROUPS)*1      !Active station in display
	CHARACTER   STATUS(0:2)*6               !Relay status
	CHARACTER   ATTRIB(0:4)*6               !Type of relay application
        CHARACTER   BCST_TYPE(0:3)*4
	LOGICAL     GRP_OK                      !Good group entered
	LOGICAL     STN_OK                      !Good station entered
C
	DATA        STATUS  /'notdef','  idle','active'/
        DATA        ATTRIB  /'notdef',' chain','allrel',
     *                       'allbro','stnbro'/
        DATA        BCST_TYPE/'Fore','Fore','Back','Both'/
	DATA        CURIDX / 0 /
	DATA        BEGIDX / 0 /
	DATA        LSTSTN / 0 /
	DATA        LSTREL / 0 /
	DATA        LSTGRP / 0 /
C
C INITIALIZE VARIABLES.
C
	REL=X2FLDINF(XRELIDX)
	IF(REL.EQ.0 .AND. LSTREL.NE.0) REL=LSTREL
	IF(REL.EQ.0 .OR. REL.GT.X2X_RELAY_APPS) REL=1
	GRP=X2FLDINF(XGRPIDX)
	IF(GRP.EQ.0 .AND. LSTGRP.NE.0) GRP=LSTGRP
	IF(GRP.EQ.0 .OR. GRP.GT.X2X_NUM_GROUPS) GRP=-1
	STN=X2FLDINF(XSTNIDX)
	IF(STN.EQ.0 .AND. LSTSTN.NE.0) STN=LSTSTN
	IF(STN.EQ.0 .OR. STN.GT.X2X_STATIONS) STN=-1
C
C DETERMINE THE TOTAL CAPACITY OF ALL THE FRONT ENDS.
C
	MAXCAP=0
	DO 50 I=1,X2X_SAP
	  IF (X2XR_SUBNETWORK(REL).EQ.X2XE_SUBNETWORK(I))	!V04
     *		     	  MAXCAP=MAXCAP+X2XE_MAX_CAPACITY(I)	!V04
50	CONTINUE
C
C LOAD ALL THE RELAY GROUPS CONFIGURED FOR THE APPLICATION.
C
	ACTCNT=0
	GRP_OK=.FALSE.
	FSTGRP=0
	DO 100 I=1,X2X_NUM_GROUPS
	  IF(X2XR_GROUP_ACTIVE(I,REL).NE.0) THEN
	    ACTCNT=ACTCNT+1
	    ACTNUM(ACTCNT,1)=I
	    ACTNUM(ACTCNT,2)=X2XG_CNT(I)
	    ACTNUM(ACTCNT,3)=X2XR_GROUP_TIMOUT_CNT(I,REL)
	    IF(I.EQ.1) FSTGRP=I
	    IF(GRP.EQ.-1) GRP=I
	    IF(GRP.EQ.I) GRP_OK=.TRUE.
	  ENDIF
100	CONTINUE
	MAXLIN=ACTCNT/7
	IF(MOD(ACTCNT,7).NE.0) MAXLIN=MAXLIN+1
	IF(.NOT.GRP_OK) GRP=FSTGRP
C
C DETERMINE WINDOW POSITION.
C
	IF(X2FLDINF(XBOTIDX).NE.0) THEN
	  BEGIDX=MAX0(1,MAXLIN-MAXDISP)
	  X2FLDINF(XBOTIDX)=0
	ELSE IF(X2FLDINF(XTOPIDX).NE.0) THEN
	  BEGIDX=1
	  X2FLDINF(XTOPIDX)=0
	ELSE IF(X2FLDINF(XFORIDX).NE.0) THEN
	  BEGIDX=MIN0(MAXLIN,CURIDX+(MAXDISP*X2FLDINF(XFORIDX)))
	  X2FLDINF(XFORIDX)=0
	ELSE IF(X2FLDINF(XBAKIDX).NE.0) THEN
	  BEGIDX=MAX0(1,CURIDX-(MAXDISP*X2FLDINF(XBAKIDX)))
	  X2FLDINF(XBAKIDX)=0
	ENDIF
	IF(BEGIDX.LE.0) BEGIDX=1
C
C OPEN THE GROUP FILE IF A NEW GROUP HAS BEEN ENCOUNTERED.
C
	IF(GRP.NE.LSTGRP .AND. GRP.GT.0) THEN
	  CALL OPENW(1,SFNAMES(1,XGRP),4,0,0,ST)
	  CALL IOINIT(X2XGRP_FDB,1,X2XGRP_SECT*256)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2XGRP_NAME,'OPENX',ST,0)
	    CALL GPAUSE
	  ENDIF
C
C READ THE GROUP RECORD.
C
	  CALL READW(X2XGRP_FDB,GRP,X2XGRP_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2XGRP_NAME,'READW',ST,GRP)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XGRP_REC(1).LE.0) X2XGRP_DESC='UNKNOWN     '
	ENDIF
C
C DISPLAY RELAY APPLICATION IDENTIFICATION.
C
	WRITE(CLIN1,9000) X2XR_SUBNETWORK(REL)			!V04
	WRITE(CLIN2,9010) BCST_TYPE(X2XR_APP_TO_SEND(REL))
	WRITE(CLIN3,9020)
	WRITE(CLIN4,9030)
	WRITE(CLIN5,9040) REL, STATUS(X2XR_APP_STATUS(REL)),
     *	                       ATTRIB(X2XR_APP_ATRIBUTE(REL)),
     *	                       X2XR_NO_ACTIVE(REL),
     *	                       X2XR_NO_REQUEST(REL),
     *	                      (MAXCAP*X2XR_MAX_ACTIVE(REL)/100)+1,
     *	                       X2XR_SEND_TIMOUT(REL)/1000,
     *	                       X2XR_WAIT_TIMOUT(REL)/1000,
     *	                       X2XR_ACTIVITY_CNT(REL),
     *	                       X2XR_NON_ACTIVE_CNT(REL),
     *	                       X2XR_TIMOUT_CNT(REL),
     *	                       QUECNT(X2XR_APP_QUEUE(1,REL)),
     *	                       X2XR_MAX_RETRY(REL)
C
C IF THE RELAY APPLICATION IS A CHAIN, DISPLAY THE RELAY AND
C STATION STATISTICS.
C
	LINIDX=7
	DO 150 I=LINIDX,24
	  WRITE(XNEW(  I),9200)
150	CONTINUE
	IF(X2XR_APP_ATRIBUTE(REL).NE.X2XR_APPA_ALL_NO_FORMAT .OR.
     *     GRP.GT.0) THEN
C
C CHECK FOR A VALID INPUT STATION.
C
	  FSTSTN=0
	  STN_OK=.FALSE.
          IF(GRP.GT.0) THEN
            IF(X2XG_CNT(GRP).EQ.0) GO TO 153
	    DO 152 I=1,X2XG_CNT(GRP)
	      IF(I.EQ.1) FSTSTN=X2XG_LIST(I,GRP)
	      IF(STN.EQ.-1) STN=X2XG_LIST(I,GRP)
	      IF(STN.EQ.X2XG_LIST(I,GRP)) STN_OK=.TRUE.
152	    CONTINUE
153       CONTINUE
	  ENDIF
	  IF(.NOT.STN_OK) STN=FSTSTN
C
C DISPLAY ALL ACTIVE GROUPS IN THE RELAY.
C
	  WRITE(XNEW(  LINIDX),9050) REL
	  LINIDX=LINIDX+1
	  NUMGRP=ACTCNT-((BEGIDX-1)*7+1)+1
	  LOOP=NUMGRP/7
	  IF(MOD(NUMGRP,7).NE.0) LOOP=LOOP+1
	  LOOP=MIN0(LOOP,5)
	  DO 200 I=BEGIDX,BEGIDX+LOOP-1
	    BEGGRP=(I-1)*7+1
	    ENDGRP=MIN0(BEGGRP+6,ACTCNT)
	    WRITE(XNEW(  LINIDX),9060) (ACTNUM(J,1),ACTNUM(J,2),
     *	                                ACTNUM(J,3),J=BEGGRP,ENDGRP)
	    LINIDX=LINIDX+1
200	  CONTINUE
C
	  LINIDX=LINIDX+1
	  WRITE(XNEW(LINIDX),9070) GRP, X2XGRP_DESC
	  LINIDX=LINIDX+1
C
C DISPLAY ALL STATIONS IN THE SPECIFIED RELAY GROUP.
C
          IF (GRP.GT.0) THEN 
            LOOP=X2XG_CNT(GRP)/8
	    IF(MOD(X2XG_CNT(GRP),8).NE.0) LOOP=LOOP+1
	    DO 300 I=1,LOOP
	      BEGGRP=(I-1)*8+1
	      ENDGRP=MIN0(BEGGRP+7,X2XG_CNT(GRP))
	      DO 302 J=BEGGRP,ENDGRP
	        HEAD(J)=' '
	        ACTV(J)=' '
	        IF(X2XG_LIST(J,GRP).EQ.X2XR_SEND_STATION(GRP,REL))
     *	          HEAD(J)='*'
	        IF(X2XG_LIST(J,GRP).EQ.STN) ACTV(J)='+'
302	      CONTINUE
	      WRITE(XNEW(  LINIDX),9080) (HEAD(J),ACTV(J),
     *                                X2XG_LIST(J,GRP),J=BEGGRP,ENDGRP)
	      LINIDX=LINIDX+1
300	    CONTINUE
          ENDIF
C
C DISPLAY ALL STATIONS WHICH ARE ACTIVE IN THE CURRENT PROCESS.
C
        ELSE
          ACTCNT=0
          LINCNT=0
          WRITE(XNEW(  LINIDX),9072)
          LINIDX=LINIDX+1
          DO 350 I=MAX0(1,STN),X2X_STATIONS
            IF(TSBIT(X2XR_STATION_ACTIVE(I),REL-1)) THEN
              ACTCNT=ACTCNT+1
              STNACTIVE(ACTCNT)=I
            ENDIF
            IF(ACTCNT.EQ.8) THEN
              WRITE(XNEW(  LINIDX),9082) (STNACTIVE(J),J=1,ACTCNT)
              LINIDX=LINIDX+1
              LINCNT=LINCNT+1
              ACTCNT=0
              IF(LINCNT.GT.5) GOTO 352
            ENDIF
350       CONTINUE
          IF(ACTCNT.NE.0) THEN
            WRITE(XNEW(  LINIDX),9082) (STNACTIVE(J),J=1,ACTCNT)
            LINIDX=LINIDX+1
            LINCNT=LINCNT+1
          ENDIF
        ENDIF
C
C DISPLAY THE SPECIFIC STATION.
C
352     CONTINUE
	  IF(STN.GT.0) THEN
	    LINIDX=LINIDX+1
	    WRITE(XNEW(  LINIDX),9090) STN
	    LINIDX=LINIDX+1
C           
C ********************** START V06 CHANGES *************************                                                                   
	    I4=X2XR_STATION_ID(2,STN,REL)
C
	    LOAD    = I2(2)
	    SEGMENT = I2(1)
C
	    I4=X2XR_STATION_ID(1,STN,REL)
	    ROMREV_IDX = I1(2)			
	    FORE_BACKGND = I1(1)		

            LOAD=MOD(LOAD,256)
            IF(LOAD.GT.MAXLOADS) THEN
               LOAD = X2_MAXLOADS - LOAD
            ENDIF
C
	    ACT_ROMREV = 0
	    IF (STN.NE.0) THEN
	       STNCLS = X2XS_STNCLS(STN)
	       IF (STNCLS.NE.0) THEN
	           IF (ROMREV_IDX.EQ.1) THEN
	              ACT_ROMREV = X2XC_ROMREV1(STNCLS)
		   ELSE IF (ROMREV_IDX.EQ.2) THEN
		      ACT_ROMREV = X2XC_ROMREV2(STNCLS)
		   ELSE IF (ROMREV_IDX.EQ.3) THEN
		      ACT_ROMREV = X2XC_ROMREV3(STNCLS)
		   ENDIF
	       ENDIF
	    ENDIF
C
	    IF (ACT_ROMREV.NE.0) THEN
	       CALL X2GETAPPNO(ACT_ROMREV,APPLICATION_NO,FORE_BACKGND)
	    ELSE
	       APPLICATION_NO = -1
	    ENDIF
C
	    CALL NLBYTE(PROCESS,X2XR_STATION_ID(1,STN,REL),0)
	    RETRY_OFF=X2X_STATIONS*(PROCESS-1)+STN
	    CALL ILBYTE(RTYCNT,IX2XR_RETRY_CNT,RETRY_OFF-1)
            CALL X2GETPER(APPLICATION_NO,LOAD,SEGMENT,PERALL,
     *			  PERSEL,PERLOAD)
            IF(LOAD.LE.0.OR.LOAD.GT.MAXLOADS) THEN
              NLOAD='    '
              TSEGS=0
            ELSE
              NLOAD=SMFDLNAM(1,LOAD,APPLICATION_NO)
	      IF (P(COMPRESSED_LOAD).EQ.0.AND.
     *		  LOAD.NE.MCP_LOAD_NO.AND.
     *		  LOAD.NE.ACL_LOAD_NO) THEN
                   TSEGS=SMFDLTAB(LOAD,C_NBRSEG,APPLICATION_NO)
	      ELSE
                   TSEGS=SMFDLTAB(LOAD,NBRSEG,APPLICATION_NO)
	      ENDIF
            ENDIF
C
	    IF (FORE_BACKGND.LT.1.OR.FORE_BACKGND.GT.2) FORE_BACKGND = 0
C
            WRITE(XNEW(  LINIDX),9100) RTYCNT,PROCESS,APPLICATION_NO,
     *				       FOREGROUND(FORE_BACKGND),
     *				       NLOAD,LOAD, SEGMENT,TSEGS
C ********************** END V06 CHANGES *************************                                                                   
	    LINIDX=LINIDX+1
            WRITE(XNEW(  LINIDX),9110)PERLOAD
	    LINIDX=LINIDX+1
            WRITE(XNEW(  LINIDX),9111)PERALL
    	    LINIDX=LINIDX+1
C
C DISPLAY THE NUMBER OF ACTIVE, REQUESTED, AND TIMEOUT TO SEND.
C
	    DO 400 I=1,X2X_RELAY_APPS
	      ACTIVE(I)=0
	      REQUEST(I)=0
	      LOOPTIM(I)=0
	      IF(TSBIT(X2XR_STATION_ACTIVE(STN),I-1)) ACTIVE(I)=1
	      IF(TSBIT(X2XR_STATION_REQUEST(STN),I-1)) REQUEST(I)=1
	      LOOPTIM(I)=(X2XR_STATION_TIMOUT(STN,I)-
     *	              X2X_LOOP_TIME)/1000
	      IF(LOOPTIM(I).LT.0) LOOPTIM(I)=0
400	    CONTINUE
C
            WRITE(XNEW(  LINIDX),9132) X2XS_ERR_CNT(STN),
     *                                 X2XS_LAST_ERR_CODE(STN),
     *                                 X2XS_ACK_CNT(STN)
	  ENDIF
C
C PROGRAM EXIT.
C
	LSTGRP=GRP
	LSTSTN=STN
	LSTREL=REL
	X2FLDINF(XGRPIDX)=0
	X2FLDINF(XSTNIDX)=0
	X2FLDINF(XRELIDX)=0
	CURIDX=BEGIDX
	CALL CLOSEFILE(X2XGRP_FDB)
	RETURN
C
C     ================== Format Statements ===================
C
9000	FORMAT('Broadcast snapshot, Subnetwork :',I4)		!V04
9010	FORMAT('Type: ',A4,T21,'==== Relay ====',T38,'Send',
     *         T44,'Wait',T58,'Non',	!V05
     *	       T62,'Timeout',T70,'Queue',T77,'Max')
9020	FORMAT(T4,'Status',T12,'Attrib',T21,'Act   Req   Max',		!V05
     *	       T38,'Time',T44,'Time',T53,'Acty',T58,'Active',
     *	       T65,'Cnt',T71,'Cnt',T75,'Retry')
9030	FORMAT(79('='))
9040	FORMAT(I2,T4,A6,T12,A6,T19,I5,T25,I5,T31,I5,T37,I5,T43,I5,	!V05
     *	       T49,I8,T58,I5,T64,I5,T70,I4,T76,I4)
9050	FORMAT(22('='),' Active Relay Groups for Relay ',I2,1X,23('='))
9060	FORMAT(7(I3.3,'/',I2.2,'/',I3.3,1X))
9070	FORMAT(25('='),'Relay Group: ',I3,2X,A12,1X,23('='))
9072    FORMAT(30('='),' Active Stations ',30('='))
9080	FORMAT(2X,8(A1,A1,I5,3X))
9082    FORMAT(2X,8(I5,3X))
9090    FORMAT(21('='),' Station Statistics for Station ',I5,1X,20('='))
9100	FORMAT('Retry cnt: ',I4,1X,'Process: ',I2,1X,'Appl: ',I2,1X,A,2X,
     *           A4,1X,'Load: ',I3,1X,'Segment: ',I3,' of ',I3)
9110    FORMAT(T18,'% of load remaining: ',F6.2)
9111    FORMAT(T18,'% of total loads remaining: ',F6.2)
9120	FORMAT('Request:',T13,16(A3,1X))
9130	FORMAT('Timeout:',T13,16(I3,1X))
9132    FORMAT('Del Err:',T13,I6,2X,'Last error:  ',Z8,1X,
     *         'Del Ack:',1X,I6)
9200	FORMAT(80(' '))
	END
