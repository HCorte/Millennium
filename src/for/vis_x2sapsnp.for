C
C SUBROUTINE X2SAPSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SAPSNP.FOV                                 $
C  $Date::   17 May 1996 11:45:28                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2sapsnp.for;1 **
C
C X2SAPSNP.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V03 01-AUG-95 DAS DISPALAY 24 PORTS                              
C V02 20-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes 
C		   into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will display the individual SAP
C detail for the X2X network system.
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
	SUBROUTINE X2SAPSNP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:X2MAINT.DEF'
C
	INTEGER*4   SAP                         !Current SAP
	INTEGER*4   SEC,MIN,HR                  !Time display
	INTEGER*4   NETPORT                     !Port indices
	INTEGER*4   STARTPORT                   !Start Port index
	INTEGER*4   LINIDX,POS                  !Printing variables
	INTEGER*4   MSSGTYPE                    !Message type
	INTEGER*4   MAINTDATA(20)               !Maintenance information
	INTEGER*4   COUNT, LOCALSTATE, LOCALPORT, SAPPORT, I
	CHARACTER   STATUS(0:2)*8               !SAP status
	CHARACTER   PRTSTATE(0:3)*5             !Port status
	CHARACTER   TMPLIN*80			!Output buffer
C
C
	DATA        STATUS   /'not up  ','  online','    idle'/
	DATA        PRTSTATE /'     ',' idle','onlin',' down'/
C
C INITIALIZE VARIABLES.
C
	SAP=X2FLDINF(XSAPIDX)
	IF(SAP.EQ.0 .OR. SAP.GT.X2X_SAP) SAP=1
	STARTPORT=X2FLDINF(XIDXIDX)
	IF(STARTPORT .EQ. 0 .OR. STARTPORT .GT. X2X_SAP_PORTS)
     *	STARTPORT=1
	MSSGTYPE=X2FLDINF(XMNTIDX)
C***  TYPE *, 'sap #', SAP, 'stport #', STARTPORT, 'message type',
C*** *MSSGTYPE
C
C LOAD INFORMATION.
C
	SEC=X2XE_TIME(SAP)
	HR=SEC/3600
	MIN=(SEC-HR*3600)/60
	SEC=SEC-(HR*3600+MIN*60)
	DO 200 I = 1, 24
	  WRITE(XNEW(  I),9200)
200	CONTINUE
C
C DISPLAY SAP IDENTIFICATION
C
	WRITE(CLIN1,9000)
	WRITE(CLIN2,9100) SAP, X2XE_FE_ID(SAP), X2XE_CAPACITY(SAP)
	WRITE(CLIN3,9110) X2XE_SEQ_RECV(SAP), X2XE_SEQ_XMIT(SAP),
     *	                  X2XE_CNT_ERR(SAP)
	WRITE(CLIN4,9120) X2XE_CNT_BLK(SAP), X2XE_FORMAT_TIME(SAP),
     *	                  X2XE_CONF(SAP)
	WRITE(CLIN5,9130) STATUS(X2XE_ACT_STATUS(SAP)),
     *	                  STATUS(X2XE_DEF_STATUS(SAP)),
     *	                  HR,MIN,SEC
	WRITE(CLIN6,9140) X2XE_BUF(SAP), X2XE_INDEX(SAP),
     *	                  X2XE_TOT_RCV_CNT(SAP)
	WRITE(CLIN7,9150) X2XE_TOT_MSG_CNT(SAP),
     *	                  X2XE_STATION_MSG_CNT(SAP),
     *	                  X2XE_SERIAL(SAP)
	WRITE(CLIN8,9155) X2XE_SUBNETWORK(SAP)			!V02
	IF (MSSGTYPE .EQ. 0) THEN
C
C DISPLAY PORT INFORMATION.
C
	  WRITE(CLIN10,9160)
	  WRITE(CLIN11,9170)
	  WRITE(CLIN12,9180)
	  LINIDX=13
	  POS=0
	  TMPLIN=' '
	  DO 300 SAPPORT=STARTPORT,MIN0(STARTPORT+23,X2X_SAP_PORTS)
C         IF(X2XE_LOCAL_PORT(SAPPORT,SAP).NE.0) THEN
	      LOCALPORT=X2XE_LOCAL_PORT(SAPPORT,SAP)
	      IF(LOCALPORT.NE.0) THEN
	        NETPORT=X2XPL_LOCAL_TO_NETWORK(LOCALPORT)
	        LOCALSTATE=X2XPL_STATE(LOCALPORT)
	      ELSE
	        LOCALPORT=SAPPORT
	        NETPORT=0
	        LOCALSTATE=0
	      ENDIF
	      POS=POS+1
	      IF(POS.EQ.1) THEN
	        WRITE(XNEW(  LINIDX),9190) SAPPORT,
     *	          PRTSTATE(X2XE_LOCAL_PORT_STATE(SAPPORT,SAP)),
     *	          LOCALPORT,PRTSTATE(LOCALSTATE),
     *	          NETPORT
      	        WRITE(TMPLIN(1:26),9190) SAPPORT,
     *            PRTSTATE(X2XE_LOCAL_PORT_STATE(SAPPORT,SAP)),
     *            LOCALPORT,PRTSTATE(LOCALSTATE),
     *            NETPORT
	      ELSE IF(POS.EQ.2) THEN
	        WRITE(XNEW( LINIDX),9192) TMPLIN(1:26), SAPPORT,
     *	          PRTSTATE(X2XE_LOCAL_PORT_STATE(SAPPORT,SAP)),
     *	          LOCALPORT,PRTSTATE(LOCALSTATE),
     *	          NETPORT
                WRITE(TMPLIN(27:53),9190) SAPPORT,
     *            PRTSTATE(X2XE_LOCAL_PORT_STATE(SAPPORT,SAP)),
     *            LOCALPORT,PRTSTATE(LOCALSTATE),
     *            NETPORT
	      ELSE
	        WRITE(XNEW(  LINIDX),9194) TMPLIN(1:53), SAPPORT,
     *	          PRTSTATE(X2XE_LOCAL_PORT_STATE(SAPPORT,SAP)),
     *	          LOCALPORT,PRTSTATE(LOCALSTATE),
     *	          NETPORT
	        POS=0
	        LINIDX=LINIDX+1
	      ENDIF
C         ENDIF
300	  CONTINUE
	ELSE
C
C DISPLAY MAINTENANCE INFORMATION.
C
	  WRITE(CLIN8,9960) MSSGTYPE
	  IF (MSSGTYPE .EQ. 1) THEN
	    WRITE(CLIN9,9970)
	    WRITE(CLIN10,9980)
	    WRITE(CLIN11,9990)
	  ELSE  IF (MSSGTYPE .EQ. 2) THEN
	    WRITE(CLIN9,9200)
	    WRITE(CLIN10,9220)
	    WRITE(CLIN11,9230)
	  ELSE
	    WRITE(CLIN9,9310)
	    WRITE(CLIN10,9320)
	    WRITE(CLIN11,9330)
	  ENDIF
	  LINIDX=12
	  COUNT = 0
	  DO 2110 SAPPORT = STARTPORT, X2X_SAP_PORTS
	    IF (MSSGTYPE .EQ. 1) THEN
 	      CALL ILBYTE(MAINTDATA(1),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,1,SAPPORT,SAP),X2MAINT_T1M_LINE_NO - 1)
	      CALL MOV2TOI4(MAINTDATA(3),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,1,SAPPORT,SAP),X2MAINT_T1M_NO_CONN_VC - 1)
	      CALL MOV2TOI4(MAINTDATA(4),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,1,SAPPORT,SAP),X2MAINT_T1M_NO_DISCONN - 1)
	      CALL MOV2TOI4(MAINTDATA(5),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,1,SAPPORT,SAP),X2MAINT_T1M_TOT_NO_DAT_PACK_REC - 1)
	      CALL MOV2TOI4(MAINTDATA(6),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,1,SAPPORT,SAP),X2MAINT_T1M_TOT_NO_DAT_PACK_SEN  - 1)
	      CALL MOV2TOI4(MAINTDATA(7),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,1,SAPPORT,SAP),X2MAINT_T1M_NO_AVAIL_LOG_CRC - 1)
	      CALL MOV2TOI4(MAINTDATA(8),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,1,SAPPORT,SAP),X2MAINT_T1M_TOT_NO_ERR - 1)
	      CALL MOV2TOI4(MAINTDATA(9),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,1,SAPPORT,SAP),X2MAINT_T1M_LINE_UTIL - 1)
	    ELSE   IF (MSSGTYPE .EQ. 2) THEN
	      CALL ILBYTE(MAINTDATA(1),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,2,SAPPORT,SAP),X2MAINT_T2M_LINE_NO - 1)
	      CALL MOV2TOI4(MAINTDATA(3),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,2,SAPPORT,SAP),X2MAINT_T2M_NO_CRC_ERR - 1)
	      CALL MOV2TOI4(MAINTDATA(4),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,2,SAPPORT,SAP),X2MAINT_T2M_NO_RNR_REC - 1)
	      CALL MOV2TOI4(MAINTDATA(5),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,2,SAPPORT,SAP),X2MAINT_T2M_NO_RNR_SENT - 1)
	      CALL MOV2TOI4(MAINTDATA(6),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,2,SAPPORT,SAP),X2MAINT_T2M_NO_DISCONN_00 - 1)
	      CALL MOV2TOI4(MAINTDATA(7),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,2,SAPPORT,SAP),X2MAINT_T2M_NO_DISCONN_05 - 1)
	      CALL MOV2TOI4(MAINTDATA(8),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,2,SAPPORT,SAP),X2MAINT_T2M_NO_DISCONN_09 - 1)
	      CALL MOV2TOI4(MAINTDATA(9),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,2,SAPPORT,SAP),X2MAINT_T2M_NO_DISCONN_OT - 1)
	    ELSE
	      CALL ILBYTE(MAINTDATA(1),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,3,SAPPORT,SAP),X2MAINT_T3M_LINE_NO - 1)
	      CALL MOV2TOI4(MAINTDATA(3),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,3,SAPPORT,SAP),X2MAINT_T3M_RUN_MAX_VC_CONN - 1)
	      CALL MOV2TOI4(MAINTDATA(4),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,3,SAPPORT,SAP),X2MAINT_T3M_NO_SABM_DISC_SENT - 1)
	      CALL MOV2TOI4(MAINTDATA(5),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,3,SAPPORT,SAP),X2MAINT_T3M_NO_SABM_DISC_REC - 1)
	      CALL MOV2TOI4(MAINTDATA(6),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,3,SAPPORT,SAP),X2MAINT_T3M_BAND_UTIL - 1)
	      CALL MOV2TOI4(MAINTDATA(7),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,3,SAPPORT,SAP),X2MAINT_T3M_RESP_TIME - 1)
	      CALL MOV2TOI4(MAINTDATA(8),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,3,SAPPORT,SAP),X2MAINT_T3M_TOT_TIME_ACT - 1)
	      CALL MOV2TOI4(MAINTDATA(9),X2XE_LOCAL_PORT_MAINTENANCE
     *	      (1,3,SAPPORT,SAP),X2MAINT_T3M_TOT_TIME_AVAIL - 1)
	      MAINTDATA(15) = MAINTDATA(8) / 3600
	      MAINTDATA(16) = (MAINTDATA(8)-MAINTDATA(15)*3600) / 60
	      MAINTDATA(17) = MAINTDATA(8) - (MAINTDATA(15)*3600+
     *	      MAINTDATA(16)*60)
	      MAINTDATA(18) = MAINTDATA(9) / 3600
	      MAINTDATA(19) = (MAINTDATA(9)-MAINTDATA(18)*3600) / 60
	      MAINTDATA(20) = MAINTDATA(9) - (MAINTDATA(18)*3600+
     *	      MAINTDATA(19)*60)
	    ENDIF
	    IF (MAINTDATA(1) .LE. 0 .OR. MAINTDATA(1) .GT.
     *	        X2X_SAP_PORTS) GO TO 2110
C
C THE LOCAL PORT STATUS IS DETERMINED FROM X2XE_LOCAL_PORT_STATE
C TABLE TO HAVE IT ACTUALLY UPDATED
C
	    MAINTDATA(2)=X2XE_LOCAL_PORT_STATE(MAINTDATA(1),SAP)
C
C  MAINT. DATA MESSAGE  1
C
	    IF (MSSGTYPE .EQ. 1) 
     *	    WRITE(XNEW(  LINIDX),9510) MAINTDATA(1), PRTSTATE(
     *	               MAINTDATA(2)),(MAINTDATA(I),I=3,9)
C
C  MAINT. DATA MESSAGE 2
C	  
           IF (MSSGTYPE .EQ. 2)
     *	    WRITE(XNEW(  LINIDX),9520) MAINTDATA(1), PRTSTATE(
     *	               MAINTDATA(2)),(MAINTDATA(I),I=3,9)
C
C  MAINT. DATA MESSAGE 3
C
           IF (MSSGTYPE .EQ. 3)
     *	    WRITE(XNEW(  LINIDX),9530) MAINTDATA(1), PRTSTATE(
     *	               MAINTDATA(2)),(MAINTDATA(I),I=3,7),
     *	               (MAINTDATA(I),I=15,20)
C
	    LINIDX = LINIDX + 1
	    COUNT = COUNT + 1
	    IF (COUNT .GE. 10) RETURN
2110	  CONTINUE
	ENDIF
C
C PROGRAM EXIT.
C
	RETURN
C
C     ================== Format Statements ===================
C
9000	FORMAT('Service Access Point snapshot')
9100	FORMAT(T2,'SAP #:',T17,I8,T27,'Front End id: ',T43,I8,
     *	       T53,'Capacity: ',T66,I8)
9110	FORMAT(T2,'Seq recv:',T17,I8,T27,'Seq xmit:',T43,I8,
     *	       T53,'Seq errs:',T66,I8)
9120	FORMAT(T2,'SAP blk cnt:',T17,I8,T27,'Format time:',T43,I8,
     *	       T53,'Configuration:',T66,I8)
9130	FORMAT(T2,'Active status: ',T17,A8,T27,'Def status:',T43,A8,
     *	       T53,'Last active:',T66,I2.2,':',I2.2,':',I2.2)
9140	FORMAT(T2,'Output buf: ',T17,I8,T27,'Last buf indx:',T43,I8,
     *	       T53,'Buf rec cnt:',T66,I8)
9150	FORMAT(T2,'Messages sent:',T17,I8,T27,'Sent to Stn:',T43,I8,
     *	       T53,'Last serial:',T65,I9)
9155	FORMAT(T2,'Subnetwork:',T17,I8)			!V02
9160	FORMAT(T2,31('='),' Local Ports ',31('='))
9170	FORMAT(T2,3('FE   FE   Loc  Loc  Net   '))
9180	FORMAT(T2,3('Prt State Prt State Prt   '))
9190	FORMAT(T2,I3,1X,A5,1X,I3,1X,A5,1X,I3)
9192    FORMAT(A26,T28,I3,1X,A5,1X,I3,1X,A5,1X,I3)
9194    FORMAT(A53,T54,I3,1X,A5,1X,I3,1X,A5,1X,I3)
9200    FORMAT(80(' '))
9220	FORMAT(' FE    FE     # CRC   #  RNR   # RNR',
     *	       '   Code 00 Code 05 Code 09  Other ')
9230	FORMAT('Prt   State  Errors  Received   Sent   ',4('Disconn '))
9310	FORMAT(T23, 2('SABM    '),40(' ') )
9320	FORMAT(' FE    FE    Max SVC / DISC  / DISC  Bandwth Response',
     *	       '  Active     Time ')
9330	FORMAT('Prt   State  Connect  Sent  Received  Used',
     *	       2('     Time'),'   Available ')
9510	FORMAT(I3,' | ',A5,' | ',
     *         T15,I5,' | ',I8,' | ',I6,' | ',I6,' | ',I7,' | ' 
     *         T59,I5,' | ',I6,' |')
C
9520	FORMAT(I3,' | ',A5,' | ',
     *	       T15,I5,' | ',2(I6,' | '),2(I5,' | '),
     *	       T57,2(I5,' | '),' ')
C
9530	FORMAT(I3,' | ',A5,' | ',
     *         T15,I5,' | ',I4,' | ',I6,' | ',I5,' | ',I6,' |',
     *	       T55,2(I2,':',I2.2,':',I2.2,'|'),' ')
C
9960	FORMAT(T2,20('='),' Maintenance Information Type ',I2,1X, X,
     *	20('='))
9970	FORMAT(34(' '), 'Data    Data ')
9980	FORMAT(' FE    FE     # SVC     # of     Packet    Packet ',
     *	      ' #  SVC  ',8(' '),'%  Line')
9990	FORMAT('Prt   State  Connect Disconnect Received    Sent  ',
     *	       'Available  # Err    Used')
	END
