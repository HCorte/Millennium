C
C SUBROUTINE X2DLYSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2DLYSNP.FOV                                 $
C  $Date::   17 Apr 1996 16:15:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2dlysnp.for **
C
C X2DLYSNP.FOR
C
C X2X Upgrade: 22-FEB-96 wsm Added AGTINF.DEF, moved VISCOM.DEF for Finland.
C
C V02 13-DEC-94 GPR Integrate UK changes into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This routine will display the network delay snapshot
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
	SUBROUTINE X2DLYSNP(CLINE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'					!V02
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'					!V02
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'					!V02
C
	INTEGER*4   MY_INDEX                  !Index of last interval
C
	INTEGER*4   SUBNETWORK/0/					!V02
	INTEGER*4   CLINE(*)
C
C	***** Start V02 changes *****
C
	INTEGER*4   ALL_DELAYS,		      !Display Selection
     *	            LAST_DELAYS,
     *		    KEY_CLEAR,
     *	            KEY_NUM,
     *              KEY_LOOP
	PARAMETER  ( ALL_DELAYS=1,
     *	            LAST_DELAYS=2,
     *		    KEY_CLEAR=3  ,
     *	            KEY_LOOP=4)
C
C	***** End V02 changes *****
C
        INTEGER*4 NUM_KEYS, POS
	PARAMETER (NUM_KEYS=3)                !NUMBER OF KEYS
	REAL*8 KEY_NAME(0:NUM_KEYS)
	DATA   KEY_NAME /'ALLdel  ','LASTdel ','CLEAR   ','LOOPback'/	!V02
	INTEGER*4 SEC, HR, MIN						!V02
C
C DETERMINE NETWORK REPONSE TIME STATISTICS.
C
C** SELECT TABULAR OR HISTOGRAM REPRESENTATION ******************
C
C
	WRITE(CLIN23,9023)'ALLdel  <subnet>, ','LASTdel <subnet>, ',	!V02
     *		  ' CLEAR <subnet>, ','LOOPback <subnet>'		!V02
C
	POS=1
	CALL KEY(CLINE,KEY_NAME,NUM_KEYS+1,POS,KEY_NUM)
C
C	***** Start V02 changes *****
C
	CALL NUMB(CLINE,POS,SUBNETWORK)
	IF (SUBNETWORK.LT.0 .OR. SUBNETWORK.GT.X2X_MAX_DELAY_SUBNET-1)
     *					      THEN
	      SUBNETWORK=0
	      IF (KEY_NUM.EQ.KEY_CLEAR) THEN
		WRITE(CLIN23,9023) 'Invalid subnetwork for the clear command'
		RETURN
	      ENDIF
	ENDIF
C
	SEC=RE_TIME_INIT(SUBNETWORK)
	HR=SEC/3600
	MIN=(SEC-HR*3600)/60
	SEC=SEC-(HR*3600+MIN*60)
C

	WRITE(CLIN1,9000) HR,MIN,SEC  
	IF (KEY_NUM .EQ. ALL_DELAYS) THEN
	  WRITE(CLIN2,90021) SUBNETWORK
	  CALL X2HISTD(X2XA_DELAY(0,SUBNETWORK),1,
     *	      X2X_NET_RESPONSE_LEN+1,1,'sec ')
C
C**** Histograms **********************************
C
	ELSEIF (KEY_NUM.EQ. LAST_DELAYS) THEN
C
	  WRITE(CLIN2,9002) SUBNETWORK
C
	  MY_INDEX = 1
	  IF (X2XA_LASTDELAY_INDEX .EQ. 1) MY_INDEX = 2
C
	  CALL X2HISTD(X2XA_LASTDELAY(0,SUBNETWORK,MY_INDEX),1,
     *	                              X2X_NET_RESPONSE_LEN+1,1,'sec ')
C
	ELSEIF (KEY_NUM .EQ. KEY_CLEAR) THEN
	  WRITE(CLIN2,9002) SUBNETWORK
	  CALL FASTSET(0,X2XA_DELAY(0,SUBNETWORK),X2X_MAXDELAY+1)
	  CALL FASTSET(0,X2XA_LASTDELAY(0,SUBNETWORK,1),
     *					      X2X_MAXDELAY+1)
	  CALL FASTSET(0,X2XA_LASTDELAY(0,SUBNETWORK,2),
     *					      X2X_MAXDELAY+1)
	  CALL FASTSET(0,RE_LOOP_GRAPH(1,SUBNETWORK),RE_GRAPH_MAX_TIME)
	  RE_LOOP_RETRY_CNT(SUBNETWORK)=0
	  RE_TIME_INIT(SUBNETWORK)=P(ACTTIM)
	ELSEIF (KEY_NUM.EQ.KEY_LOOP) THEN
	  WRITE(CLIN2,90022) SUBNETWORK,RE_LOOP_RETRY_CNT(SUBNETWORK),
     *	      P(LOOPDLAY),P(LOOPOUT),P(LOOPIN)
	  CALL X2HISTD(RE_LOOP_GRAPH(1,SUBNETWORK),1,
     *	      RE_GRAPH_MAX_TIME,1,'sec ')
	ENDIF
C
C PROGRAM EXIT
C
8000	CONTINUE
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT('Network Delays - ',' Init',I4.2,':',I2.2,':',I2.2)
9002	FORMAT('Last Delay ',' ++ Subnetwork',I3)
90021	FORMAT('Accumulative Delay ',' ++ Subnetwork',I3)
90022	FORMAT('Loopback Delay ',' ++ Subnetwork',I3,
     *	      ' retries',I6,' len in/len out/delay',I3,'/',I3,'/',I3)
9023	FORMAT(8(A,1X))
C
C	***** End V02 changes *****
C
	END
