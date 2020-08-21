C
C SUBROUTINE REQSNP
C
C V01 04-JAN-2011 FJG Lotto2 Project: Batch2 download BUG discovered
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]VIS_REQSNP.FOV                               $
C  $Date::   17 Apr 1996 15:54:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - vis_REQsnp.for **
C
C X2REQSNP.FOR
C
C X2X Upgrade: 28-FEB-96 wsm Adde AGTINF.DEF, rearranged order of VISCOM.DEF.
C
C This routine will display the number of terminals requesting a 
C segment, for each segment
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
	SUBROUTINE REQSNP(INTERVAL,REQ_SUBNETWORK)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'
C
	INTEGER*4   SUBNETWORK/0/
	INTEGER*4   REQ_SUBNETWORK
	INTEGER*4   INTERVAL
C
	INTEGER*4 SEC, HR, MIN
	INTEGER*4 ALLSUBNETS_REQ_GRAPH(0:RE_MAX_TOT_GRAPH)
					    !Contains totals for all subnets
	INTEGER*4 SEGMENT_INTERVAL,SUBNET
C
C DETERMINE NETWORK REPONSE TIME STATISTICS.
C
C** SELECT TABULAR OR HISTOGRAM REPRESENTATION ******************
C
C
C
        IF(INTERVAL.LT.10) INTERVAL = 10
        IF(INTERVAL.GT.500) INTERVAL = 500
	SUBNETWORK = REQ_SUBNETWORK
C
	IF (SUBNETWORK.EQ.255) THEN    ! Request display for all subnetworks
	  DO SEGMENT_INTERVAL=0,RE_MAX_TOT_GRAPH
	    ALLSUBNETS_REQ_GRAPH(SEGMENT_INTERVAL) = 0
	    DO SUBNET=0,RE_MAX_SUBNET
		ALLSUBNETS_REQ_GRAPH(SEGMENT_INTERVAL) = 
     *		    ALLSUBNETS_REQ_GRAPH(SEGMENT_INTERVAL) + 
     *		    RE_TOTAL_REQUESTS_GRAPH(SEGMENT_INTERVAL,SUBNET)
	    END DO  !END SUBNET COUNTER
	  END DO    !END SEGMENT_INTERVAL COUNTER
C
	ELSEIF (SUBNETWORK.LT.0 .OR. SUBNETWORK.GT.X2X_MAX_DELAY_SUBNET-1)
     *	   THEN
	      SUBNETWORK=0
	ENDIF
C
	IF (SUBNETWORK.NE.255) THEN
	    SEC=RE_TIME_INIT(SUBNETWORK)
	    HR=SEC/3600
	    MIN=(SEC-HR*3600)/60
	    SEC=SEC-(HR*3600+MIN*60)
	ENDIF
C
	WRITE(CLIN1,9000) HR,MIN,SEC
C
	IF (SUBNETWORK.EQ.255) THEN
	    WRITE(CLIN2,90030), -ALLSUBNETS_REQ_GRAPH(0)
	    CALL X2HISTD(ALLSUBNETS_REQ_GRAPH(1),1,
     *	       RE_MAX_TOT_GRAPH,INTERVAL,'segs')
	ELSE
	    WRITE(CLIN2,90021) SUBNETWORK,
     *		  -RE_TOTAL_REQUESTS_GRAPH(0,SUBNETWORK)
	    CALL X2HISTD(RE_TOTAL_REQUESTS_GRAPH(1,SUBNETWORK),1,
     *	       RE_MAX_TOT_GRAPH,INTERVAL,'segs')
	ENDIF
C
C
C PROGRAM EXIT
C
8000	CONTINUE
	RETURN
C
C     ================== Format Statements =====================
C
9000    FORMAT('Requested segments         ',I4.2,':',I2.2,':',I2.2)
90021   FORMAT('Acc. no. of requested segments ',' ++ Subnetwork',I3,
     *         3X,'Total req. segments:',I6)
9023    FORMAT(8(A,1X))
90030   FORMAT('Acc. no. of requested segments ',                     !V02
     *        ' ALL Subnetworks',4X,'Total req. terminals:',I6)

        END

