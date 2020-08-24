C
C SUBROUTINE CMDXSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMDXSTN.FOV                                  $
C  $Date::   17 Apr 1996 12:39:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - cmdxsub.for;1 **
C
C	 3-Mar-94   JWE	Add broadcast server
C
C V06 27-JUN-95 SCD CHANGE PROCESSING TO CONSERVE SPACE IN OVERFLOW TABLE.
C		    TRY TO MATCH EVSN WITH ONE IN THE SORTED TABLE AND DO
C		    IN-PLACE SWAP WITH A 0 (INVALID) STATION NUMBER IF POSSIBLE.
C V05 20-DEC-94 SCD FOR SATELITE ID CHANGES USE THE ORIGINAL CONNECTION
C		    TYPE, NOT THE CURRENT CONNECTION TYPE.  THIS ALLOWS
C		    US TO CHANGE SATELLITE IDS FOR SATELLITE STATIONS WHICH
C		    HAVE SWITCHED TO DIAL BACKUP
C V04 22-AUG-94 GPR NO LONGER PROCESS FIELDS WHICH WERE MOVED TO CLASS:
C		    BAUD, CLOCK, SYNC, DIAL ENABLE
C V03 20-JUL-94 WS MULTINETWORK CHANGES
C V02 29-APR-94 GPR LOAD THE PVC CIRCUIT FOR SATELITE ID CHANGES
C
C =============================================================
C CMDXSTN
C
C This subroutine loads the station information into
C common.
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
	SUBROUTINE CMDXSTN(FIELD,ALLREC,ADDFLG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4   FIELD           !Modified field
	INTEGER*4   ALLREC(128)     !Record buffer
	INTEGER*4   ADDFLG          !New network port
	INTEGER*4   STN             !Station number
	INTEGER*4   MESS(EDLEN)     !ERRLOG message buffer
	INTEGER*4   MAXLOCK         !Max times to try to lock common
	INTEGER*4   NETSTAT         !Send network delay statistics
	INTEGER*4   AUTOUPD         !Auto send new poll status
        INTEGER*4   CLOCK           !Clock: 0 - internal, 1 - external
        INTEGER*4   SYNC            !       0 - Async   , 1 - Sync
	INTEGER*4   MASK, NEWGRP, CONF, TEMP
	INTEGER*4   STATUS	    !Return status from X2SWPVSN - V06
	INTEGER*4   OLD_STN	    !Old Station # to match in sorted table-V06
	INTEGER*4   NEW_STN	    !New Station # to write to sorted table-V06
	INTEGER*4   OLD_EVSN(X2X_EVSN_MAXLEN)    !Old EVSN for this Station -V06
	INTEGER*4   OLDSTN, I, SAVIDX, OLDGRP
        INTEGER*4   OLDCNT, IND
        INTEGER*4   SAVE_CNT
        INTEGER*4   PVC_REC_INDX                ! PVC INDEX IN          ! V02
        PARAMETER   (PVC_REC_INDX = 24)         ! STATION RECORD.       ! V02
        INTEGER*4   OLDGVTVAL                   ! OLD CIRCUIT FOR USAT. ! V02
        INTEGER*4   GVTVAL                      ! CIRCUIT FOR USAT.     ! V02
        INTEGER*4   GVTVAL_BIT_OFFSET           ! CIRCUIT OFFSET IN     ! V02
        PARAMETER   (GVTVAL_BIT_OFFSET = 16)    ! SATELITE ID.          ! V02
        INTEGER*4   LAPB_INDEX                  ! LAPB INDX FROM SAT ID ! V02
	INTEGER*4   CLASS
	INTEGER*4   NEXT
	LOGICAL*2   ADD_TO_SORTED		! Flag indicating whether we
						! can simply update sorted table
						! or have to add to overflow-V06
C
	PARAMETER (MAXLOCK=100)
C
C STORE THE INFORMATION INTO COMMON.
C
	CALL FASTMOV(ALLREC,X2XSTN_REC,128)
	STN=X2XSTN_STN
        IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) GOTO 8000
C
	CLASS=X2XS_STNCLS(STN)
	IF(FIELD.EQ.2)  THEN
		X2XS_ADRESS_LEN(STN)=X2XSTN_ADDLEN
	ELSEIF(FIELD.EQ.3)  THEN
		X2XS_ADRESS(1,STN)=X2XSTN_ADDRES(1)
		X2XS_ADRESS(2,STN)=X2XSTN_ADDRES(2)
		DO 100, NEXT=1,X2X_STATIONS
		    IF (X2X_SORTED_ADR(1,NEXT) .EQ. X2XS_ADRESS(1,STN) .AND.
     *		       X2X_SORTED_ADR(2,NEXT) .EQ. X2XS_ADRESS(2,STN)) THEN
		      IF (X2X_SORTED_ADR(1,NEXT) .EQ. 0 .AND.
     *		         X2X_SORTED_ADR(2,NEXT) .EQ. 0) GOTO 100
			X2X_SORTED_ADR(0,NEXT)=STN
		    ENDIF
100		CONTINUE
	ELSEIF(FIELD.EQ.6)  THEN					!V04
		CALL ISBYTE(X2XSTN_TYPE,IX2XS_CONN_TYPE,STN-1)
	ELSEIF(FIELD.EQ.9) THEN						!V04
		CALL ISBYTE(X2XSTN_STNDIS,IX2XS_STATION_DISCONNECT,STN-1)
	ELSEIF(FIELD.EQ.10) THEN					!V04
		CALL ISBYTE(X2XSTN_FEDIS,IX2XS_FE_DISCONNECT,STN-1)
	ELSEIF(FIELD.EQ.11) THEN					!V04	
C
C IF THE GROUP NUMBER HAS CHANGED, DELETE THE STATION FROM THE
C EXISTING GROUP (ONLY FOR MODIFICATIONS) AND THEN RE-ADD.
C
          IF (X2XSTN_GROUP .LT. 0 .OR.
     *        X2XSTN_GROUP .GT. X2X_NUM_GROUPS) THEN
            CALL OPS('INVALID GROUP NUMBER ',X2XSTN_GROUP,STN)
            GOTO 8000
          ELSEIF (X2XSTN_GROUP .NE.0) THEN
              IF (X2XG_CNT(X2XSTN_GROUP).GE.X2X_LIST_LEN) THEN
                CALL OPS('GROUP ALREADY FULL',X2XSTN_GROUP,STN)
                GOTO 8000
              ENDIF
          ENDIF
C
          OLDGRP=X2XS_GROUP(STN)
          IF(OLDGRP.NE.0) THEN
             OLDCNT = X2XG_CNT(OLDGRP)
             IF(ADDFLG.NE.0) THEN
               SAVIDX=0
               SAVE_CNT = X2XG_CNT(OLDGRP)
               DO 200 I=1,SAVE_CNT
                 OLDSTN=X2XG_LIST(I,OLDGRP)
                 IF(OLDSTN.NE.STN) THEN
                   SAVIDX=SAVIDX+1
                   X2XG_LIST(SAVIDX,OLDGRP)=X2XG_LIST(I,OLDGRP)
C
C CHANGE CONFIGURATION CHECK TO FORCE ALL STATIONS IN THE
C RELAY CHAIN TO REQUEST NEW CONFIGURATION.
C
                   CALL ILBYTE(TEMP,IX2XS_CONF,OLDSTN-1)
                   CONF=ISHFT(IAND(TEMP,'E0'X),-5)
                   CONF=CONF+1
                   IF(MOD(CONF,8).EQ.0) CONF=0
                   CONF=ISHFT(CONF,5)
                   TEMP=IAND(TEMP,'1F'X)+CONF
                   CALL ISBYTE(TEMP,IX2XS_CONF,OLDSTN-1)
                 ELSE
                   X2XG_CNT(OLDGRP)=X2XG_CNT(OLDGRP)-1
                 ENDIF
200            CONTINUE
               IF(OLDCNT.NE.0) X2XG_LIST(OLDCNT,OLDGRP)=0
             ENDIF
          ENDIF
C
C FIND THE NEXT AVAILABLE SLOT IN THE NEW RELAY GROUP.
C
          NEWGRP=X2XSTN_GROUP
          IF(NEWGRP.NE.0) THEN
            IF(X2XG_CNT(NEWGRP).LT.X2X_LIST_LEN) THEN
              SAVE_CNT = X2XG_CNT(NEWGRP)
              DO 250 I=1,SAVE_CNT
                OLDSTN=X2XG_LIST(I,NEWGRP)
                CALL ILBYTE(TEMP,IX2XS_CONF,OLDSTN-1)
                CONF=ISHFT(IAND(TEMP,'E0'X),-5)
                CONF=CONF+1
                IF(MOD(CONF,8).EQ.0) CONF=0
                CONF=ISHFT(CONF,5)
                TEMP=IAND(TEMP,'1F'X)+CONF
                CALL ISBYTE(TEMP,IX2XS_CONF,OLDSTN-1)
250           CONTINUE
              X2XG_CNT(NEWGRP)=X2XG_CNT(NEWGRP)+1
              X2XG_LIST(X2XG_CNT(NEWGRP),NEWGRP)=STN
            ENDIF
          ENDIF
          X2XS_GROUP(STN)=NEWGRP
C        ENDIF
C
	ELSEIF(FIELD.EQ.12) THEN					!V04
		X2XS_STNCLS(STN) = X2XSTN_STNCLS		
	ELSEIF(FIELD.EQ.13) THEN					!V04
		CALL ISBYTE(X2XSTN_STATE,IX2XS_STATE,STN-1)
        ELSEIF(FIELD.EQ.14 .AND. X2XS_MAXNET.GE.1) THEN		        !V04
          I=1
          X2XS_NETPORT(I,STN) = X2XSTN_NETPT1
        ELSEIF(FIELD.EQ.15 .AND. X2XS_MAXNET.GE.2) THEN			!V04
          I=2
          X2XS_NETPORT(I,STN) = X2XSTN_NETPT2
        ELSEIF(FIELD.EQ.16 .AND. X2XS_MAXNET.GE.3) THEN		        !V04
          I=3
          X2XS_NETPORT(I,STN) = X2XSTN_NETPT3
        ELSEIF(FIELD.EQ.17 .AND. X2XS_MAXNET.GE.4) THEN		        !V04
          I=4
          X2XS_NETPORT(I,STN) = X2XSTN_NETPT4
        ELSEIF(FIELD.EQ.18 .AND. X2XS_MAXNET.GE.5) THEN		        !V04
          I=5
          X2XS_NETPORT(I,STN) = X2XSTN_NETPT5
        ELSEIF(FIELD.EQ.19 .AND. X2XS_MAXNET.GE.6) THEN		        !V04
          I=6
          X2XS_NETPORT(I,STN) = X2XSTN_NETPT6
        ELSEIF(FIELD.EQ.20 .AND. X2XS_MAXNET.GE.7) THEN			!V04
          I=7
          X2XS_NETPORT(I,STN) = X2XSTN_NETPT7
	ELSEIF(FIELD.EQ.21) THEN					!V04
		X2XS_PVC(STN)=X2XSTN_PVC
	ELSEIF(FIELD.EQ.22) THEN					!V04
		X2XS_PHYS(STN)=X2XSTN_PVCPORT
	ELSEIF(FIELD.EQ.23) THEN					!V04
	    X2X_STN_PRINT(STN)=X2XSTN_PRTFLG
	ELSEIF(FIELD.EQ.7.) THEN					!V04
	  IF(X2XSTN_DELACK.EQ.0) THEN
	    CALL BSET(IX2XS_DOWNFLAG,(STN-1)*8+2)
	  ELSE
	    CALL BCLR(IX2XS_DOWNFLAG,(STN-1)*8+2)
	  ENDIF
	ELSEIF(FIELD.EQ.8) THEN						!V04
	  IF(X2XSTN_ERRREP.EQ.0) THEN
	    CALL BSET(IX2XS_DOWNFLAG,(STN-1)*8+1)
	  ELSE
	    CALL BCLR(IX2XS_DOWNFLAG,(STN-1)*8+1)
	  ENDIF
	ELSEIF(FIELD.EQ.24) THEN					!V04
	  CALL ILBYTE(NETSTAT,IX2XS_ATRIBUTE,STN-1)
	  IF(X2XSTN_NETSTAT.EQ.0) THEN
	    NETSTAT=IOR(NETSTAT,X2XSA_STATS)
	  ELSE
	    MASK=X2XSA_AUTO_STATS
	    NETSTAT=IAND(NETSTAT,MASK)
	  ENDIF
	  CALL ISBYTE(NETSTAT,IX2XS_ATRIBUTE,STN-1)
	ELSEIF(FIELD.EQ.25) THEN					!V04
	  CALL ILBYTE(AUTOUPD,IX2XS_ATRIBUTE,STN-1)
	  IF(X2XSTN_AUTOUPD.EQ.0) THEN
	    AUTOUPD=IOR(AUTOUPD,X2XSA_AUTO_STATS)
	  ELSE
	    MASK=X2XSA_STATS
	    AUTOUPD=IAND(AUTOUPD,MASK)
	  ENDIF
	  CALL ISBYTE(AUTOUPD,IX2XS_ATRIBUTE,STN-1)
C***	ELSEIF(FIELD.EQ.30) THEN					!V04
C***	   CALL ISBYTE(X2XSTN_BAUD,IX2XS_BAUD,STN-1)			!V04
C***    ELSEIF(FIELD.EQ.31 .AND. X2XC_TTN_PORT1(CLASS).NE.0) THEN	!V04
C***       X2XS_TTN1_CHKSUM(STN) = X2XD_CHKSUM(X2XC_TTN_PORT1(CLASS))   !V04
C***    ELSEIF(FIELD.EQ.32 .AND. X2XC_TTN_PORT2(CLASS).NE.0) THEN       !V04
C***       X2XS_TTN2_CHKSUM(STN) = X2XD_CHKSUM(X2XC_TTN_PORT2(CLASS))   !V04
	ELSEIF(FIELD.EQ.26) THEN					!V04
	   X2XS_DEF_PORT(1,STN)=X2XSTN_DEF_PORT(1)			
	ELSEIF(FIELD.EQ.27) THEN					!V04
	   X2XS_DEF_PORT(2,STN)=X2XSTN_DEF_PORT(2)
	ELSEIF(FIELD.EQ.28) THEN					!V04
	   X2XS_DEF_PORT(3,STN)=X2XSTN_DEF_PORT(3)
	ELSEIF(FIELD.EQ.29) THEN					!V04
	   X2XS_DEF_PORT(4,STN)=X2XSTN_DEF_PORT(4)
C***	ELSEIF(FIELD.EQ.37) THEN					!V04
C***	   X2XS_DIAL_ENABLE(STN) = CHAR(X2XSTN_DIALENA)			!V04
        ELSEIF(FIELD.EQ.30) THEN					!V04
           X2XS_DIAL_PORT(1,STN)=X2XSTN_DIAL_PORT1
        ELSEIF(FIELD.EQ.31) THEN					!V04
           X2XS_DIAL_PORT(2,STN)=X2XSTN_DIAL_PORT2
	ELSEIF(FIELD.EQ.33) THEN					!V04
	   IF(X2XSTN_POLL.EQ.1) THEN
	      CALL BSET(BX2XS_PARAM(STN),7)
	   ELSE
	      CALL BCLR(BX2XS_PARAM(STN),7)
	   ENDIF	
        ELSEIF(FIELD.EQ.34) THEN				        !V04
           X2XS_X32PORT(1,STN) = X2XSTN_X32_PORT1
        ELSEIF(FIELD.EQ.35) THEN				        !V04
           X2XS_X32PORT(2,STN) = X2XSTN_X32_PORT2
        ELSEIF(FIELD.EQ.36) THEN					!V04
           X2XS_VSP(STN) = X2XSTN_VSP
        ELSEIF(FIELD.EQ.37) THEN					!V04
           X2XS_EVSN_LEN(STN) = X2XSTN_EVSN_LEN
        ELSEIF(FIELD.EQ.38) THEN					!V04

C	***** Start of V06 changes

	   ADD_TO_SORTED = .FALSE.			!Don't add unless we 
							!have to

C	If existing EVSN for this station is nonzero, then check sorted table
C	for this EVSN and clear station # associated with this EVSN if it's
C	found.

	   OLD_EVSN(1) = X2XS_EVSN(1,STN)
	   OLD_EVSN(2) = X2XS_EVSN(2,STN)

	   IF ((OLD_EVSN(1).NE.0).AND.(OLD_EVSN(2).NE.0)) THEN

C	Check sorted table for this station's OLD EVSN.  If we find this EVSN
C	and OLD_STN, then clear the station #, i.e. set station # to NEW_STN

	       OLD_STN = STN
	       NEW_STN = 0
	       CALL X2SWPVSN(X2XSTN_EVSN_LEN,OLD_EVSN,OLD_STN,NEW_STN,STATUS)
C	Don't bother to check status.  We only clear out old STATION # to
C	avoid having 2 entries in the sorted table for the same station.

	   ENDIF				!on OLD_EVSN check

C	If the new EVSN for this station is zero, then we must add all 0 GVT
C	IDs to the overflow table to keep the sorted and linear tables in sync.
C	If the new EVSN for this station is nonzero, then check sorted table
C	for this EVSN. If the station # associated with this EVSN is 0, then
C	simply replace 0 station # with the new station #.

	   IF ((X2XSTN_EVSN(1).EQ.0).AND.(X2XSTN_EVSN(2).EQ.0)) THEN
	       ADD_TO_SORTED = .TRUE.		!ALWAYS add 0 GVT ID to overflow
	   ELSE

C	Check sorted table for the station's NEW EVSN.  If we find this EVSN,
C	and the station #=0, then set station # to NEW_STN.  This enables us
C	to avoid using another overflow when we don't need to.

	       OLD_STN = 0
	       NEW_STN = STN
	       CALL X2SWPVSN(X2XSTN_EVSN_LEN,X2XSTN_EVSN,OLD_STN,NEW_STN,
     *			     STATUS)
	       IF (STATUS .EQ. -1) THEN		!NEW EVSN not found in sorted
		   ADD_TO_SORTED = .TRUE.	!table so add to overflow
	       ENDIF
	   ENDIF

C	***** End of V06 changes
C
	  IF (ADD_TO_SORTED) THEN		!V06
C Check the sort table overflow area for this station. If entry
C already exists for this Station then Overwrite it. Otherwise, append
C it.
C
              DO 300 IND = X2X_STATIONS+1,X2X_STATIONS+X2X_ADDED_EVSN
                 IF(X2X_SORTED_EVSN(0,IND).EQ.STN) THEN
                    X2X_SORTED_EVSN(1,IND) = X2XSTN_EVSN(1)
                    X2X_SORTED_EVSN(2,IND) = X2XSTN_EVSN(2)
                    GOTO 400
                 ENDIF
300           CONTINUE

C
C Neither station # nor EVSN are in overflow area so add to overflow if possible.
C Double check to make sure we won't oveflow sort table
C
              IF(X2X_ADDED_EVSN+1.GT.X2X_EVSN_MAXADD) THEN
                 CALL OPS('Extended Ver Seq. Table Overflow - Station',
     *                     STN,STN)
                 GOTO 8000
              ELSE
                 X2X_ADDED_EVSN = X2X_ADDED_EVSN + 1
                 X2X_SORTED_EVSN(0,X2X_STATIONS+X2X_ADDED_EVSN) = STN
                 X2X_SORTED_EVSN(1,X2X_STATIONS+X2X_ADDED_EVSN) =
     *                           X2XSTN_EVSN(1)
                 X2X_SORTED_EVSN(2,X2X_STATIONS+X2X_ADDED_EVSN) =
     *                           X2XSTN_EVSN(2)
              ENDIF				!on X2X_EVSN_MAXADD check
	   ENDIF				!on ADD_TO_SORTED check
C
400        CONTINUE
           X2XS_EVSN(1,STN) = X2XSTN_EVSN(1)
           X2XS_EVSN(2,STN) = X2XSTN_EVSN(2)

C          ***** START V02 CHANGES *****

C          Load the PVC if the Station uses
C          USAT connection type

           IF (BX2XS_ORIG_TYPE(STN) .EQ. X2XSCT_USAT_PVC) THEN		!V05

C             Save the Lapb Index and Circuit

              LAPB_INDEX = X2XSTN_EVSN(1)
              GVTVAL = ISHFT(X2XSTN_EVSN(2),-GVTVAL_BIT_OFFSET)

C             Load the PVC in ALLREC so it will get written
C             to the station file.

              ALLREC(PVC_REC_INDX) = GVTVAL

C             Save the Old PVC and
C             Load the new PVC in Commons

              OLDGVTVAL = X2XS_PVC(STN)
              X2XS_PVC(STN) = GVTVAL

C             Clear the old Circuit to station and
C             Load the new one if it matches

              IF (X2XPN_PVC_CKTS_TO_STN(OLDGVTVAL,LAPB_INDEX) .EQ.
     *            STN) THEN

                 X2XPN_PVC_CKTS_TO_STN(OLDGVTVAL,LAPB_INDEX) = 0
                 X2XPN_PVC_CKTS_TO_STN(GVTVAL,LAPB_INDEX) = STN

C             Otherwise send error message to OPs

              ELSE

                 CALL OPS('Invalid Lapb Index',LAPB_INDEX,STN)
                 GOTO 8000

              ENDIF

           ENDIF

C          ***** END V02 CHANGES *****

C***    ELSEIF(FIELD.EQ.48) THEN				!V04
C***       X2XS_CLOCK(STN)  = CHAR(X2XSTN_CLOCK)		!V04
C***    ELSEIF(FIELD.EQ.49) THEN			        !V04
C***       X2XS_SYNC(STN)   = CHAR(X2XSTN_SYNC)		        !V04
        ELSEIF(FIELD.EQ.40) THEN				!V04
           X2XS_BCST_ENABLE(STN)   = CHAR(X2XSTN_BCST_ENABLE) 	!V03
        ELSEIF(FIELD.EQ.41) THEN				!V04
	   X2XS_TYPE(STN)=X2XSTN_STATION_TYPE
	ENDIF
C
C CALCULATE DEFAULT PARAMETERS CHECKSUMS.
C
        CALL X2STNCHK(STN,TRABUF)
C
C PROGRAM EXIT.
C
8000    CONTINUE

C
C PROGRAM EXIT.
C
	RETURN
	END
