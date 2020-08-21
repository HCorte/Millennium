C
C SUBROUTINE X2LODSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:22:06                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodstn.for;1 **
C
C X2LODSTN.FOR
C
C
C V05 13-DEC-94 GPR Integrate UK changes into X2X Baseline
C V04 22-AUG-94 GPR NO LONGER PROCESS FIELDS WHICH WERE MOVED TO CLASS:
C                   BAUD, CLOCK, SYNC, DIAL ENABLE
C V03 20-JUL-94 WS MULTINETWORK CHANGES
C V02  4-MAR-94 JWE Add Broadcast server code
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will load the GTECH Distributed Network
C common from the Station Configuraiton data files.
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
	SUBROUTINE X2LODSTN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
C
	INTEGER*4   STN,STNCNT              !Station/terminal pointers
	INTEGER*4   ST                      !Status
	INTEGER*4   STNATR                  !Station attributes
	INTEGER*4   GROUP
	INTEGER*4   LOCAL_PORT
	INTEGER*4   SAP_PORT
        INTEGER*4   ENACNT, I
        INTEGER*4   PVC_IDX,PVC_CKT         !Idx into circuits vector
	CHARACTER   X2FILNAM*20             !File name function
	INTEGER*4   PORT						    !V05
C
C OPEN THE STATION CONFIGURATION, STATION CLASS, AND
C NETWORK PORT FILES.
C
	CALL OPENLOD(X2FILNAM(XSTN),1)
	CALL OPENX2X(X2FILNAM(XNPC),4)
C
C LOOP THROUGH ALL STATIONS IN THE FILE.
C
	STN=0
	STNCNT=0
        ENACNT=0
100	CONTINUE
	  STN=STN+1
	  IF(STN.GT.X2X_STATIONS) GOTO 8000
	  CALL READLOD(1,STN,X2XSTN_REC,ST)
	  IF(ST.EQ.144) GOTO 8000
	  IF(X2XSTN_REC(1).LE.0) GOTO 100
          IF(X2XSTN_STNCLS.LE.0.OR.X2XSTN_STNCLS.GT.X2XC_CLASSES)
     *      GOTO 100
C
C CHECK THAT STATION CLASS, LOADED BY X2LODSCL, IS DEFINED	 
C
	  IF (X2XC_STATE(X2XSTN_STNCLS) .EQ.
     *			  CHAR(X2XC_UNDEFINED)) THEN	
	    TYPE *,'X2LODSTN:INVALID STATION CLASS : STATION - ',STN,
     *		   ' CLASS - ', X2XSTN_STNCLS
	    GOTO 100
	  ENDIF						  
C
C STORE THE Station INFORMATION INTO COMMON.
C
C
C IF ASYNC PVC CONNNECTION THE BUILD FE CONN ID NOW.
C
	  IF(X2XSTN_TYPE.EQ.X2XSCT_ASYPVC  .OR.
     *       X2XSTN_TYPE.EQ.X2XSCT_USAT_PVC) THEN
	     LOCAL_PORT = 0
             SAP_PORT   = 0
C
C	  ***** Start V05 changes *****
C
	     IF(X2XSTN_PVCPORT.GT.0 .AND.
     *          X2XSTN_PVCPORT.LE.X2X_NETWORK_PORTS) THEN
      	        LOCAL_PORT = X2XPN_NETWORK_TO_LOCAL(X2XSTN_PVCPORT)
	     ELSE
                TYPE*,'INVALID PVCPORT STATION:',STN,' port: ',
     *		  X2XSTN_PVCPORT
		TYPE *,'Station not built '
   	        GOTO 100
	     ENDIF
	     IF(LOCAL_PORT.GT.0.AND.LOCAL_PORT.LE.X2X_LOCAL_PORTS) THEN
               SAP_PORT = X2XPL_SAP_PORT(LOCAL_PORT) 
             ELSE
               TYPE*,'INVALID LOCAL PORT STATION: ',STN,' local port: ',
     *		  LOCAL_PORT
	       TYPE *,'Station not built '
               GOTO 100
             ENDIF
             X2XS_CONN_ID(STN)= IOR(ISHFT(SAP_PORT,24),
     *                               ISHFT(X2XSTN_PVC,8)) 
	  ENDIF
	  STNCNT=STNCNT+1
	  BX2XS_STATE(STN)=X2XSTN_STATE
C
          IF(X2XSTN_STATE.GE.X2XS_IDLE) ENACNT=ENACNT+1
C
	  BX2XS_STATION_DISCONNECT(STN)=X2XSTN_STNDIS
	  BX2XS_FE_DISCONNECT(STN)=X2XSTN_FEDIS
          X2XS_PVC(STN) = X2XSTN_PVC          
	  BX2XS_CONN_TYPE(STN)=X2XSTN_TYPE
          BX2XS_ORIG_TYPE(STN)   = X2XSTN_TYPE
C
C	  ***** End V05 changes *****
C
	  X2XS_ADRESS(1,STN)    = X2XSTN_ADDRES(1)
	  X2XS_ADRESS(2,STN)    = X2XSTN_ADDRES(2)
	  X2XS_ADRESS_LEN(STN)  = X2XSTN_ADDLEN
	  X2XS_PHYS(STN)        = X2XSTN_PVCPORT
          IF(X2XSTN_TYPE.EQ.X2XSCT_USAT_PVC) THEN
            PVC_IDX = X2XPN_PVC_INDEX(X2XSTN_PVCPORT)
            PVC_CKT= X2XS_PVC(STN)
            IF(PVC_IDX.EQ.0) THEN
              TYPE *,'X2LODSTN:PVC NET PORT NOT USAT TYPE, STN=',STN
            ELSEIF(PVC_CKT.GE.0.AND.PVC_CKT.LE.X2X_MAXPVC_CKTS)THEN
                 X2XPN_PVC_CKTS_TO_STN(PVC_CKT,PVC_IDX)=STN
            ELSE
              TYPE *,'X2LODSTN:PVC CIRUIT INVALID, STN=',STN
            ENDIF
          ENDIF
	  X2X_STN_PRINT(STN)=X2XSTN_PRTFLG
	  IF(X2XSTN_ERRREP.EQ.0)
     *	    CALL BSET(IX2XS_DOWNFLAG,(STN-1)*8+1)
	  IF(X2XSTN_DELACK.EQ.0)
     *	    CALL BSET(IX2XS_DOWNFLAG,(STN-1)*8+2)
	  STNATR=0
	  IF(X2XSTN_NETSTAT.EQ.0)
     *	    STNATR=IOR(STNATR,X2XSA_STATS)
	  IF(X2XSTN_AUTOUPD.EQ.0)
     *	    STNATR=IOR(STNATR,X2XSA_AUTO_STATS)
          BX2XS_ATRIBUTE(STN)=STNATR
C
C STORE OTHER STATION PARAMETERS
C
	  IF(X2XSTN_POLL.EQ.1) THEN                  !PORT NOPOLL FLAG
             CALL BSET(BX2XS_PARAM(STN),7)
	  ELSE
	     CALL BCLR(BX2XS_PARAM(STN),7)
	  ENDIF
C
	  X2XS_STNCLS(STN)=X2XSTN_STNCLS	    
          DO 200 I=1,X2XS_MAXNET
             X2XS_NETPORT(I,STN)=X2XSTN_NETPORT(I)
200       CONTINUE
C
          X2XS_X32PORT(1,STN)=X2XSTN_X32_PORT1
          X2XS_X32PORT(2,STN)=X2XSTN_X32_PORT2

	  DO 210, PORT=1,X2XS_MAXDEF					!V05
	    X2XS_DEF_PORT(PORT,STN)=X2XSTN_DEF_PORT(PORT)		!V05
210	  CONTINUE							!V05

          X2XS_VSP(STN) = X2XSTN_VSP
          X2XS_EVSN_LEN(STN)=X2XSTN_EVSN_LEN
          X2XS_EVSN(1,STN)=X2XSTN_EVSN(1)
          X2XS_EVSN(2,STN)=X2XSTN_EVSN(2)
          X2XS_DIAL_PORT(1,STN)=X2XSTN_DIAL_PORT1
          X2XS_DIAL_PORT(2,STN)=X2XSTN_DIAL_PORT2
          X2XS_TYPE(STN)=X2XSTN_STATION_TYPE				!V03
          CALL ISBYTE(X2XSTN_BCST_ENABLE,IX2XS_BCST_ENABLE,STN-1)	!V03
C
C
C STORE RELAY INFORMATION.
C
	  GROUP=X2XSTN_GROUP
	  IF(GROUP.NE.0) THEN
            IF(GROUP.GT.X2X_NUM_GROUPS) THEN
              TYPE *,'INVALID GROUP NUMBER FOR STATION .....',STN
            ELSE IF(X2XG_CNT(GROUP).GE.X2X_LIST_LEN) THEN                
              TYPE *,'EXCEEDED NUMBER OF STATIONS IN GROUP ',GROUP
              TYPE *,'STATION NOT STORED INTO GROUP........',STN
            ELSE
              X2XS_GROUP(STN)=GROUP                                
              X2XG_CNT(GROUP)=X2XG_CNT(GROUP)+1
              X2XG_LIST(X2XG_CNT(GROUP),GROUP)=STN
            ENDIF
	  ENDIF
C
C STORE THE NETWORK PORT (PVC PORT) TO STATION RELATION
C
C NOTE: FOR ASYNC PVC X2XSTN_PVC WILL ALWAYS BE ZERO
C
         IF(X2XSTN_PVCPORT.GT.0.AND.
     *       X2XSTN_PVCPORT.LE.X2X_NETWORK_PORTS)THEN
             IF(X2XSTN_TYPE.EQ.X2XSCT_ASYPVC) THEN
                IF(X2XSTN_PVC.NE.0) THEN
                 TYPE *,'X2LODSTN: INVALID NON-ZERO PVC CKT ,STN= ',STN
                  X2XSTN_PVC = 0
                ENDIF
                X2XPN_PVC_TO_STATION(X2XSTN_PVCPORT, X2XSTN_PVC)=STN
             ELSEIF(X2XSTN_TYPE.EQ.X2XSCT_USAT_PVC) THEN
C
C               USES: X2XPN_PVC_INDEX AND X2XPN_PVC_CKTS_TO_STN
C
                X2XPN_PVC_TO_STATION(X2XSTN_PVCPORT,0)=-1
             ELSE                                          ! OTHER: UNDEFINED
                X2XPN_PVC_TO_STATION(X2XSTN_PVCPORT,0)=-1
             ENDIF
          ENDIF
C
C STORE THE ADDRESS INTO THE SORT TABLE, TO BE SORTED LATER.
C
	  X2X_SORTED_ADR(0,STNCNT)=STN
	  X2X_SORTED_ADR(1,STNCNT)=X2XS_ADRESS(1,STN)
	  X2X_SORTED_ADR(2,STNCNT)=X2XS_ADRESS(2,STN)
C
C
C STORE THE EXT. VER. SEQ. NUMBER INTO THE SORT TABLE, TO BE SORTED LATER.
C
          X2X_SORTED_EVSN(0,STNCNT)=STN
          X2X_SORTED_EVSN(1,STNCNT)=X2XS_EVSN(1,STN)
          X2X_SORTED_EVSN(2,STNCNT)=X2XS_EVSN(2,STN)
C
C READ THE NEXT RECORD.
C
	  GOTO 100
C
C SORT THE X2X_SORTED_ADR ARRAY AND X2X_SORTED_EVSN ARRAY,
C CLOSE THE FILE AND RETURN.
C
8000	CONTINUE
	IF(STNCNT.GT.1) THEN
           CALL I4XSORT(X2X_SORTED_ADR, 3,STNCNT,2,3,0)
           CALL I4XSORT(X2X_SORTED_EVSN,3,STNCNT,2,3,0)
        ENDIF
C
	CALL CLOSLOD(1)
	CALL CLOSX2X(4)
C
        WRITE(5,*) IAM(),'Number of stations enabled: ',ENACNT
C
	RETURN
	END
