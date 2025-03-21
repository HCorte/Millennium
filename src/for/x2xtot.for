C
C SUBROUTINE X2XTOT.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2XTOT.FOV                                   $
C  $Date::   17 Apr 1996 16:45:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  GXSRC:X2XTOT.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Rearranged AGTINF.DEF, VISCOM.DEF for Finland.
C
C V01 13-DEC-94 GPR RELEASED FOR UK
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
        SUBROUTINE X2XTOT(SUBNET)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:X2VIS.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
C
	INTEGER*4   SUBNET
C
	INTEGER*4   MAX_INTERVAL,MAX_VALUES
	PARAMETER   (MAX_INTERVAL=6,
     *		     MAX_VALUES=5)
	INTEGER*4   MAX_INACT_INTERVAL
	PARAMETER   (MAX_INACT_INTERVAL=7)
	INTEGER*4   COUNT_TERMS(MAX_INTERVAL,MAX_VALUES)
	INTEGER*4   COUNT_TERMS_INACTIVE(MAX_INACT_INTERVAL)
	INTEGER*4   I,J,END
	CHARACTER*30	DESCR(MAX_VALUES)
	DATA	DESCR /'Stns with errorcount         ',
     *		       'Stns with resetcount         ',
     *		       'Stns with no. of disconnects ',
     *		       'Terms with delays (sec)       ',
     *                 'Broadcast server changes      '/
	INTEGER*4   ERR_INDX,
     *		    RESET_INDX,
     *		    DISC_INDX,
     *		    DELAY_INDX,
     *		    BCST_CHANGES_INDX
	PARAMETER   (ERR_INDX=1,
     *		     RESET_INDX=2,
     *		     DISC_INDX=3,
     *		     DELAY_INDX=4,
     *		     BCST_CHANGES_INDX=MAX_VALUES)
	INTEGER*4 TERM,STN,CLASS,STN_SUBNET
	INTEGER*4 TIME_INACTIVE
C
	INTEGER*4   LEN_ARRAY
	PARAMETER   (LEN_ARRAY=25)
	INTEGER*4 ERRCOUNTARRAY(2,LEN_ARRAY)
C
	INTEGER*4   BEG_SCREEN
	PARAMETER   (BEG_SCREEN=5)
	INTEGER*4   LAST_ERROR

	INTEGER*4   DROPPED_SEND
	INTEGER*4   DROPPED_RECEIVE

	INTEGER*4   FIRST_SUBNETWORK, LAST_SUBNETWORK
	INTEGER*2   VALUE
C
C INITIALIZE ARRAYS
C
	CALL FASTSET(0,COUNT_TERMS,MAX_INTERVAL*MAX_VALUES)
	CALL FASTSET(0,COUNT_TERMS_INACTIVE,MAX_INACT_INTERVAL)
C
        DO I=1,LEN_ARRAY
            ERRCOUNTARRAY(1,I) = -1
        END DO
C
C SCAN DATA FOR ALL STATIONS
C
	DO 100, STN=1,X2X_STATIONS
      		CLASS = X2XS_STNCLS(STN)
	        IF (CLASS.EQ.0) GOTO 100
      		STN_SUBNET = X2XC_SUBNETWORK(CLASS)
		IF (STN_SUBNET.EQ.SUBNET .OR. SUBNET.EQ.255) THEN
C
		    CALL ERROR_COUNTS(X2XS_LAST_ERR_CODE(STN),
     *			  ERRCOUNTARRAY)
C
C
		    CALL INCREMENT_COUNTER(X2XS_ERR_CNT(STN),
     *		      COUNT_TERMS(1,ERR_INDX))
		    CALL INCREMENT_COUNTER(X2XS_RESET_CNT(STN),
     *		      COUNT_TERMS(1,RESET_INDX))
		    CALL INCREMENT_COUNTER(X2XS_DISC_CNT(STN),
     *		      COUNT_TERMS(1,DISC_INDX))
		    CALL INCREMENT_COUNTER(X2XS_BCST_STATUS_CHANGES(STN),
     *		      COUNT_TERMS(1,BCST_CHANGES_INDX))
		ENDIF
100	CONTINUE
C
C	LOOP THROUGH ALL TERMINALS
C
	DO 200, TERM=1,X2X_TERMS
	    IF (X2XT_TIME(TERM).LE.0) GOTO 200
	    STN= X2XT_STATION_NO(TERM)
	    IF (STN.NE.0) THEN
      		CLASS = X2XS_STNCLS(STN)
      		IF (CLASS.NE.0) STN_SUBNET = X2XC_SUBNETWORK(CLASS)
		IF (STN_SUBNET.EQ.SUBNET .OR. SUBNET.EQ.255) THEN
C
		      TIME_INACTIVE=P(ACTTIM)-X2XT_TIME(TERM)
		      IF (P(ACTTIM).LT.X2XT_TIME(TERM)) THEN
			TIME_INACTIVE=24*3600+P(ACTTIM)-X2XT_TIME(TERM)
		      ENDIF
		      IF (TIME_INACTIVE.GE.0 .AND. 
     *		        TIME_INACTIVE.LE.10) THEN
     		       COUNT_TERMS_INACTIVE(1) = COUNT_TERMS_INACTIVE(1)+1
C
		      
		      ELSEIF (TIME_INACTIVE.GT.10 .AND. 
     *		        TIME_INACTIVE.LE.60) THEN
     		       COUNT_TERMS_INACTIVE(2) = COUNT_TERMS_INACTIVE(2)+1
C
		      ELSEIF (TIME_INACTIVE.GT.60 .AND. 
     *		        TIME_INACTIVE.LE.10*60) THEN
     		       COUNT_TERMS_INACTIVE(3) = COUNT_TERMS_INACTIVE(3)+1
C
		      ELSEIF (TIME_INACTIVE.GT.10*60 .AND. 
     *		        TIME_INACTIVE.LE.60*60) THEN
     		       COUNT_TERMS_INACTIVE(4) = COUNT_TERMS_INACTIVE(4)+1
C
		      ELSEIF (TIME_INACTIVE.GT.3600 .AND. 
     *		        TIME_INACTIVE.LE.3600*2) THEN
      		       COUNT_TERMS_INACTIVE(5) = COUNT_TERMS_INACTIVE(5)+1
C
		      ELSEIF (TIME_INACTIVE.GT.3600*2 .AND. 
     *		        TIME_INACTIVE.LE.3600*5) THEN
     		       COUNT_TERMS_INACTIVE(6) = COUNT_TERMS_INACTIVE(6)+1
C
		      ELSEIF (TIME_INACTIVE.GT.3600*5) THEN
     		         COUNT_TERMS_INACTIVE(7) = COUNT_TERMS_INACTIVE(7)+1
		      ENDIF
C
	              VALUE = X2XT_NETWORK_DELAY(TERM)/1000
     		      CALL INCREMENT_COUNTER(VALUE,COUNT_TERMS(1,DELAY_INDX))
C
		ENDIF
	    ENDIF	    
200	CONTINUE
C
	DO I=1,LEN_ARRAY
	    IF (ERRCOUNTARRAY(1,I).EQ.-1) THEN
		LAST_ERROR = I-1
		GOTO 2500
	    ENDIF
	END DO
	LAST_ERROR = LEN_ARRAY
2500	CONTINUE
C
	IF (LAST_ERROR .GT. 1) THEN
		CALL I4XSORT(ERRCOUNTARRAY,2,LAST_ERROR,2,0,0)
	ENDIF
C
C	GET NO OF MESAGES DISGARDED

	FIRST_SUBNETWORK=0
	LAST_SUBNETWORK=X2X_MAX_SUBNETWORK
	IF (SUBNET.NE.255) THEN
	    FIRST_SUBNETWORK=SUBNET
	    LAST_SUBNETWORK=SUBNET
	ENDIF

	DROPPED_SEND=0
	DROPPED_RECEIVE=0

	DO 2600, I=FIRST_SUBNETWORK, LAST_SUBNETWORK
	    DROPPED_SEND=DROPPED_SEND+X2XSN_TOTAL_FLUSHED_SEND(I)
	    DROPPED_RECEIVE=DROPPED_RECEIVE+X2XSN_TOTAL_FLUSHED(I)
2600	CONTINUE
C	
	WRITE (CLIN1,9070)
	WRITE (CLIN4,9000)
	WRITE (CLIN3,9003) ' dropped messages: send ',DROPPED_SEND,
     *			    ' receive ',DROPPED_RECEIVE
	WRITE (CLIN5,9010)
C
	DO I=1,MAX_VALUES
	    WRITE (XNEW(BEG_SCREEN+I),9020) DESCR(I),
     *	      (COUNT_TERMS(J,I),J=1,MAX_INTERVAL)
	END DO
C
	WRITE (XNEW(BEG_SCREEN+MAX_VALUES+2),9100)
	WRITE (XNEW(BEG_SCREEN+MAX_VALUES+3),9110)
C
	DO I=1,1
	    WRITE (XNEW(BEG_SCREEN+MAX_VALUES+I+3),9120) 
     *	      '# Terms inactive since ',
     *	      (COUNT_TERMS_INACTIVE(J),J=1,MAX_INACT_INTERVAL)
	END DO
C
	WRITE (XNEW(BEG_SCREEN+MAX_VALUES+6),9150) 
C
	DO I=1,LEN_ARRAY/5
	  IF (ERRCOUNTARRAY(1,(I-1)*5+1).NE.-1) THEN
	     DO J=1,5
		IF (ERRCOUNTARRAY(1,(I-1)*5+J).EQ.-1) THEN
		    END = J-1
		    GOTO 2000
		ENDIF
	     END DO
	     END = 5
2000	     WRITE (XNEW(BEG_SCREEN+MAX_VALUES+I+6),9140) 
     *	    ((ERRCOUNTARRAY(1,(I-1)*5+J),ERRCOUNTARRAY(2,(I-1)*5+J))
     *		  ,J=1,END)
	     IF (I.EQ.LEN_ARRAY/5 .AND. J.EQ.5) THEN
	         WRITE (XNEW(BEG_SCREEN+MAX_VALUES+I+6),9160)
	     ENDIF
	  ENDIF
	END DO
C
	RETURN

9000	FORMAT('|',30X,'|   0   |   1   |   2   |  3-5  |  6-10 |  10+  |')
9003	FORMAT(A,I6,A,I6)
9010	FORMAT('+',30('-'),<MAX_INTERVAL>('+',7('-')),'+')
9020	FORMAT('|',A30,<MAX_INTERVAL>('|',I7),'|')
C
9100	FORMAT('|',22X,'| 0-10 s|11-60 s| 1-10 m|11-60 m| 1-2 h | 3-5 h |',
     *	  '  5+ h |')
9110	FORMAT('+',22('-'),<MAX_INACT_INTERVAL>('+',7('-')),'+')
9120	FORMAT('|',A22,<MAX_INACT_INTERVAL>('|',I7),'|')
9140	FORMAT(5(Z8.8,':',I5,2X))
9150	FORMAT('Last Errors Encountered <error no.: # of terms.> :')
9160	FORMAT(T63,'OTHERS:  ')
9070	FORMAT('Totals for Stns')
C
	END
C
C
C**************** INCREMENT_COUNTER () ***********************
C
C
	SUBROUTINE INCREMENT_COUNTER(VALUE,UPDATE_ARRAY)

        IMPLICIT NONE
C
	INTEGER*4   MAX_INTERVAL,MAX_VALUES
	PARAMETER   (MAX_INTERVAL=6,
     *		     MAX_VALUES=5)

	INTEGER*2   VALUE
	INTEGER*4   UPDATE_ARRAY(MAX_INTERVAL)

	IF (VALUE.EQ.0) THEN
	    UPDATE_ARRAY(1) = UPDATE_ARRAY(1)+1
	ELSEIF (VALUE.EQ.1) THEN
	    UPDATE_ARRAY(2) = UPDATE_ARRAY(2)+1
	ELSEIF (VALUE.EQ.2) THEN
	    UPDATE_ARRAY(3) = UPDATE_ARRAY(3)+1
	ELSEIF (VALUE.GE.3.AND.VALUE.LE.5) THEN
	    UPDATE_ARRAY(4) = UPDATE_ARRAY(4)+1
	ELSEIF (VALUE.GE.6.AND.VALUE.LE.10) THEN
	    UPDATE_ARRAY(5) = UPDATE_ARRAY(5)+1
	ELSEIF (VALUE.GT.10) THEN
	    UPDATE_ARRAY(MAX_INTERVAL) = UPDATE_ARRAY(MAX_INTERVAL)+1
	ENDIF
C
	RETURN
C
	END

C
C**************** ERROR_COUNTS () ***********************
C
        SUBROUTINE ERROR_COUNTS(ERRNR,ERRCOUNTARRAY)
        IMPLICIT NONE

        INTEGER*4   ERRNR
	INTEGER*4   LEN_ARRAY
	PARAMETER   (LEN_ARRAY=25)
        INTEGER*4   ERRCOUNTARRAY(2,LEN_ARRAY)  !array counting number of
                                                !occurrences of each occured
                                                !error
        INTEGER*4 I
C
        DO I=1,LEN_ARRAY-1
            IF (ERRCOUNTARRAY(1,I).EQ.ERRNR) THEN
              ERRCOUNTARRAY(2,I) = ERRCOUNTARRAY(2,I)+1
              RETURN
            ENDIF
            IF (ERRCOUNTARRAY(1,I).EQ.-1) THEN
                ERRCOUNTARRAY(1,I) = ERRNR
                ERRCOUNTARRAY(2,I) = 1
                RETURN
            ENDIF
        END DO
C
C STORE OTHER ERRORS IN LAST ELEMENT
C THIS CODE WILL BE ENTERED ONLY IF THE ERROR WASN'T FOUND IN THE OTHER
C ELEMENTS
C

	IF (ERRCOUNTARRAY(1,LEN_ARRAY) .EQ. -1) THEN
	    ERRCOUNTARRAY(1,LEN_ARRAY) = 0
	    ERRCOUNTARRAY(2,LEN_ARRAY) = 0
	ENDIF
C
	ERRCOUNTARRAY(2,LEN_ARRAY) = ERRCOUNTARRAY(2,LEN_ARRAY) + 1
C
        RETURN
C
        END
