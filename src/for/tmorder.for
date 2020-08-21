C TMORDER.FOR
C
C V19 12-NOV-1999 RXK World Tour coupons added
C V18 31-AUG-1999 UXN Moniveto coupons removed.
C V17 21-MAY-1999 UXN Empty line added.
C V16 31-MAR-1999 UXN VAKIO REDUCED SYSTEM COUPON AND JOKERI COUPON ADDED.
C V15 23-FEB-1998 UXN Super Score and Todays Triple added.(Ässä,Casino and
C                     Special instant tickets removed, printer ribbon field 
C                     replaced with TSSC/TTRP betslips)
C V14 13-NOV-1997 UXN TMFREP.DEF added.
C V13 27-FEB-1996 RXK Layout fixed
C V12 24-JAN-1996 RXK Correction of formats and fix of order in line of totals
C V11 15-DEC-1995 PXB Changes for double and couple games
C V10 29-APR-1994 JXP COPY=0
C V09 17-NOV-1993 SXH Uncommented CONCOM.DEF
C V08 08-OCT-1993 HXK Totals weren't added for some reason.
C V07 04-JUN-1993 HHN INITIAL RELEASE FOR VAX FINLAND
C V06 09-MAR-1993 HJK Fix for Ravi lists being omitted, 
C                     set Las Vegas coupons to 0.
C V05 06-MAR-1993 JWE Simplify the calculation of totals a bit
C V04 07-JAN-1993 HJK CHANGED FOR VIKING LOTTO #*#
C V03 14-OCT-1992 HJK CHANGED FOR SPEDEN GAME
C V02 17-OCT-1990 MTK CHANGED FOR KENO GAME
C V01 19-FEB-1990 MGM INITIAL RELEASE FOR FINLAND
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
CTITLE Copyright 1999 GTECH Corporation. All rights reserved.                   
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      SUBROUTINE TMORDER
      IMPLICIT NONE

	INCLUDE '(LIB$ROUTINES)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TMFREP.DEF'

C Input parameters
C ----------------
      INTEGER*4   LUN
C                                            
      INTEGER*4   STATUS,COPY,INDX,PAGE,LINCNT,AGT,I
      INTEGER*4   OFF,ST
      INTEGER*4   IND
      INTEGER*4   I4TEMP
      INTEGER*2   I2TEMP(2)

      INTEGER*4   SORT(10,NUMAGT)                    
      INTEGER*2   I2SORT(20,NUMAGT)                  
      INTEGER*4   TOTORDS(20)                                                   
      LOGICAL     FIRST/.TRUE./
      LOGICAL     WARN/.FALSE./                                                 
      DATA LINCNT/0/,INDX/0/                                                    
      EQUIVALENCE(I2SORT,SORT)
      EQUIVALENCE(I4TEMP,I2TEMP)
      INTEGER*4   K
C
      IF(FIRST)THEN                                                             
      	 TYPE *
      	 TYPE *,IAM(),'<<<<< TMORDER >>>>>'
      	 TYPE *
C      	 CALL PRMNUM('Enter number of copies',COPY,0,20,EXT)
C      	 IF(EXT.LT.0) RETURN
         COPY=0

      	 FIRST=.FALSE.
      	 INDX = 0
      	 CALL FASTSET(0,SORT,10*NUMAGT)
C
C OPEN THE REPORT FILE.
C
	 ST = LIB$GET_LUN(ST)
	 IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
	 CALL ROPEN('TMORDER.REP',LUN,ST)
	 IF(ST.NE.0) THEN
	   TYPE*,IAM(),'Error opening TMORDER.REP >',ST
	   CALL GSTOP(GEXIT_FATAL)
	 ENDIF
C
C        PRINT REPORT HEADING
C        --------------------
      	 CALL TITLE ('AGENT SUPPLY ORDERS',' TMORDER',
     *                1,LUN,PAGE,DAYCDC)
      	 WRITE(LUN,9000)
      	 LINCNT = 7
      ENDIF

C     ====================== Main Processing ========================           

      IF(EOF) GOTO 1000                                                         

      IF(INDX.EQ.NUMAGT) THEN                                                   
        IF(.NOT.WARN) TYPE*,IAM(),' Detail order table is full'
        WARN=.TRUE.                                                             
        RETURN                                                                  
      ENDIF                                                                     


      INDX = INDX + 1                                                           
      SORT(1,INDX)   = TRABUF(TAGT)                                             
      SORT(2,INDX)   = TRABUF(TTER)                                             
      DO OFF = TSDT1,TSDT15
         I4TEMP = TRABUF(OFF)
	 IND = (OFF - TSDT1) + 5
	 I2SORT(IND,INDX) = I2TEMP(1)
C***     CALL FASTMOV(TRABUF(TSDT1),SORT(3,INDX),14)                      
      END DO

      RETURN                                                                    

C     ==================== End of Main Processing ===================           

1000  CONTINUE                                                                  
      IF(INDX.LE.0) GOTO 200

C SORT BY AGENT NUMBER                                                          
C --------------------                     
      CALL ISORTA(SORT,INDX,1)                                                  

      DO 100 AGT=1,INDX                                                         
         WRITE(LUN,90011) SORT(1,AGT)/10,MOD(SORT(1,AGT),10), SORT(2,AGT),
     *                (I2SORT(K,AGT), K=5, 18)


         DO I=1,15
            TOTORDS(I)=TOTORDS(I)+I2SORT(I+4,AGT)
         ENDDO

         LINCNT = LINCNT + 1                                                    
         IF(LINCNT.GT.25)THEN                                                   
            CALL TITLE ('AGENT SUPPLY ORDERS',' TMORDER',               
     *                   1,LUN,PAGE,DAYCDC)                                   
            WRITE(LUN,9000)                                                   
            LINCNT = 7                                                          
         ENDIF                                                                  
100   CONTINUE                                                                  

      WRITE(LUN,9002) (TOTORDS(I),I=1,14)

200   CONTINUE
      CLOSE(LUN)
      ST = LIB$FREE_LUN(LUN)	
      CALL SPOOL('TMORDER.REP',COPY,STATUS)                                     

C     =================== Format Statements ====================                
9000  FORMAT(/,1X,'ASIAM',3X,'PÄÄTE',
     *       2X,'TOSITE',2X,'LOTTO',3X,'VAKIO',3X,
     *       'RAVI',4X,'TULOS',3X,'SPEDE ',2X, 
     *       'VIKING',2X,'PITKÄ',3X,'VOITT.',2X,'YMP.',4X
     *       'VAKIO',3X,'JOKERI',2X,
     *       'TILITYS-',1X,'VOITTO-',                   
     *       /,16X,'PAPERI',2X,'KUPONG',2X,'KUPONG',2X,
     *       'KUPONG',2X,'KUPONG',2X,'KUPONG',2X,'KUPONG',2X,                   
     *       'KUPONG',2X,'KUPONG',2X,'KUPONG',2X,
     *       'JÄRJES.',1X,'KUPONG',2X,
     *       'KUORET',3X,'TILIK.'
     *       /,1X,131('='))                                                     
90011 FORMAT(1X,I6.6,'-',I1.1,1X,I4,1X,14(I7,1X),/)
9002  FORMAT(//,'YHTEENSÄ:',6X,14(I7,1X)) 
      RETURN                                                                    
      END                                                                       
