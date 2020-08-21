C
C X2SETPOLL.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SETPOLL.FOV                                $
C  $Date::   17 Apr 1996 16:34:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C X2SETDISABLE.FTN 
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C V01 11/03/90, WS RELEASED FOR SWEDEN                                          
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
      SUBROUTINE X2SETPOLL(SUBNETWORK,FIRST_STATION
     *						  ,LAST_STATION)
      IMPLICIT NONE                                                             
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INTEGER*4   SUBNETWORK  !-1 IF ALL SUBNETWORKS

	INTEGER*4 OFF, STATE, FIRST_STATION, LAST_STATION
	INTEGER*4 CLASS
	INTEGER*4 TEMP, CONF	
*                                                                               
	DO 10, OFF=FIRST_STATION,LAST_STATION
	    CLASS=X2XS_STNCLS(OFF)
	    IF (CLASS.LE.0) GOTO 10
	    IF (X2XC_SUBNETWORK(CLASS).NE.SUBNETWORK .AND.
     *		SUBNETWORK.GE.0)	      GOTO 10
	    CALL ILBYTE(STATE,IX2XS_STATE,OFF-1)                                   
	    IF (STATE.EQ.0) GOTO 10                                                
C                                                                               
C UPDATE STATION CONFIGURATION                                                  
C                                                                               
	    CALL ILBYTE(TEMP, IX2XS_CONF, OFF - 1)                             
	    CONF=ISHFT(IAND(TEMP,'000000E0'X),-5)                                        
	    CONF=CONF+1                                                            
	    IF(MOD(CONF,8).EQ.0) CONF=0                                            
	    CONF=ISHFT(CONF,5)                                                     
	    TEMP=IAND(TEMP,'0000001F'X)+CONF                                             
	    CALL ISBYTE(TEMP, IX2XS_CONF, OFF - 1)                             
C                                                                               
	    CALL ISBYTE(0, IX2XS_PARAM, OFF - 1)                            

10	CONTINUE                                                                  
*                                                                               
	RETURN                                                                                
      END                                                                       
