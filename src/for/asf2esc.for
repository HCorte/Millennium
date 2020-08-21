C
C ASF2ESC.FOR
C
C V02 28-MAR-2014 FRP Set disable status in XML file for offline terminals
C V00 04/06/2013 FJG INITIAL RELEASE FOR SCML
C
C GENERATE ESC.XML FILE FROM ASF
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
C Copyright 2013 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM  ASF2ESC
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'	
C	
        INTEGER*4    AGT
        INTEGER*4    ERR
        CHARACTER*22 XML
        INTEGER*4    DAT(3)
        INTEGER*4    CNT
C
        TYPE*,IAM()
	CALL COPYRITE
	TYPE*,IAM()
C-------GET DATE        
        CALL GDATE(DAT(2),DAT(1),DAT(3))
C-------OPEN ESC_PT_CDC.XML
        WRITE(XML,1000) DAYCDC
	TYPE*,IAM()
        TYPE*,IAM(),'Creating XML ',XML
        TYPE*,IAM()
        OPEN(UNIT=1,FILE=XML,STATUS='NEW',CARRIAGECONTROL='LIST',IOSTAT=ERR)
	IF (ERR.NE.0) THEN
	  TYPE*,IAM(),'Error opening ',XML,' Error: ',ERR
	ELSE
	  CNT=0
          WRITE(1,1100)	DAYCDC,DAT(1),DAT(2),2000+DAT(3)
          DO 100 AGT=1,NUMAGT
            IF(AGTTAB(AGTNUM,AGT).GT.0) THEN
              IF(TSBIT(AGTTAB(AGTTYP,AGT),AGTTON)) THEN  !ONLINE TERMINAL
                WRITE(1,1200) AGT,AGTTAB(AGTNUM,AGT)
              ELSE                                       !OFFLINE TERMINAL
                WRITE(1,1201) AGT,AGTTAB(AGTNUM,AGT)
              ENDIF
              CNT=CNT+1
            ENDIF
100       CONTINUE
          WRITE(1,1300)	CNT
	ENDIF
	CLOSE(UNIT=1)
C
        TYPE*,IAM()
        TYPE*,IAM(),'Finished'	
        TYPE*,IAM()        
C	
        CALL GSTOP(GEXIT_SUCCESS)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
1000    FORMAT('FILE:ESC_PT_',I6.6,'.XML')
1100    FORMAT('<?xml version="1.0" encoding="UTF-8" ?>',/,
     *         '<DOCUMENT>',/,
     *         '<EXPORT>',/,
     *         '<HEADER>',/,
     *         '<HSITE>PT</HSITE>',/,
     *         '<CDC>',I4,'</CDC>',/,
     *         '<DATE>',I2.2,'/',I2.2,'/',I4.4,'</DATE>',/,
     *         '<HOSTNAME>GOLS</HOSTNAME>',/,
     *         '</HEADER>')
1200    FORMAT('<RECORD>',/,
     *         '<TERMNUM>',I5,'</TERMNUM>',/,
     *         '<STATE>enabled</STATE>',/,
     *         '<CLIENTID>',I7.7,'</CLIENTID>',/,
     *         '<NETWORK_CLASS>adsl</NETWORK_CLASS>',/,
     *         '<ASSIGNED_CONFIG>GT1200</ASSIGNED_CONFIG>',/,
     *         '</RECORD>')
1201    FORMAT('<RECORD>',/,
     *         '<TERMNUM>',I5,'</TERMNUM>',/,
     *         '<STATE>disabled</STATE>',/,
     *         '<CLIENTID>',I7.7,'</CLIENTID>',/,
     *         '<NETWORK_CLASS>adsl</NETWORK_CLASS>',/,
     *         '<ASSIGNED_CONFIG>GT1200</ASSIGNED_CONFIG>',/,
     *         '</RECORD>')
1300    FORMAT('<TRAILER>',/,
     *         '<TSITE>PT</TSITE>',/,
     *         '<COUNT>',I5,'</COUNT>',/,
     *         '</TRAILER>',/,
     *         '</EXPORT>',/,
     *         '</DOCUMENT>')
	END
