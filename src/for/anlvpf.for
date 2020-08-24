C ANLVPF.FOR
C
C V02 18-OCT-2010 FJG Add VNOPAY in report (RETAFTDRW)
C V01 01-JAN-2010 FJG ePassive
C
C Program to analyze the VPF files
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the propercntty of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferrorsed from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferrorsed,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /check=nooverflow/ext
        program anlvpf
        implicit none
!
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
!
        include 'inclib:global.def'
        include 'inclib:concom.def'
        include 'inclib:pascom.def'        
        include 'inclib:desval.def'
	include 'inclib:vdetail.def'        
        include 'inclib:prmvpf.def'
        include 'inclib:hshcom.def'
!
        integer*4    recmax
        integer*4    lunrep
        integer*4    lunfil
!
        parameter   (recmax=i4bucsiz/vpflen)
        parameter   (lunrep = 7)
        parameter   (lunfil = 1)
!                        
        integer*4    bufbig(i4bucsiz)
        integer*4    bufone(vpflen,recmax)
        equivalence (bufbig,bufone)
!
        integer*4    gamnum
        integer*4    gamind
        integer*4    gamemi                
        integer*4    fdbfil(7)
        integer*4    varwk1
        integer*4    varwk2
        integer*4    dummie
        integer*4    reppag
        integer*4    errors
        integer*4    blocks
        integer*4    totals
        integer*4    counts
        integer*4    cntdum
        integer*4    numrec
        integer*4    mulrec
        integer*4    mulcnt(1:4)        
        integer*4    nulrec
        integer*4    recbuk(0:recmax)
        integer*4    statab(3,numtot,9,pagemi,numpas)
        integer*4    staind
        integer*4    stasta     
        integer*4    i4mkey   
        integer*4    hsherr
        integer*4    totbuc         
        real*8       percnt
        real*8       pertot
        character*8  chrgam    
C===============================================================================
C       Start process 
C===============================================================================
        call copyrite
        type*,iam()
        type*,iam(),'<<<<< ANLVPF Validation Passive File Analysis   >>>>>'
        type*,iam()
!        
        if (daysts.eq.dsopen) then
          type*,iam()
          type*,iam(),'************************* WARNING *************************'
          type*,iam(),'*                                                         *'          
          type*,iam(),'* THIS PROGRAM SHOULD NOT BE RUN DURING ONLINE PROCESSING *'
          type*,iam(),'*     IF YOU CONTINUE YOU WILL DO IT BY YOUR OWN RISK     *'          
          type*,iam(),'*                                                         *'                    
          type*,iam(),'***********************************************************'          
          type*,iam()
          call gpause        
        endif        
C===============================================================================
C       Open report
C===============================================================================
        call ropen('ANLVPFST.REP',lunrep,errors)
        if(errors.ne.0) then
          type*,iam(),' cannot open report file ',errors
          call gstop(gexit_fatal)
        endif
        reppag = 0    
        call fastset(0,statab,3*numtot*9*pagemi*numpas)        
C===============================================================================
C       INI MAIN LOOPS
C===============================================================================                
        do gamind=1,numpas
          do gamemi=1,pagemi   
            if(passubsts(gamemi,gamind).eq.pdrwpur.or.
     *         passubsts(gamemi,gamind).eq.pdrwval) then
              if(filexist(cpasvpffil(gamemi,gamind))) then
                dummie = 0
                totals = 0
                blocks = 0
                percnt = 0
                pertot = 0
                cntdum = 0
                call fastset(0,recbuk,recmax)
                call fastset(0,mulcnt,4)          
                call title(cpasvpffil(gamemi,gamind) // ' FILE ANALYSIS','ANLVPF',1,lunrep,reppag,daycdc) 
                write(lunrep,9000)
                write(lunrep,9001)
!             
                call openw(lunfil,pasvpffil(1,gamemi,gamind),0,0,0,errors)
                if(errors.ne.0) then
                  call filerr(pasvpffil(1,gamemi,gamind),1,errors,0)
                  call gstop(gexit_fatal)
                endif
                call ioinit(fdbfil,lunfil,bucsec*256)   
                call getsiz_used(lunfil,totbuc)                     
                totbuc = totbuc / BUCSEC       
                hsherr = 0     
                numrec = 0                
C===============================================================================
C       Loop blocks
C===============================================================================
!=============> INI LOOP
                do while (.true.)
                  blocks = blocks + 1
                  call readw(fdbfil,blocks,bufbig,errors)
                  if(errors.eq.144) exit
                  if(errors.ne.0) then
                    call filerr(pasvpffil(1,gamemi,gamind),2,errors,blocks)
                    call gstop(gexit_fatal)
                  endif
!
                  mulrec = 0
                  counts = 0
                  nulrec = 0
!===============> INI LOOP
                  do varwk1 = 1,recmax
                    if(mulrec.gt.1) then
                      mulrec = mulrec - 1
                    else
                      if(bufone(1,varwk1).eq.0) then
                        nulrec = varwk1
                      else
                        if(nulrec.ne.0) then
                          write(lunrep,9007) blocks,nulrec
                          nulrec = 0
                        endif
!
                        i4mkey = iand(bufone(1,varwk1),INDMSK)
                        i4mkey = mod(i4mkey,totbuc) + 1
                        if(i4mkey.ne.blocks) hsherr = hsherr + 1
!                        
                        mulrec = ishft(bufone(1,varwk1),-30) + 1
                        mulcnt(mulrec) = mulcnt(mulrec) + 1
                        counts = counts + mulrec
                        numrec = numrec + 1
                        call logpas(valrec,bufone(1,varwk1))
                        call dlogpas(valrec,vdetail)
                        if(valrec(vstat).eq.vuncsh) then
                          staind = 1
                        else if(valrec(vstat).eq.vprpay) then
                          staind = 2                               
                        else if(valrec(vstat).eq.vcash) then
                          staind = 3                          
                        else if(valrec(vstat).eq.vdel) then
                          staind = 4                                                    
                        else if(valrec(vstat).eq.vcxl) then
                          staind = 5                                                                              
                        else if(valrec(vstat).eq.vbank) then
                          staind = 6
                        else if(valrec(vstat).eq.vnopay) then
                          staind = 7                          
                        else
                          staind = 8                            
                        end if
!                       Per status
                        statab(3,tracnt,staind,gamemi,gamind) = 
     *                  statab(3,tracnt,staind,gamemi,gamind) + 1
                        statab(3,dolamt,staind,gamemi,gamind) = 
     *                  statab(3,dolamt,staind,gamemi,gamind) + valrec(vpamt)
!                       Per Total
                        statab(3,tracnt,9,gamemi,gamind) = 
     *                  statab(3,tracnt,9,gamemi,gamind) + 1
                        statab(3,dolamt,9,gamemi,gamind) = 
     *                  statab(3,dolamt,9,gamemi,gamind) + valrec(vpamt)     
!     
                        if(valrec(vpastyp).eq.vpasoff) then
                          stasta = 1
                        else
                          stasta = 2                          
                          if(valrec(vscdc).le.0) then
                            if(staind.eq.1.or.staind.eq.2) then
                              write(lunrep,9008) valrec(vtckt),valrec(vsern),valrec(vpfrac),
     *                                           valrec(vster),valrec(vscdc),valrec(vsser),valrec(vstat)
                              cntdum = cntdum + 1
                            endif
                          endif                            
                        end if    
                        statab(stasta,tracnt,staind,gamemi,gamind) = 
     *                  statab(stasta,tracnt,staind,gamemi,gamind) + 1
                        statab(stasta,dolamt,staind,gamemi,gamind) = 
     *                  statab(stasta,dolamt,staind,gamemi,gamind) + valrec(vpamt)
!                       Per Total
                        statab(stasta,tracnt,9,gamemi,gamind) = 
     *                  statab(stasta,tracnt,9,gamemi,gamind) + 1
                        statab(stasta,dolamt,9,gamemi,gamind) = 
     *                  statab(stasta,dolamt,9,gamemi,gamind) + valrec(vpamt)                                
                    endif
                    endif
                  enddo   
!===============> END LOOP             
                  recbuk(counts) = recbuk(counts) + 1
                  totals = totals + counts
  	        enddo
!=============> FIN LOOP  	        
                call closefil(fdbfil)
C===============================================================================
C               End of file
C===============================================================================                
  	        blocks = blocks - 1
                type*,iam(),cpasvpffil(gamemi,gamind),blocks,' Blocks processed ...'  	        
                if(hsherr.gt.0) then
                  type*,iam(),'***** HASHING KEY ERRORS FOUND:    ',hsherr,' *****'
                  type*,iam(),'***** COPVPF COULD BE DONE ',cpasvpffil(gamemi,gamind),' *****'
                  write(lunrep,9010) 'Error hash keys integrity: RECORDS READ ',blocks
                  write(lunrep,9010) 'Error hash keys integrity: MAXBUC CALCD ',totbuc
                  write(lunrep,9010) 'Incorrect records found:                ',hsherr                  
                endif
                if(cntdum.gt.0) then
                  type*,iam(),'***** DUMMY RECORDS FOUND IN FILE: ',cntdum,' *****'                  
                endif
                type*,iam()
                write(lunrep,9002)
!=============> INI LOOP                
                do varwk1 = 0,recmax
                  varwk2 = recbuk(varwk1)
                  percnt = dfloat(varwk2) / dfloat(blocks) * 100.0D0
  	          pertot = pertot + percnt          
                  if(varwk2.gt.0) write(lunrep,9003) varwk1,varwk2,percnt       
                enddo
!=============> FIN LOOP                                
                write(lunrep,9009) recmax,pertot      
  	        varwk2 = blocks * recmax
  	        percnt = dfloat(totals)/dfloat(varwk2) * 100.0D0
  	        write(lunrep,9004) totals,blocks,percnt
  	        write(lunrep,9005) (varwk1,mulcnt(varwk1),mulcnt(varwk1)*varwk1,varwk1=1,4)
  	        write(lunrep,9011) numrec
                write(lunrep,9006) dummie
              else
                type*,iam(),'FILE NOT FOUND: Skipping: ',cpasvpffil(gamemi,gamind)
                type*,iam()
              endif
            endif
          enddo
        enddo
        close(lunrep)
C===============================================================================
C       FIN MAIN LOOPS
C===============================================================================        
        reppag=0  
        call ropen('ANLVPF.REP',lunrep,errors)
        if(errors.ne.0) then
          type*,iam(),' cannot open report file ',errors
          call gstop(gexit_fatal)
        endif
!     
        do gamind=1,numpas
          gamnum = gtntab(tpas,gamind)
          write(chrgam,'(A3,1X,A4)') 'VPF',gsnames(gamnum)	   
          call title('ANALYSIS OF VPF',chrgam,1,lunrep,reppag,daycdc)
          write(lunrep,9100) 
          write(lunrep,9110)           
          do gamemi=1,pagemi
            if(passubsts(gamemi,gamind).eq.pdrwpur.or.
     *         passubsts(gamemi,gamind).eq.pdrwval) then            
              write(lunrep,9130) gsnames(gamnum),pasemis(gamemi,gamind),pasdraw(gamemi,gamind),
     *                          (statab(3,tracnt,varwk1,gamemi,gamind),varwk1=1,9)
              write(lunrep,9140) cpasvpffil(gamemi,gamind),
     *                           (cmony(statab(3,dolamt,varwk1,gamemi,gamind),12,valunit),varwk1=1,9)  
              write(lunrep,9150) 'OFFLINE >>',(statab(1,tracnt,varwk1,gamemi,gamind),varwk1=1,9)
              write(lunrep,9160) (cmony(statab(1,dolamt,varwk1,gamemi,gamind),12,valunit),varwk1=1,9)  
              write(lunrep,9150) 'ONLINE >>>',(statab(2,tracnt,varwk1,gamemi,gamind),varwk1=1,9)
              write(lunrep,9160) (cmony(statab(2,dolamt,varwk1,gamemi,gamind),12,valunit),varwk1=1,9)         
              write(lunrep,9110)                
            endif
          enddo
        enddo
        close(lunrep)        
!
	call gstop(gexit_success)
C===============================================================================
C       Format statements
C===============================================================================
9000    format(1X,131('='))
9001    format(///,X,30('*'),' Abnormalities ',30('*'),/)
9002    format(///,X,30('*'),' Block Usage ',30('*'),/)
9003    format(X,'Total Buckets with ',I4,' Records = ',I6,X,F6.2,'%')
9004    format(/,X,'Total Records ',I10,' IN ',I8,' Blocks: 'F6.2,'% of file is used')
9005    format(//,4(X,'# of Records using ',I2,' Slots: ',I12,' Records: ',I12,/))
9006    format(' There are ',I4,' DUMMY Records',/)
9007    format(X,'HOLE IN BLOCK ',I8,' Record: ',I4)
9008    format(X,'Ticket ',I5.5,' S',I2.2,' F',I2.2,' has dummy values STER:',I5,' SCDC: ',I4,' SSER:',I9,' STAT: ',I2)
9009    format(X,'Max Records/Bucket ',I4,'                  ',F6.2,'%')
9010    format(X,'Error: ',A40,I10)
9011    format(X,'# of Validation records:     ',I12,/)
9100    format(/,1X,'TIPO DRAW EMISIO       UNCASHED    UNCSHPRV     CASHED     DELETED    CANCELLED '
     *              ' BANK CASHED  WINRETAFT     OTHERS      TOTALS   ')
9110    format(' ==== ==== ======',4X,9(' ==========='))
9130    format(1X,A4,X,I4,X,I6.6,4X,9(I12))   
9140    format(1X,A20,9(A12),/)   
9150    format(1X,6X,A10,4X,9(I12))   
9160    format(1X,20X,9(A12))   
	end
