C UPDVPF.FOR
C
C V03 18-AUG-2011 FJG EVO Project bug with PASRETAFTAMT solved
C     24-AUG-2011 FJG Incorrect counting
C V02 01-JAN-2010 FJG ePassive: Complete rewrite
C V01 26-APR-2001 ANG Initial release for Portugal. 
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C PROGRAM TO UPDATE PRIZES STATUS ON VPF'S FILES TO VCXL
C 
C=======OPTIONS /check=nooverflow/ext
	program updvpf
        implicit none
!
        include 'inclib:sysextrn.def'
        include 'inclib:sysdefine.def'
!
        include 'inclib:global.def'
        include 'inclib:concom.def'
        include 'inclib:pascom.def'
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def' 	
        include 'inclib:desval.def'        
        include 'inclib:valpasfil.def'
        include 'inclib:prmhsh.def'        
        include 'inclib:dparec.def'                
!
        integer*4    vlun
        parameter    (vlun = 9)
        integer*4    flun
        parameter    (flun = 1)        
!        
        integer*4    erro
        integer*4    gind
        integer*4    emis
        integer*4    mnum
        integer*4    mser
        integer*4    mfra
        integer*4    terr
        character*11 ckey   
        integer*4    tamt    
        integer*4    tcom 
        integer*4    ttik
        integer*4    draw
        integer*4    temp
        integer*4    gnum
        integer*4    ffdb(7)
!
        integer*4    nrec
        integer*4    nbuk
        integer*4    obuk
        integer*4    vkey(2)
        integer*4    vbuf(I4BUCSIZ)   
        integer*4    rcnt       
        integer*4    ramt
!
        record /stpasfdb/  pasfdb        
        record /stpasrec/  pasrec        
        record /stpasrec/  pasbuk(pasbukrec) !32 of stpasrec            
C===============================================================================
C       Start process 
C===============================================================================
        call copyrite
        type*,iam()
        type*,iam(),'<<<<<<<<<<< UPDVPF - Returned After Draw Update >>>>>>>>>>>'
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
C       Main Loop 
C===============================================================================        
        do gind=1,numpas
          do emis=1,pagemi
            if(pasretaftamt(emis,gind).gt.0) then ! V03 This means counters
              rcnt = 0
              ramt = 0
              draw = pasemis(emis,gind)
              gnum = gtntab(TPAS,gind)
              call pasio_init(pasfdb,gind,pasemis(emis,gind),pasnumtck(emis,gind)-1,pasnumser(emis,gind),
     *                        pasnoffra(emis,gind),cpastpffil(emis,gind))     
              call pasio_openro(pasfdb)  
              if(pasfdb.err.ne.ioe_noerr) then
                type*,iam(),'Error: ',pasfdb.err,' opening file: ',pasfdb.filnam
                call pasio_dump(pasfdb)
                cycle              
              else 
                call iopen(pasvpffil(1,emis,gind),vlun,VPFLEN*2,VFSCDC,VFSSER*2-1,erro)
                if(erro.ne.0) then
                  type*,iam(),'Error: ',erro,' opening file: ',cpasvpffil(emis,gind)
                  call pasio_close(pasfdb) 
                  cycle    
                else
                  type*,iam(),'Scanning file: ',pasfdb.filnam                 
                  terr = 0
                  obuk = 0
                  ttik = 0
!=================Calculate the base price of the ticket - comission============
                  tamt = pasprc(emis,gind)
                  tcom = idnint(dfloat(tamt)*calper(retcom(gind)))
                  tamt = tamt - tcom
!===============================================================================
                  do mnum = 0,pasnumtck(emis,gind)-1
                    do mser = 1,pasnumser(emis,gind)
                      do mfra = 1,pasnoffra(emis,gind)
!===============================================================================
!                       This is copied from PASIOSUBS to avoid the sucesive reads
!                       Please keep in mind that should be the same               
!===============================================================================       
                        nrec = (mnum+1-1)*pasfdb.maxser*pasfdb.maxfra !step 1: pasnum-1+1 because starts at zero
                        nrec = nrec + ((mser-1)*pasfdb.maxfra)        !step 2: the previous sers * # of fras
                        nrec = nrec + mfra                            !step 3: add the searched fraction
                        nbuk = ((nrec - 1)/pasbukrec)+1               !step 4: calculate bucket record
                        nrec = nrec-((nbuk-1)*pasbukrec)              !step 5: offset within the bucket                      
!===============================================================================                      
                        if(nbuk.ne.obuk) then
                          call readw(pasfdb.fdb,nbuk,pasbuk,pasfdb.err)
                          obuk = nbuk
                        else
                          pasfdb.err = ioe_noerr 
                        endif
                        pasrec = pasbuk(nrec)
                        if(pasfdb.err.eq.ioe_noerr.and.pasrec.key.ne.pasio_key(mnum,mser,mfra)) pasfdb.err = ioe_keyerr
!                      
                        if(pasfdb.err.ne.ioe_noerr) then
                          write(ckey,900) mnum,mser,mfra                        
                          type*,iam(),'Error: ',pasfdb.err,' reading record: ',ckey
                          terr = terr + 1
                          if(terr.ge.20) then
                            terr = 0
                            call gpause
                            type*,iam(),'Skipping file: ',pasfdb.filnam
                            goto 10
                          endif
                        else
!+++++++++++++++++++++++FRACTION FOUND++++++++++++++++++++++++++++++++++++++++++
                          if(pasrec.stat.eq.pbilkof.and.pasrec.cdc.eq.daycdc) then
                            vkey(1) = mfra
                            vkey(2) = ishft(mser,24) + mnum                            
                            call iread(vkey,v4buf_pas,vlun,erro)
                            if(erro.ne.0) then
                              type*,iam(),'Error: ',erro,' reading VPF record'
                            else
	                      call logpas(valrec,v4buf_pas)                      
	                      if(valrec(VSTAT).eq.VUNCSH.or.VALREC(VSTAT).eq.VPRPAY) then
	                        rcnt = rcnt + 1               ! V03
	                        ramt = ramt + valrec(vpamt)   ! V03
                                valrec(VSTAT) = VNOPAY
                                call paslog(valrec,v4buf_pas)
                                call iwrite(V4BUF_PAS,vlun,erro)
                                if(erro.ne.0) then
                                  type*,iam(),'Error: ',erro,' writing VPF record'
                                else
                                  ttik = ttik + 1
                                endif
                              endif
                            endif
                          endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                        endif
                      enddo
                    enddo
                  enddo
                  call iclose(vlun,vbuf,erro)
                  tamt = tamt * ttik
                  type*,iam()
                  type*,iam(),'Draw: ',draw,' SCML:    ',pasdraw(emis,gind)
                  type*,iam()
                  type*,iam(),'Found:      ',rcnt,' (',CMONY(ramt,11,VALUNIT),')'
                  type*,iam(),'Updated:    ',ttik                  
                  type*,iam()          
                  if(rcnt.ne.pasretaftamt(emis,gind)) then
                    type*,iam()
                    type*,iam(),'---------------------------------'
                    type*,iam(),'WARNING: Counter data discrepancy'
                    type*,iam(),'Tickets Registered: ',pasretaftamt(emis,gind)
                    type*,iam(),'Tickets Processed:  ',rcnt                  
                    type*,iam(),'---------------------------------'
                    type*,iam()                              
                  endif
                  pasretaftamt(emis,gind) = ramt  ! V03 CRITICAL. We TRANSFORM counters in amount prior to CSHPAS
!                 For future it will be better having two fields: one for counters other for amount
!                 and have the information in both. This will add a control: if counter > 0 and amt = 0, UPDVPF not run
                endif
10              continue                
                call pasio_close(pasfdb)   
              endif
!=============UPDATE PASSIVE GAME FILE V3========================================
              write(5,910) iam(),(gfnames(temp,gnum),temp=1,5),draw
              call openw(flun,gfnames(1,gnum),4,0,0,erro)
              call ioinit(ffdb,flun,dpasec*256)       
              if(erro.ne.0) then
                type*,iam(),'Error: ',erro,' opening file'                 
              else
                call readw(ffdb,draw,dparec,erro)
                if(erro.ne.0) then
                  type*,iam(),'Error: ',erro,' reading file'                                   
                else
                  DPARETAFTAMT = DPARETAFTAMT + ramt
                  call writew(ffdb,draw,dparec,erro)
                  if(erro.ne.0) then
                    type*,iam(),'Error: ',erro,' writing file'                                                     
                  else
                  endif
                endif
                call closefil(ffdb)
              endif
              type*,iam()
!===============================================================================
            endif
          enddo
        enddo
!
        call gstop(gexit_success)
!===============================================================================
!       Formats
!===============================================================================
900     format(I5.5,'s',I2.2,'f',I2.2)
910     format(1X,A,'Updating game file ',5A4,' Draw ',I5)
        end
