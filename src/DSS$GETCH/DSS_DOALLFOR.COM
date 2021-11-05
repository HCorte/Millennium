$!
$! v01 13-feb-95 wxm initial release
$!
$ if "''p1'" .eqs. "" then p1 = "ACTIVE"
$ if "''p1'" .eqs. "*"      then goto START
$ if "''p1'" .eqs. "ACTIVE" then goto START
$ if "''p1'" .eqs. "LIVE"   then goto START
$ if "''p1'" .eqs. "TOOL"   then goto START
$ if "''p1'" .eqs. "JUNK"   then goto START
$ if "''p1'" .eqs. "UPDATE" then goto START
$ write sys$output "Invalid group parameter."
$ goto END
$ START:
$ if f$search("gxsrc:allfor.lis") .nes. "" then del/nolog gxsrc:allfor.lis;*
$ if "''p1'" .eqs. "*"
$ then
$  domain = "library"
$  cms sho element/format="#E"/out=gxsrc:allfor.lis *.FOR
$ else
$  domain = "''p1' group"
$  cms sho element/format="#E"/out=gxsrc:allfor.lis 'p1'_code
$ endif
$ if f$search("gxsrc:allfor.lis") .eqs "" then goto NONE
$ open/read/error=erropin infil gxsrc:allfor.lis
$ if f$search("gxsrc:allfor.com") .nes. "" then del/nolog gxsrc:allfor.com;*
$ open/write/error=erropout outfil gxsrc:allfor.com
$ write/err=errwrout outfil "$set noon"
$ write/err=errwrout outfil "$set ver"
$ cnt = 0
$ NEXT:
$  read/end=endin/err=errrdin infil file
$  if f$parse("''file'",,,"TYPE") .nes. ".FOR" then goto NEXT
$  name = f$parse("''file'",,,"NAME")
$  vax=f$extract(0,3,platform).eqs."VAX"
$  algn=""
$  if .not.vax
$  then
$    algn="/ALIGN=(COMMON=(PACK,MULTILANGUAGE),RECORD=PACK)"
$  endif
$  write/err=errwrout outfil "$lotfort''algn' ''name'"
$  cnt = cnt+1
$ goto NEXT
$ ERRRDIN:
$ write sys$output "Error reading temporary file GXSRC:ALLFOR.LIS"
$ close infil
$ close outfil
$ goto END
$ ERRWROUT:
$ write sys$output "Error writing output file GXSRC:ALLFOR.COM"
$ close infil
$ close outfil
$ goto END
$ ERROPIN:
$ write sys$output "Error opening temporary file GXSRC:ALLFOR.LIS"
$ goto END
$ ERROPOUT:
$ write sys$output "Error opening output file GXSRC:ALLFOR.COM"
$ close infil
$ goto END
$ ENDIN:
$ write/err=errwrout outfil "$set nover"
$ close infil
$ close outfil
$ del/nolog gxsrc:allfor.lis;*
$ if cnt .eq. 0
$ then
$  write sys$output "No .FOR files found in ''p1' group"
$  del/nolog gxsrc:allfor.com;*
$ endif
$ goto END
$ NONE:
$ write sys$output "No .FOR files found in ''p1' group"
$ END:
$ exit