#!/bin/bash
echo "starting transfer file process"
#logica de ftp mais tarde até ser por ssh...
#echo -n "Qual é o nome do ficheiro? "
#read ficheiro
cd ~/Documents/olimpo-millennium/src/for
repeat=true
while [[ "$repeat"=true ]] 
do
  current=`pwd`
  echo -e  "Correct directory '${current}' \e[90m[y/n]\e[0m? "
  echo -e  'write \e[92m"for"\e[0m to go to source code dir'
  echo -e  'write \e[92m"lnk"\e[0m to go to linkage files dir'
  echo -e  'write \e[92m"def"\e[0m to go to def type files dir '
  echo -e  'write \e[92m"c"\e[0m to go to source code of c type files dir'
  read choice

  case $choice in
    y)
        echo 'correct directory...'
        pwd
    ;;

    for)
       cd ~/Documents/olimpo-millennium/src/for
       pwd
    ;;

    lnk)
       cd ~/Documents/olimpo-millennium/src/lnk
       pwd
    ;; 

    def)
       cd ~/Documents/olimpo-millennium/src/def
       pwd
    ;;

    c)
       cd ~/Documents/olimpo-millennium/src/c
       pwd
    ;;        
  esac 

  read -p 'File Name: ' ficheiro
  #echo ficheiro escolhido \"$ficheiro\"

  fileSearch=`ls $ficheiro 2> /tmp/Error`
  ERROR=$(</tmp/Error)
  exists=${#fileSearch}

#  echo "fileSearch:"$fileSearch
#  echo "fileSearch length:"${#fileSearch}
#  echo "ERROR:"$ERROR
#  echo "ERROR length:"${#ERROR}

  #if grep -q "{search_word}" <<< "$fileexists"
  if [[ $exists > 0 ]]
  then
    echo -e '\e[92mficheiro existe\e[0m'
    pwd
ftp -n 10.100.82.101 <<END_SCRIPT
    quote USER "console"
    quote PASS "console"
    pwd
    cd dkd10:[DMIL.WRK.HMC]
    pwd
    ascii
    put $ficheiro
    quit
END_SCRIPT
  else
    echo -e '\e[31mficheiro não existe\e[0m'
  fi

  read -p $'Want more files to be transfer \e[90m[y/n]\e[0m: ' YN
#  echo  ${YN^^}
  [[ ${YN^^} = "N" ]] && break

done

echo "transfer completed"
exit 0
