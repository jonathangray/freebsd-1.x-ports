BEGIN {
  RS="ASKMF_OUTPUT" ;
  FS="\n" ;
  OFS="\n" ;
}
#{ print }
/ starts here/ { 
  for (i=2;i<=NF;i++) print($i) ;
}
