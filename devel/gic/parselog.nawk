BEGIN {LM=-1000}
$0 ~ /----------------------------/ {LM=0; printf("\n");}
$0 ~ /\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=\=/ {LM=0; printf("\n");}
{if (LM == 1) printf("%s ", $0);
 if (LM >= 3) printf("%s ", $0);
 LM++;}
