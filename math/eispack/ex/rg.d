1                    EXPLANATION OF COLUMN ENTRIES FOR THE SUMMARY STATISTICS

 -----------------------------------------------------------------------------------------------
                          BALANCING               NO BALANCING          VECTORS COMPUTED
                  ------------------------ ------------------------ ------------------------
 ORDER LOW UPP T     HQR2   HQR     INVIT     HQR2   HQR     INVIT  
 -----------------------------------------------------------------------------------------------

 'BALANCING' IS THE OPTION THAT EMPLOYS SUBROUTINE  BALANC  TO BALANCE THE MATRIX BEFORE THE   
 EIGENVALUES ARE COMPUTED AND  BALBAK  TO BACK TRANSFORM THE EIGENVECTORS.                     

 'VECTORS COMPUTED' IDENTIFIES BY POSITION, IN THE SET RETURNED BY  HQR, THOSE EIGENVALUES     
 FOR WHICH  INVIT  COMPUTED THE ASSOCIATED EIGENVECTORS.  T  INDEXES AN EIGENVALUE FOR WHICH   
 THE EIGENVECTOR WAS COMPUTED AND  F  INDEXES A PASSED EIGENVALUE.

 UNDER 'ORDER' IS THE ORDER OF EACH TEST MATRIX.   

 UNDER 'LOW' AND 'UPP' ARE INTEGERS INDICATING THE BOUNDARY INDICES FOR THE BALANCED MATRIX.   

 UNDER 'T' IS THE LETTER E OR O INDICATING THE USE OF ELEMENTARY OR ORTHOGONAL TRANSFORMATIONS.

 UNDER 'HQR2   HQR' ARE TWO NUMBERS AND A KEYWORD.  THE FIRST NUMBER, AN INTEGER, IS THE       
 ABSOLUTE SUM OF THE ERROR FLAGS RETURNED SEPARATELY FROM  HQR2  AND  HQR.  THE SECOND         
 NUMBER IS THE MEASURE OF PERFORMANCE BASED UPON THE RESIDUAL COMPUTED FOR THE  HQR2  PATH.    
 THE KEYWORD REPORTS THE DUPLICATION OF THE COMPUTED EIGENVALUES FROM  HQR2  AND  HQR.         
 'SAME' MEANS THAT THE EIGENVALUES ARE EXACT DUPLICATES.  'DIFF' MEANS THAT FOR AT LEAST ONE   
 PAIR OF CORRESPONDING EIGENVALUES, THE MEMBERS OF THE PAIR ARE NOT IDENTICAL.                 

 UNDER 'INVIT' ARE TWO NUMBERS.  THE FIRST NUMBER, AN INTEGER, IS THE ABSOLUTE SUM OF THE
 ERROR FLAGS RETURNED FROM THE PATH.  THE SECOND NUMBER IS THE MEASURE OF PERFORMANCE BASED
 UPON THE RESIDUAL COMPUTED FOR THE PATH.

 -1.0  AS THE MEASURE OF PERFORMANCE IS PRINTED IF AN ERROR IN THE CORRESPONDING PATH HAS
 PREVENTED THE COMPUTATION OF THE EIGENVECTORS.

0FOR REDUCTIONS BY ELEMENTARY TRANSFORMATIONS
 THE  HQR2   PATH WITH    BALANCING USES THE EISPACK CODES  BALANC-ELMHES-ELTRAN-HQR2  -BALBAK,
 AS CALLED FROM DRIVER SUBROUTINE  RG.
 THE   HQR   PATH WITH    BALANCING USES THE EISPACK CODES  BALANC-ELMHES-HQR   ,              
 AS CALLED FROM DRIVER SUBROUTINE  RG.
 THE  INVIT  PATH WITH    BALANCING USES THE EISPACK CODES  BALANC-ELMHES-HQR   -INVIT -ELMBAK-BALBAK. 
 THE  HQR2   PATH WITH NO BALANCING USES THE EISPACK CODES  ELMHES-ELTRAN-HQR2  .              
 THE   HQR   PATH WITH NO BALANCING USES THE EISPACK CODES  ELMHES-HQR   .                     
 THE  INVIT  PATH WITH NO BALANCING USES THE EISPACK CODES  ELMHES-HQR   -INVIT -ELMBAK.       
0FOR REDUCTIONS BY ORTHOGONAL TRANSFORMATIONS
 THE  HQR2   PATH WITH    BALANCING USES THE EISPACK CODES  BALANC-ORTHES-ORTRAN-HQR2  -BALBAK.
 THE   HQR   PATH WITH    BALANCING USES THE EISPACK CODES  BALANC-ORTHES-HQR   .              
 THE  INVIT  PATH WITH    BALANCING USES THE EISPACK CODES  BALANC-ORTHES-HQR   -INVIT -ORTBAK-BALBAK. 
 THE  HQR2   PATH WITH NO BALANCING USES THE EISPACK CODES  ORTHES-ORTRAN-HQR2  .              
 THE   HQR   PATH WITH NO BALANCING USES THE EISPACK CODES  ORTHES-HQR   .                     
 THE  INVIT  PATH WITH NO BALANCING USES THE EISPACK CODES  ORTHES-HQR   -INVIT- ORTBAK.       
 D.P. VERSION 04/15/83
1       TABULATION OF THE ERROR FLAG  ERROR  AND THE MEASURE OF PERFORMANCE  Y  FOR 
     THE  EISPACK  CODES.  THIS RUN DISPLAYS THESE STATISTICS FOR REAL GENERAL MATRICES.   

                          BALANCING               NO BALANCING          VECTORS SELECTED
                  ------------------------ ------------------------ ------------------------
 ORDER LOW UPP T     HQR2   HQR     INVIT     HQR2   HQR     INVIT  
   8    1   8  E   0  .105 SAME   0  .590   0  .105 SAME   0  .590    tttttttt
               O   0  .054 SAME   0  .234   0  .054 SAME   0  .234
   6    1   6  E   0  .026 SAME   0  .012   0  .026 SAME   0  .012    tttttt
               O   0  .030 SAME   0  .095   0  .030 SAME   0  .095
   5    1   5  E   0  .019 SAME   0  .014   0  .019 SAME   0  .014    ttftf
               O   0  .012 SAME   0  .009   0  .012 SAME   0  .009
  12    1  12  E   0  .070 SAME   0  .023   0  .070 SAME   0  .023    tttttttttttt
               O   0  .015 SAME   0  .075   0  .015 SAME   0  .075
  10    1  10  E   0  .044 SAME   0  .027   0  .040 SAME   0  .011    tttttttttt
               O   0  .007 SAME   0  .006   0  .022 SAME   0  .006
  15    1  15  E   0  .344 SAME   0  .078   0  .046 SAME   0  .312    ttttttttttttttt
               O   0  .089 SAME   0  .990   0  .037 SAME   0  .203
  19    1  19  E   0  .028 SAME   0  .067   0  .028 SAME   0  .067    tftftfttttttttttttt
               O   0  .023 SAME   0  .029   0  .023 SAME   0  .029
   6    1   1  E   0  .000 SAME   0  .083   0  .000 SAME   0  .083    tttttt
               O   0  .000 SAME   0  .083   0  .000 SAME   0  .083
   6    1   6  E   0  .009 SAME   0  .012   0  .009 SAME   0  .012    ttftft
               O   0  .019 SAME   0  .010   0  .019 SAME   0  .010
   6    1   6  E   0  .010 SAME   0  .005   0  .010 SAME   0  .005    tttttt
               O   0  .010 SAME   0  .004   0  .010 SAME   0  .004
   5    1   5  E   0  .062 SAME   0  .153   0  .062 SAME   0  .153    tfttf
               O   0  .061 SAME   0  .068   0  .061 SAME   0  .068
   5    1   5  E   0  .089 SAME   0  .282   0  .089 SAME   0  .282    ttftf
               O   0  .089 SAME   0  .289   0  .089 SAME   0  .289
   5    1   5  E   0  .032 SAME   0  .109   0  .032 SAME   0  .109    ttftf
               O   0  .032 SAME   0  .105   0  .032 SAME   0  .105
   5    1   5  E   0  .033 SAME   0  .084   0  .033 SAME   0  .084    tfttf
               O   0  .033 SAME   0  .108   0  .033 SAME   0  .108
   5    1   5  E   0  .072 SAME   0  .154   0  .072 SAME   0  .154    ttftf
               O   0  .072 SAME   0  .211   0  .072 SAME   0  .211
   3    1   1  E   0  .000 SAME   0  .000   0  .000 SAME   0  .060    ttt
               O   0  .000 SAME   0  .000   0  .000 SAME   0  .060
1       TABULATION OF THE ERROR FLAG  ERROR  AND THE MEASURE OF PERFORMANCE  Y  FOR 
     THE  EISPACK  CODES.  THIS RUN DISPLAYS THESE STATISTICS FOR REAL GENERAL MATRICES.   

                          BALANCING               NO BALANCING          VECTORS SELECTED
                  ------------------------ ------------------------ ------------------------
 ORDER LOW UPP T     HQR2   HQR     INVIT     HQR2   HQR     INVIT  
   3    1   1  E   0  .000 SAME   0  .000   0  .000 SAME   0  .072    ttt
               O   0  .000 SAME   0  .000   0  .000 SAME   0  .072
   3    1   1  E   0  .003 SAME   0  .000   0  .003 SAME   0  .067    ttt
               O   0  .003 SAME   0  .000   0  .003 SAME   0  .067
   4    1   4  E   0  .042 SAME   0  .008   0  .011 SAME   0  .013    tttf
               O   0  .042 SAME   0  .015   0  .011 SAME   0  .011
   3    1   3  E   0  .119 SAME   0  .167   0  .119 SAME   0  .167    tft
               O   0  .119 SAME   0  .116   0  .119 SAME   0  .116
   4    1   4  E   0  .070 SAME   0  .275   0  .070 SAME   0  .275    ttft
               O   0  .070 SAME   0  .200   0  .070 SAME   0  .200
   5    1   5  E   0  .138 SAME   0  .742   0  .138 SAME   0  .742    tftft
               O   0  .138 SAME   0  .868   0  .138 SAME   0  .868
   6    1   6  E   0  .122 SAME   0 1.318   0  .122 SAME   0 1.318    ttftft
               O   0  .122 SAME   0  .836   0  .122 SAME   0  .836
   8    1   8  E   0  .029 SAME   0  .003   0  .025 SAME   0  .001    ttftftft
               O   0  .029 SAME   0  .003   0  .025 SAME   0  .001
   4    1   4  E   0  .173 SAME   0  .381   0  .173 SAME   0  .381    tttf
               O   0  .173 SAME   0  .585   0  .173 SAME   0  .585
   3    2   3  E   0  .001 SAME   0  .056   0  .001 SAME   0  .056    ttt
               O   0  .001 SAME   0  .056   0  .001 SAME   0  .056
   4    1   4  E   0  .115 SAME   0  .031   0  .115 SAME   0  .031    tftf
               O   0  .115 SAME   0  .023   0  .115 SAME   0  .023
   4    1   4  E   0  .078 SAME   0  .107   0  .078 SAME   0  .107    tttt
               O   0  .028 SAME   0  .091   0  .028 SAME   0  .091
   6    2   6  E   0  .011 SAME   0  .019   0  .002 SAME   0  .126    tttttt
               O   0  .013 SAME   0  .016   0  .004 SAME   0  .005
   6    1   6  E   0  .013 SAME   0  .015   0  .013 SAME   0  .015    tftttt
               O   0  .008 SAME   0  .008   0  .008 SAME   0  .008
   8    1   8  E   0  .076 SAME   0  .080   0  .076 SAME   0  .080    tftftftf
               O   0  .021 SAME   0  .031   0  .021 SAME   0  .031
   4    1   4  E   0  .024 SAME   0  .025   0  .024 SAME   0  .025    ttft
               O   0  .028 SAME   0  .033   0  .028 SAME   0  .033
1       TABULATION OF THE ERROR FLAG  ERROR  AND THE MEASURE OF PERFORMANCE  Y  FOR 
     THE  EISPACK  CODES.  THIS RUN DISPLAYS THESE STATISTICS FOR REAL GENERAL MATRICES.   

                          BALANCING               NO BALANCING          VECTORS SELECTED
                  ------------------------ ------------------------ ------------------------
 ORDER LOW UPP T     HQR2   HQR     INVIT     HQR2   HQR     INVIT  
   6    1   6  E   0  .014 SAME   0  .017   0  .014 SAME   0  .017    tftftf
               O   0  .042 SAME   0  .025   0  .042 SAME   0  .025
   8    7   8  E   0  .000 SAME   0  .000   0  .016 SAME   0  .049    tttttttt
               O   0  .000 SAME   0  .000   0  .005 SAME   0  .007
  10    1  10  E   0  .000 SAME   0  .002   0  .000 SAME   0  .001    tttttttftf
               O   0  .014 SAME   0  .009   0  .010 SAME   0  .002
0END OF DATA FOR SUBROUTINE  RMATIN(RGREADI). 
1
