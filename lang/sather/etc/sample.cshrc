# These items should appear, for instance in your .cshrc

  setenv SATHER_HOME /proj/hop/tools/sather
  setenv SATHER_MAKE pmake
  setenv SATHER_ENVI sun4          # or sun4kr, mips, mipskr, sgi, sgikr

  set path = ( $path $SATHER_HOME/bin.$SATHER_ENVI )
  setenv MANPATH $MANPATH":"$SATHER_HOME/man



