-bash-4.1$ PKG_CONFIG_PATH=$PWD LD_LIBRARY_PATH=/home/AMED/michael.panciera/tmp/lib/python2.7:/home/AMED/michael.panciera/anaconda/include:$LD_LIBRARY_PATH:/home/AMED/michael.panciera/tmp/include/python2.7/ TMPDIR=$HOME/tmp stack build  --extra-lib-dirs=/home/AMED/michael.panciera/tmp/lib/ --extra-include-dirs=/home/AMED/michael.panciera/include/ 

-bash-4.1$ PKG_CONFIG_PATH=$PWD LD_LIBRARY_PATH=/home/AMED/michael.panciera/tmp/lib/python2.7:/home/AMED/michael.panciera/anaconda/include:$LD_LIBRARY_PATH:/home/AMED/michael.panciera/tmp/include/python2.7/ TMPDIR=$HOME/tmp stack build --extra-include-dirs=/home/AMED/michael.panciera/tmp/include/-bash-4.1$ PKG_CONFIG_PATH=$PWD LD_LIBRARY_PATH=/home/AMED/michael.panciera/tmp/lib/python2.7:/home/AMED/michael.panciera/anaconda/include:$LD_LIBRARY_PATH:/home/AMED/michael.panciera/tmp/include/python2.7/ TMPDIR=$HOME/tmp stack build  --extra-lib-dirs=/home/AMED/michael.panciera/tmp/lib/ --extra-include-dirs=/home/AMED/michael.panciera/include/


had to add python.pc file:

```
prefix=/usr
exec_prefix=${prefix}
libdir=/home/AMED/michael.panciera/tmp/lib
includedir=/home/AMED/michael.panciera/tmp/include

Name: Python
Description: Python library
Requires: 
Version: 2.7
Libs.private: -lpthread -ldl  -lutil
Libs: -L${libdir} -lpython2.7
Cflags: -I${includedir}/python2.7 
```
