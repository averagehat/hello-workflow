
# hello-workflow


### Frameworks to consider:
* bpipe
* shake
* CWL
* altered Drake
* pydoit


* treat _R[12]_ files differently;
  *  group paired read files

* handle dynamically missing output

* handle input with unknown names

* merge output into single file

* branch based on config file
* easily get command args (inc. list) form config file
* change command based on properties of a file
* log easily
* represent changes which do not create a new file (tagreads)
* handle multiple outputs/inputs
* hopefullly abstract over common needs (bam needed lots)
* some sff -> fastq (calling python)



###Shake
stack setup
stack build
stack exec shell-exe
