del src\signmaker\lib\elm.js

move .\dist\*.js  src\signmaker\lib\elm.js
copy .\src\styles\*.css  src\signmaker\css\
copy .\src\images\*.*  src\signmaker\images\
copy .\src\chooserclassification.js  src\signmaker\chooserclassification.js


copy .\src\signmaker\lib\elm.js  "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\"
copy .\src\chooserclassification.js  "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\"
	                    
copy .\src\styles\*.css  "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\css\"
copy .\src\images\*.*  "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\images\"
copy .\src\chooserclassification.js  "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\chooserclassification.js"

pause