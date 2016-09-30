xcopy .\dist\*.*  published\ /s /y
del src\signmaker\lib\elm.js

move .\dist\*.js  src\signmaker\lib\elm.js
copy .\src\styles\*.css  src\signmaker\css\

copy src\signmaker\lib\elm.js "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\lib\"
copy .\src\styles\*.css  "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\css\"

copy src\signmaker\lib\elm.js published\signmaker\lib\elm.js
copy src\signmaker\css\*.* published\signmaker\css\
pause