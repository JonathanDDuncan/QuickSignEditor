move .\dist\js\*.js  .\dist\js\elm.js
copy  .\dist\js\elm.js src\assets\lib\elm.js

xcopy /d /s /y .\src\assets  .\published\signmaker\assets
xcopy /d /s /y .\src\assets  "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\assets\" 

pause