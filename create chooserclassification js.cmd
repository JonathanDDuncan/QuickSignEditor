move "C:\Big Files\Downloads\chooserclassification.json" .\src
cd src
del chooserclassification.js
echo var chooserclassification = >> chooserclassification.js
copy /b chooserclassification.js+chooserclassification.json  chooserclassification.js

pause