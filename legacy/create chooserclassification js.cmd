move "C:\Big Files\Downloads\chooserclassification.json" .


echo var chooserclassification = >> .\src\assets\lib\chooserclassification.js
copy /b .\src\assets\lib\chooserclassification.js+.\chooserclassification.json  .\src\assets\lib\chooserclassification.js

pause