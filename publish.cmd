del .\published\*.css
del .\published\*.js
xcopy .\dist\*.*  published /s /y

del src\signmaker\lib\elm.js

move .\dist\*.js  src\signmaker\lib\elm.js
copy .\src\styles\*.css  src\signmaker\css\
copy .\src\styles\*.css  published\styles\

copy src\signmaker\lib\elm.js "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\lib\"
copy .\src\styles\*.css  "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\css\"

copy .\src\sw10.js  published\ /y
copy .\src\sw10.min.js  published\ /y
copy .\src\chooserclassification.js published\ /y
copy .\src\symbolsize.js  published\ /y

copy .\src\sw10.js  published\signmaker\ /y
copy .\src\sw10.min.js  published\signmaker\ /y
copy .\src\chooserclassification.js published\signmaker\ /y
copy .\src\symbolsize.js  published\signmaker\ /y

xcopy .\src\signmaker\*.*  published\signmaker /s /y
rem Please commit any changes to the publish folder
pause
git branch -dr origin/gh-pages
git subtree push --prefix published origin gh-pages

pause